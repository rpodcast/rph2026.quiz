#' Postgres connection parameters from environment variables
#'
#' Reads `PG_HOST`, `PG_PORT`, `PG_DATABASE`, `PG_USER`, `PG_PASSWORD` and
#' (optionally) `PG_SSL_MODE`. Locally these come from `.Renviron`; on the
#' deployment platform they must be set as environment variables / secrets.
#'
#' @return A named list of arguments for `RPostgres::Postgres()` connections.
#'
#' @noRd
db_pg_params <- function() {
  vars <- c(
    host = "PG_HOST",
    port = "PG_PORT",
    dbname = "PG_DATABASE",
    user = "PG_USER",
    password = "PG_PASSWORD"
  )
  values <- Sys.getenv(vars, unset = "")
  missing <- vars[!nzchar(values)]
  if (length(missing)) {
    stop(
      "Missing Postgres environment variable(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  params <- as.list(stats::setNames(values, names(vars)))
  # Don't echo the value: a URI here usually embeds the password
  if (grepl("://|@|/", params$host)) {
    stop(
      "PG_HOST must be a bare host name, not a connection URI.",
      call. = FALSE
    )
  }
  params$port <- as.integer(params$port)
  if (is.na(params$port)) {
    stop("PG_PORT must be an integer.", call. = FALSE)
  }
  params$sslmode <- normalize_sslmode(Sys.getenv("PG_SSL_MODE", "require"))
  # libpq waits forever by default, e.g. when a firewall drops packets
  params$connect_timeout <- "10"
  params
}

#' Map an SSL mode setting to a libpq `sslmode`
#'
#' Accepts any libpq mode, plus on/off style aliases such as `"enabled"`.
#' Managed databases (e.g. Linode) require SSL, so empty means `"require"`.
#'
#' @param x SSL mode string.
#'
#' @noRd
normalize_sslmode <- function(x) {
  x <- tolower(trimws(x))
  libpq <- c(
    "disable",
    "allow",
    "prefer",
    "require",
    "verify-ca",
    "verify-full"
  )
  if (x %in% libpq) {
    return(x)
  }
  if (x %in% c("", "enabled", "enable", "true", "on", "yes")) {
    return("require")
  }
  if (x %in% c("disabled", "false", "off", "no")) {
    return("disable")
  }
  stop(
    "PG_SSL_MODE must be one of: ",
    paste(libpq, collapse = ", "),
    call. = FALSE
  )
}

#' Connect to the quiz database
#'
#' SQLite is used in development (file under `persistent/`, which Ricochet
#' preserves across deployments). Postgres is used in production; connection
#' details come from `db_pg_params()` (`PG_*` environment variables).
#'
#' @param backend `"sqlite"` or `"postgres"`.
#' @param sqlite_path Path to the SQLite file (sqlite backend only).
#' @param use_pool Use a `{pool}` connection pool (the app default) or a single
#'   DBI connection. Tests use a single connection because pool schedules
#'   recurring `{later}` tasks, which keep `shiny::testServer()` waiting forever
#'   when it resolves outputs.
#' @param pg_params Postgres connection arguments (postgres backend only).
#'
#' @return A list with `pool` (a pool or DBI connection) and `backend`.
#'
#' @importFrom pool dbPool
#' @importFrom DBI dbConnect dbExecute dbGetQuery
#' @noRd
db_connect <- function(
  backend = c("sqlite", "postgres"),
  sqlite_path = "persistent/quiz.sqlite3",
  use_pool = TRUE,
  pg_params = if (identical(backend, "postgres")) db_pg_params()
) {
  backend <- match.arg(backend)
  drv <- switch(
    backend,
    sqlite = RSQLite::SQLite(),
    postgres = RPostgres::Postgres()
  )
  args <- list()
  on_create <- function(con) invisible(con)
  if (identical(backend, "postgres")) {
    # COUNT(*) is bigint; return plain integers rather than integer64
    args <- c(pg_params, list(bigint = "integer"))
  }
  if (identical(backend, "sqlite")) {
    dir.create(dirname(sqlite_path), showWarnings = FALSE, recursive = TRUE)
    args$dbname <- sqlite_path
    on_create <- function(con) {
      DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL")
      DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
      invisible(con)
    }
  }
  con <- if (use_pool) {
    do.call(pool::dbPool, c(list(drv), args, list(onCreate = on_create)))
  } else {
    on_create(do.call(DBI::dbConnect, c(list(drv), args)))
  }
  list(pool = con, backend = backend)
}

#' Close the quiz database connection
#'
#' @param db A database handle from `db_connect()`.
#'
#' @importFrom pool poolClose
#' @importFrom DBI dbDisconnect
#' @noRd
db_close <- function(db) {
  if (inherits(db$pool, "Pool")) {
    pool::poolClose(db$pool)
  } else {
    DBI::dbDisconnect(db$pool)
  }
}

#' Translate `?` placeholders for the active backend
#'
#' @param db A database handle.
#' @param sql SQL with `?` placeholders.
#'
#' @noRd
db_sql <- function(db, sql) {
  if (!identical(db$backend, "postgres")) {
    return(sql)
  }
  i <- 0L
  while (grepl("?", sql, fixed = TRUE)) {
    i <- i + 1L
    sql <- sub("?", paste0("$", i), sql, fixed = TRUE)
  }
  sql
}

#' Execute / query helpers
#'
#' @param db A database handle.
#' @param sql SQL with `?` placeholders.
#' @param params List of parameters.
#'
#' @noRd
db_execute <- function(db, sql, params = list()) {
  if (length(params)) {
    DBI::dbExecute(db$pool, db_sql(db, sql), params = unname(params))
  } else {
    DBI::dbExecute(db$pool, sql)
  }
}

#' @rdname db_execute
#' @noRd
db_query <- function(db, sql, params = list()) {
  if (length(params)) {
    DBI::dbGetQuery(db$pool, db_sql(db, sql), params = unname(params))
  } else {
    DBI::dbGetQuery(db$pool, sql)
  }
}

#' Create the quiz tables if needed
#'
#' Timestamps are stored as epoch seconds so SQL is portable across backends.
#'
#' @param db A database handle.
#'
#' @noRd
db_create_tables <- function(db) {
  db_execute(
    db,
    "CREATE TABLE IF NOT EXISTS attempts (
      user_id TEXT PRIMARY KEY,
      name TEXT,
      email TEXT,
      quiz_id TEXT NOT NULL,
      quiz_version TEXT NOT NULL,
      question_ids TEXT NOT NULL,
      option_orders TEXT NOT NULL,
      n_questions INTEGER NOT NULL,
      started_at DOUBLE PRECISION NOT NULL,
      deadline_at DOUBLE PRECISION NOT NULL,
      submitted_at DOUBLE PRECISION,
      submit_reason TEXT,
      score INTEGER,
      duration_seconds DOUBLE PRECISION
    )"
  )
  db_execute(
    db,
    "CREATE TABLE IF NOT EXISTS answers (
      user_id TEXT NOT NULL REFERENCES attempts(user_id),
      question_id TEXT NOT NULL,
      value TEXT NOT NULL,
      answered_at DOUBLE PRECISION NOT NULL,
      PRIMARY KEY (user_id, question_id)
    )"
  )
  db_ensure_name_column(db)
  invisible(db)
}

#' Ensure the `attempts.name` column exists
#'
#' New schemas (`db_create_tables()`) already include `name`, but a database
#' created before the leaderboard-name feature will not have it. This adds the
#' column in place, leaving existing rows with a `NULL` name. Idempotent on
#' both backends.
#'
#' @param db A database handle.
#'
#' @noRd
db_ensure_name_column <- function(db) {
  if (db_has_column(db, "attempts", "name")) {
    return(invisible(db))
  }
  db_execute(db, "ALTER TABLE attempts ADD COLUMN name TEXT")
  invisible(db)
}

#' Whether a table has a given column
#'
#' @param db A database handle.
#' @param table,column Table and column names.
#'
#' @noRd
db_has_column <- function(db, table, column) {
  if (identical(db$backend, "postgres")) {
    found <- db_query(
      db,
      "SELECT 1 FROM information_schema.columns
       WHERE table_schema = current_schema()
         AND table_name = ? AND column_name = ?",
      list(table, column)
    )
    return(nrow(found) > 0)
  }
  # SQLite: PRAGMA does not accept bound parameters, so interpolate the
  # table name (it is a constant from our own code, never user input)
  cols <- db_query(db, sprintf("PRAGMA table_info(%s)", table))
  column %in% cols$name
}

#' Get a user's attempt
#'
#' @param db A database handle.
#' @param user_id User id (JWT `sub`).
#'
#' @return A list, or `NULL` if the user has no attempt.
#'
#' @importFrom jsonlite fromJSON
#' @noRd
db_get_attempt <- function(db, user_id) {
  row <- db_query(db, "SELECT * FROM attempts WHERE user_id = ?", list(user_id))
  if (!nrow(row)) {
    return(NULL)
  }
  attempt <- as.list(row[1, , drop = FALSE])
  attempt$question_ids <- as.character(jsonlite::fromJSON(attempt$question_ids))
  attempt$option_orders <- lapply(
    jsonlite::fromJSON(attempt$option_orders, simplifyVector = FALSE),
    as.character
  )
  for (field in c(
    "started_at",
    "deadline_at",
    "submitted_at",
    "duration_seconds"
  )) {
    attempt[[field]] <- as.numeric(attempt[[field]])
  }
  attempt$n_questions <- as.integer(attempt$n_questions)
  attempt$score <- as.integer(attempt$score)
  attempt
}

#' Get the user's attempt, starting a new one if needed
#'
#' Safe under concurrent calls (two tabs, several instances): the first insert
#' wins and everyone reads the same attempt back.
#'
#' @param db A database handle.
#' @param user A user list (`sub`, `name`, `email`).
#' @param bank A quiz bank.
#' @param n_questions Number of questions to draw.
#' @param time_limit_seconds Time limit for the whole quiz.
#' @param now Current time in epoch seconds.
#'
#' @importFrom jsonlite toJSON
#' @noRd
db_get_or_start_attempt <- function(
  db,
  user,
  bank,
  n_questions,
  time_limit_seconds,
  now = epoch_now()
) {
  existing <- db_get_attempt(db, user$sub)
  if (!is.null(existing)) {
    return(existing)
  }
  draw <- draw_quiz(bank, n_questions)
  db_execute(
    db,
    "INSERT INTO attempts (
      user_id, name, email, quiz_id, quiz_version, question_ids,
      option_orders, n_questions, started_at, deadline_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT (user_id) DO NOTHING",
    list(
      user$sub,
      user$name %||% NA_character_,
      user$email %||% NA_character_,
      bank$id %||% "quiz",
      bank$version,
      as.character(jsonlite::toJSON(draw$question_ids)),
      as.character(jsonlite::toJSON(draw$option_orders)),
      as.integer(n_questions),
      now,
      now + time_limit_seconds
    )
  )
  db_get_attempt(db, user$sub)
}

#' Save (or change) an answer
#'
#' The write only happens while the attempt is open and the deadline (plus
#' grace) has not passed; the check runs inside the statement itself.
#'
#' @param db A database handle.
#' @param user_id User id.
#' @param question_id,value The answer.
#' @param now Current time in epoch seconds.
#' @param grace_seconds Allowed slack after the deadline.
#'
#' @return `TRUE` if saved, `FALSE` if rejected.
#'
#' @noRd
db_save_answer <- function(
  db,
  user_id,
  question_id,
  value,
  now = epoch_now(),
  grace_seconds = 0
) {
  n <- db_execute(
    db,
    "INSERT INTO answers (user_id, question_id, value, answered_at)
     SELECT user_id, ?, ?, CAST(? AS DOUBLE PRECISION)
     FROM attempts
     WHERE user_id = ? AND submitted_at IS NULL
       AND deadline_at >= CAST(? AS DOUBLE PRECISION)
     ON CONFLICT (user_id, question_id) DO UPDATE SET
       value = excluded.value,
       answered_at = excluded.answered_at",
    list(question_id, value, now, user_id, now - grace_seconds)
  )
  n > 0
}

#' Get a user's answers
#'
#' @param db A database handle.
#' @param user_id User id.
#'
#' @return Named list of option values by question id.
#'
#' @noRd
db_get_answers <- function(db, user_id) {
  rows <- db_query(
    db,
    "SELECT question_id, value FROM answers WHERE user_id = ?",
    list(user_id)
  )
  stats::setNames(as.list(rows$value), rows$question_id)
}

#' Submit an attempt and record its score
#'
#' Idempotent: only the first call for an attempt has an effect.
#'
#' @param db A database handle.
#' @param bank A quiz bank.
#' @param user_id User id.
#' @param reason `"finished"` or `"timeout"`.
#' @param now Current time in epoch seconds.
#'
#' @return `TRUE` if this call submitted the attempt.
#'
#' @noRd
db_finalize_attempt <- function(
  db,
  bank,
  user_id,
  reason = c("finished", "timeout"),
  now = epoch_now()
) {
  reason <- match.arg(reason)
  attempt <- db_get_attempt(db, user_id)
  if (is.null(attempt) || !is.na(attempt$submitted_at)) {
    return(FALSE)
  }
  scored <- score_answers(
    bank,
    attempt$question_ids,
    db_get_answers(db, user_id)
  )
  duration <- min(now, attempt$deadline_at) - attempt$started_at
  n <- db_execute(
    db,
    "UPDATE attempts SET
       submitted_at = CAST(? AS DOUBLE PRECISION),
       submit_reason = ?,
       score = CAST(? AS INTEGER),
       duration_seconds = CAST(? AS DOUBLE PRECISION)
     WHERE user_id = ? AND submitted_at IS NULL",
    list(now, reason, sum(scored$is_correct), duration, user_id)
  )
  n > 0
}

#' Leaderboard of submitted attempts
#'
#' Ranked by score (high first), then time taken (low first).
#'
#' @param db A database handle.
#' @param n Number of rows.
#'
#' @noRd
db_leaderboard <- function(db, n = 10) {
  db_query(
    db,
    "SELECT user_id, name, score, n_questions, duration_seconds
     FROM attempts
     WHERE submitted_at IS NOT NULL
     ORDER BY score DESC, duration_seconds ASC, submitted_at ASC
     LIMIT ?",
    list(as.integer(n))
  )
}

#' A user's rank among submitted attempts
#'
#' @param db A database handle.
#' @param user_id User id.
#'
#' @return A list with `rank` and `total`, or `NULL` if not submitted.
#'
#' @noRd
db_user_rank <- function(db, user_id) {
  attempt <- db_get_attempt(db, user_id)
  if (is.null(attempt) || is.na(attempt$submitted_at)) {
    return(NULL)
  }
  ahead <- db_query(
    db,
    "SELECT COUNT(*) AS n FROM attempts
     WHERE submitted_at IS NOT NULL AND (score > ? OR (score = ? AND
       duration_seconds < CAST(? AS DOUBLE PRECISION)))",
    list(attempt$score, attempt$score, attempt$duration_seconds)
  )
  total <- db_query(
    db,
    "SELECT COUNT(*) AS n FROM attempts WHERE submitted_at IS NOT NULL"
  )
  list(rank = as.integer(ahead$n) + 1L, total = as.integer(total$n))
}
