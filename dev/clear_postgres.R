# Delete all rows from the production Postgres quiz tables.
#
# Empties `answers` and `attempts` (the tables created by
# dev/init_postgres.R) but keeps the tables themselves. This wipes every
# attempt and the whole leaderboard, and cannot be undone. Credentials come
# from the PG_* variables in .Renviron.
#
# Run it interactively: it shows the row counts, then asks you to type the
# database name to confirm.

pkgload::load_all(golem::pkg_path(), quiet = TRUE)

local({
  if (!interactive()) {
    stop("Run this script interactively so you can confirm.", call. = FALSE)
  }

  db <- db_connect("postgres", use_pool = FALSE)
  on.exit(db_close(db), add = TRUE)

  db_name <- db_query(db, "SELECT current_database() AS name")$name
  count_rows <- function() {
    db_query(
      db,
      "SELECT (SELECT count(*) FROM attempts) AS n_attempts,
              (SELECT count(*) FROM answers) AS n_answers"
    )
  }

  before <- count_rows()
  message(
    "Postgres database '",
    db_name,
    "' on ",
    Sys.getenv("PG_HOST"),
    " has ",
    before$n_attempts,
    " attempt(s) and ",
    before$n_answers,
    " answer(s)."
  )
  if (before$n_attempts == 0 && before$n_answers == 0) {
    message("Nothing to delete.")
    return(invisible())
  }

  answer <- readline(paste0(
    "Type the database name ('",
    db_name,
    "') to delete ALL rows: "
  ))
  if (!identical(trimws(answer), db_name)) {
    message("Aborted; nothing was deleted.")
    return(invisible())
  }

  # answers first: it references attempts
  DBI::dbWithTransaction(db$pool, {
    db_execute(db, "DELETE FROM answers")
    db_execute(db, "DELETE FROM attempts")
  })

  after <- count_rows()
  message(
    "Done. '",
    db_name,
    "' now has ",
    after$n_attempts,
    " attempt(s) and ",
    after$n_answers,
    " answer(s)."
  )
})
