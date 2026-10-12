# Reset the mock dev user so the quiz can be taken again.
#
# Deletes the mock user's attempt and answers from the database of the active
# config (GOLEM_CONFIG_ACTIVE, falling back to "default"):
#   - default:          local SQLite file
#   - local_production: production Postgres (PG_* variables in .Renviron)
# Only the mock user's rows are touched, and only configs with
# auth_mode = mock are allowed, so this refuses to run against `production`.
# Restart the app (or reload the page) afterwards.

pkgload::load_all(golem::pkg_path(), quiet = TRUE)

local({
  config <- Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv("R_CONFIG_ACTIVE", "default")
  )
  auth_mode <- get_golem_config("auth_mode", config = config)
  db_backend <- get_golem_config("db_backend", config = config)
  if (!identical(auth_mode, "mock")) {
    stop(
      "Refusing to reset: config '",
      config,
      "' does not use auth_mode = mock.",
      call. = FALSE
    )
  }

  user_id <- mock_user()$sub

  if (identical(db_backend, "sqlite")) {
    sqlite_path <- file.path(
      golem::pkg_path(),
      get_golem_config("sqlite_path", config = config)
    )
    if (!file.exists(sqlite_path)) {
      message("No database at ", sqlite_path, "; nothing to reset.")
      return(invisible())
    }
    db <- db_connect("sqlite", sqlite_path = sqlite_path, use_pool = FALSE)
    target <- sqlite_path
  } else if (identical(db_backend, "postgres")) {
    db <- db_connect("postgres", use_pool = FALSE)
    target <- paste0("Postgres database '", Sys.getenv("PG_DATABASE"), "'")
  } else {
    stop("Unsupported db_backend: ", db_backend, call. = FALSE)
  }
  on.exit(db_close(db), add = TRUE)

  db_create_tables(db)
  # answers first: it references attempts
  deleted <- DBI::dbWithTransaction(db$pool, {
    c(
      answers = db_execute(
        db,
        "DELETE FROM answers WHERE user_id = ?",
        list(user_id)
      ),
      attempts = db_execute(
        db,
        "DELETE FROM attempts WHERE user_id = ?",
        list(user_id)
      )
    )
  })
  message(
    "Reset '",
    user_id,
    "' in ",
    target,
    " (config '",
    config,
    "'): deleted ",
    deleted[["attempts"]],
    " attempt(s) and ",
    deleted[["answers"]],
    " answer(s)."
  )
})
