#' Quiz settings
#'
#' Reads quiz settings from `golem-config.yml`. Any setting can be overridden
#' at run time with a `run_app()` argument of the same name, e.g.
#' `run_app(quiz_time_limit_minutes = 2)`.
#'
#' @return A named list of settings.
#'
#' @importFrom golem get_golem_options
#' @noRd
quiz_settings <- function() {
  keys <- c(
    "quiz_bank",
    "quiz_n_questions",
    "quiz_time_limit_minutes",
    "quiz_grace_seconds",
    "leaderboard_size",
    "auth_mode",
    "db_backend",
    "sqlite_path"
  )
  settings <- lapply(keys, function(key) {
    golem::get_golem_options(key) %||% get_golem_config(key)
  })
  settings <- stats::setNames(settings, keys)
  validate_settings(settings)
}

#' Validate quiz settings
#'
#' @param settings Named list of settings.
#'
#' @noRd
validate_settings <- function(settings) {
  positive <- c(
    "quiz_n_questions",
    "quiz_time_limit_minutes",
    "leaderboard_size"
  )
  for (key in positive) {
    value <- settings[[key]]
    if (
      !is.numeric(value) || length(value) != 1 || is.na(value) || value <= 0
    ) {
      stop(
        sprintf("Setting `%s` must be a positive number.", key),
        call. = FALSE
      )
    }
  }
  settings$quiz_grace_seconds <- settings$quiz_grace_seconds %||% 0
  if (!settings$auth_mode %in% c("mock", "ricochet")) {
    stop("Setting `auth_mode` must be 'mock' or 'ricochet'.", call. = FALSE)
  }
  if (!settings$db_backend %in% c("sqlite", "postgres")) {
    stop("Setting `db_backend` must be 'sqlite' or 'postgres'.", call. = FALSE)
  }
  settings
}
