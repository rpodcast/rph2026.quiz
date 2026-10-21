#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib nav_select
#' @noRd
app_server <- function(input, output, session) {
  resources <- app_resources()
  settings <- resources$settings
  db <- resources$db
  bank <- resources$bank

  local_rv <- reactiveValues(
    intro_name_error = ""
  )

  quiz_state <- reactiveValues(
    user = resolve_user(
      session,
      settings$auth_mode,
      pubkey = if (identical(settings$auth_mode, "ricochet")) ricochet_pubkey(),
      content_id = Sys.getenv("RICOCHET_CONTENT_ID")
    ),
    attempt = NULL,
    status = NULL
  )

  mod_quiz_server("quiz", quiz_state, db, bank, settings)
  mod_results_server("results", quiz_state, db, bank, settings)

  # Decide the starting screen once per session
  observeEvent(
    TRUE,
    {
      user <- quiz_state$user
      if (is.null(user)) {
        quiz_state$status <- "signin"
        return()
      }
      attempt <- db_get_attempt(db, user$sub)
      if (!is.null(attempt) && is.na(attempt$submitted_at)) {
        if (seconds_remaining(attempt$deadline_at) <= 0) {
          db_finalize_attempt(db, bank, user$sub, reason = "timeout")
          attempt <- db_get_attempt(db, user$sub)
        }
      }
      quiz_state$attempt <- attempt
      quiz_state$status <- if (is.null(attempt)) {
        "intro"
      } else if (is.na(attempt$submitted_at)) {
        "quiz"
      } else {
        "results"
      }
    },
    once = TRUE
  )

  # Pre-fill the leaderboard name from the identity (often blank for auth0)
  observeEvent(
    quiz_state$status,
    {
      req(identical(quiz_state$status, "intro"))
      prefill <- sanitize_name(quiz_state$user$name)
      if (nzchar(prefill)) {
        updateTextInput(session, "intro_name", value = prefill)
      }
    }
  )

  observeEvent(
    input$start,
    {
      req(identical(quiz_state$status, "intro"), !is.null(quiz_state$user))
      name <- sanitize_name(input$intro_name)
      if (!nzchar(name)) {
        local_rv$intro_name_error <- "Please enter a name before starting."
        return()
      }
      local_rv$intro_name_error <- ""
      quiz_state$user$name <- name
      quiz_state$attempt <- db_get_or_start_attempt(
        db,
        quiz_state$user,
        bank,
        n_questions = settings$quiz_n_questions,
        time_limit_seconds = settings$quiz_time_limit_minutes * 60
      )
      quiz_state$status <- if (is.na(quiz_state$attempt$submitted_at)) {
        "quiz"
      } else {
        "results"
      }
    }
  )

  output$intro_name_error <- renderText({
    local_rv$intro_name_error
  })

  observeEvent(
    quiz_state$status,
    {
      bslib::nav_select("screen", quiz_state$status)
    }
  )

  output$user_name <- renderText({
    quiz_state$user$name %||% ""
  })

  output$welcome <- renderText({
    first <- strsplit(trimws(quiz_state$user$name %||% ""), "\\s+")[[1]][1]
    paste0("Welcome, ", first %||% "", "!")
  })

  output$intro_text <- renderText({
    sprintf(
      "You will get %d questions drawn at random and %s to answer them all. The clock starts when you press Start.",
      settings$quiz_n_questions,
      format_minutes(settings$quiz_time_limit_minutes)
    )
  })
}

#' Process-wide app resources
#'
#' Settings, the question bank, and the database pool are created once per R
#' process and shared by every session. The pool closes when the app stops.
#'
#' @importFrom shiny onStop
#' @noRd
app_resources <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      settings <- quiz_settings()
      bank <- read_quiz_bank(settings$quiz_bank)
      validate_quiz_bank(bank, settings$quiz_n_questions)
      db <- db_connect(settings$db_backend, settings$sqlite_path)
      db_create_tables(db)
      cache <<- list(settings = settings, bank = bank, db = db)
      # session = NULL registers at app level, not on the first session
      shiny::onStop(
        function() {
          db_close(db)
          cache <<- NULL
        },
        session = NULL
      )
    }
    cache
  }
})

#' Format a number of minutes for display
#'
#' @param minutes Number of minutes.
#'
#' @noRd
format_minutes <- function(minutes) {
  if (minutes == 1) "1 minute" else paste(minutes, "minutes")
}
