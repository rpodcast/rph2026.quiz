#' quiz UI Function
#'
#' One question at a time, with Back / Next navigation and a numbered question
#' navigator for jumping to any question. The countdown and the navigator
#' buttons are driven by `inst/app/www/timer.js`.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags textOutput radioButtons actionButton
#' @importFrom bslib card card_header card_body card_footer
mod_quiz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "quiz-progress mb-3",
      tags$div(
        class = "d-flex justify-content-between small text-muted mb-1",
        textOutput(ns("position"), inline = TRUE),
        textOutput(ns("answered"), inline = TRUE)
      ),
      tags$div(
        class = "progress",
        role = "progressbar",
        `aria-label` = "Questions answered",
        tags$div(
          id = ns("progress_bar"),
          class = "progress-bar",
          style = "width: 0%"
        )
      )
    ),
    tags$nav(
      class = "quiz-nav mb-3",
      `aria-label` = "Jump to question",
      tags$div(
        id = ns("nav"),
        class = "d-flex flex-wrap gap-1"
      ),
      tags$div(
        class = "small text-muted mt-1",
        "Outlined numbers are unanswered. Select a number to jump to it."
      )
    ),
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center gap-2",
        textOutput(ns("category"), inline = TRUE)
      ),
      bslib::card_body(
        tags$h2(
          class = "h4 quiz-prompt",
          textOutput(ns("prompt"), inline = TRUE)
        ),
        radioButtons(
          ns("answer"),
          label = NULL,
          choices = c("Loading" = "loading"),
          selected = character(0),
          width = "100%"
        )
      ),
      bslib::card_footer(
        class = "d-flex justify-content-between",
        actionButton(ns("back"), "Back", class = "btn-outline-secondary"),
        actionButton(ns("next_q"), "Next", class = "btn-primary")
      )
    )
  )
}

#' quiz Server Functions
#'
#' @param quiz_state Shared `reactiveValues()` with `user`, `attempt`, and
#'   `status`. This module sets `status` to `"results"` once the attempt is
#'   submitted.
#' @param db Database handle from `db_connect()`.
#' @param bank Quiz bank from `load_quiz_bank()`.
#' @param settings Settings from `quiz_settings()`.
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent renderText
#'   updateRadioButtons updateActionButton showModal removeModal modalDialog
#'   modalButton isolate onSessionEnded req tags
#' @importFrom later later
#' @noRd
mod_quiz_server <- function(id, quiz_state, db, bank, settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    local_rv <- reactiveValues(
      index = NULL,
      answers = list()
    )
    timers <- new.env(parent = emptyenv())

    current_question <- function() {
      bank$questions[[quiz_state$attempt$question_ids[[local_rv$index]]]]
    }

    show_question <- function(index) {
      local_rv$index <- index
      attempt <- quiz_state$attempt
      question_id <- attempt$question_ids[[index]]
      question <- bank$questions[[question_id]]
      saved <- local_rv$answers[[question_id]]
      updateRadioButtons(
        session,
        "answer",
        choices = question_choices(
          question,
          attempt$option_orders[[question_id]]
        ),
        selected = if (is.null(saved)) {
          character(0)
        } else {
          encode_choice(question_id, saved)
        }
      )
      updateActionButton(session, "back", disabled = index == 1)
      updateActionButton(
        session,
        "next_q",
        label = if (index == attempt$n_questions) "Finish" else "Next"
      )
      send_nav()
    }

    send_progress <- function() {
      n <- quiz_state$attempt$n_questions
      session$sendCustomMessage(
        "quiz_progress",
        list(
          id = ns("progress_bar"),
          percent = round(100 * length(local_rv$answers) / n)
        )
      )
    }

    send_nav <- function() {
      session$sendCustomMessage(
        "quiz_nav",
        list(
          id = ns("nav"),
          input_id = ns("jump"),
          current = local_rv$index,
          answered = question_nav_state(
            quiz_state$attempt$question_ids,
            names(local_rv$answers)
          )
        )
      )
    }

    finalize <- function(reason) {
      db_finalize_attempt(db, bank, quiz_state$user$sub, reason = reason)
      removeModal()
      session$sendCustomMessage("quiz_timer_stop", list())
      quiz_state$attempt <- db_get_attempt(db, quiz_state$user$sub)
      quiz_state$status <- "results"
    }

    is_expired <- function() {
      seconds_remaining(quiz_state$attempt$deadline_at) <= 0
    }

    observeEvent(
      quiz_state$status,
      {
        req(identical(quiz_state$status, "quiz"))
        attempt <- quiz_state$attempt
        local_rv$answers <- db_get_answers(db, quiz_state$user$sub)
        unanswered <- which(!attempt$question_ids %in% names(local_rv$answers))
        show_question(if (length(unanswered)) unanswered[[1]] else 1L)
        send_progress()

        remaining <- seconds_remaining(attempt$deadline_at)
        session$sendCustomMessage(
          "quiz_timer_start",
          list(
            input_id = ns("time_up"),
            remaining_ms = remaining * 1000
          )
        )

        # Backstop for throttled background tabs: submit on the server too
        if (!is.null(timers$cancel)) {
          timers$cancel()
        }
        timers$cancel <- later::later(
          function() {
            if (session$isClosed()) {
              return()
            }
            shiny::withReactiveDomain(session, {
              if (identical(isolate(quiz_state$status), "quiz")) {
                isolate(finalize("timeout"))
              }
            })
          },
          delay = remaining + settings$quiz_grace_seconds
        )
      }
    )

    onSessionEnded(function() {
      if (!is.null(timers$cancel)) {
        timers$cancel()
      }
    })

    observeEvent(
      input$answer,
      {
        req(identical(quiz_state$status, "quiz"), !is.null(local_rv$index))
        choice <- decode_choice(input$answer)
        req(choice)
        question <- current_question()
        # Ignore stale events from a previously shown question
        req(identical(choice$question_id, question$id))
        req(!identical(local_rv$answers[[question$id]], choice$value))
        req(is_valid_answer(
          bank,
          quiz_state$attempt,
          choice$question_id,
          choice$value
        ))

        saved <- db_save_answer(
          db,
          quiz_state$user$sub,
          choice$question_id,
          choice$value,
          grace_seconds = settings$quiz_grace_seconds
        )
        if (!saved) {
          finalize("timeout")
          return()
        }
        local_rv$answers[[choice$question_id]] <- choice$value
        send_progress()
        send_nav()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$jump,
      {
        req(identical(quiz_state$status, "quiz"))
        target <- suppressWarnings(as.integer(input$jump))
        req(
          length(target) == 1,
          !is.na(target),
          target >= 1,
          target <= quiz_state$attempt$n_questions
        )
        if (is_expired()) {
          finalize("timeout")
          return()
        }
        show_question(target)
      }
    )

    observeEvent(
      input$back,
      {
        req(identical(quiz_state$status, "quiz"), local_rv$index > 1)
        show_question(local_rv$index - 1L)
      }
    )

    observeEvent(
      input$next_q,
      {
        req(identical(quiz_state$status, "quiz"))
        if (is_expired()) {
          finalize("timeout")
          return()
        }
        n <- quiz_state$attempt$n_questions
        if (local_rv$index < n) {
          show_question(local_rv$index + 1L)
          return()
        }
        show_submit_modal()
      }
    )

    show_submit_modal <- function() {
      n <- quiz_state$attempt$n_questions
      missing <- n - length(local_rv$answers)
      showModal(modalDialog(
        title = "Submit your quiz?",
        if (missing > 0) {
          tags$p(sprintf(
            "You have %d unanswered %s (outlined in the question numbers above). Unanswered questions count as incorrect.",
            missing,
            if (missing == 1) "question" else "questions"
          ))
        },
        tags$p(
          "You cannot change your answers after submitting."
        ),
        footer = tagList(
          modalButton("Keep going"),
          actionButton(ns("confirm_finish"), "Submit", class = "btn-primary")
        ),
        easyClose = FALSE
      ))
    }

    observeEvent(
      input$confirm_finish,
      {
        req(identical(quiz_state$status, "quiz"))
        finalize(if (is_expired()) "timeout" else "finished")
      }
    )

    observeEvent(
      input$time_up,
      {
        req(identical(quiz_state$status, "quiz"))
        remaining <- seconds_remaining(quiz_state$attempt$deadline_at)
        if (remaining <= settings$quiz_grace_seconds) {
          finalize("timeout")
        } else {
          # Client clock ran fast; resync
          session$sendCustomMessage(
            "quiz_timer_start",
            list(input_id = ns("time_up"), remaining_ms = remaining * 1000)
          )
        }
      }
    )

    output$position <- renderText({
      req(local_rv$index)
      sprintf(
        "Question %d of %d",
        local_rv$index,
        quiz_state$attempt$n_questions
      )
    })

    output$answered <- renderText({
      req(quiz_state$attempt)
      sprintf(
        "%d of %d answered",
        length(local_rv$answers),
        quiz_state$attempt$n_questions
      )
    })

    output$prompt <- renderText({
      req(local_rv$index)
      current_question()$prompt
    })

    output$category <- renderText({
      req(local_rv$index)
      current_question()$category %||% ""
    })
  })
}

#' Answered flags for the question navigator
#'
#' @param question_ids Question ids in the attempt, in display order.
#' @param answered_ids Ids of questions that have a saved answer.
#'
#' @return Logical vector, one element per question in `question_ids`.
#'
#' @noRd
question_nav_state <- function(question_ids, answered_ids) {
  as.logical(question_ids %in% answered_ids)
}

## To be copied in the UI
# mod_quiz_ui("quiz")

## To be copied in the server
# mod_quiz_server("quiz", quiz_state, db, bank, settings)
