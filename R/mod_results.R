#' results UI Function
#'
#' Score summary, per-question review (without correct answers), and
#' leaderboard.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags textOutput htmlOutput tableOutput
#' @importFrom bslib card card_header card_body layout_columns value_box
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::value_box(
        title = "Score",
        value = textOutput(ns("score"), inline = TRUE),
        textOutput(ns("submit_reason"), inline = TRUE)
      ),
      bslib::value_box(
        title = "Time taken",
        value = textOutput(ns("duration"), inline = TRUE)
      ),
      bslib::value_box(
        title = "Your rank",
        value = textOutput(ns("rank"), inline = TRUE),
        textOutput(ns("rank_total"), inline = TRUE)
      )
    ),
    bslib::card(
      bslib::card_header("Leaderboard"),
      bslib::card_body(
        tableOutput(ns("leaderboard"))
      )
    ),
    bslib::card(
      bslib::card_header("Your answers"),
      bslib::card_body(
        htmlOutput(ns("review"))
      )
    )
  )
}

#' results Server Functions
#'
#' @inheritParams mod_quiz_server
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent renderText
#'   renderTable renderUI
#' @noRd
mod_results_server <- function(id, quiz_state, db, bank, settings) {
  moduleServer(id, function(input, output, session) {
    local_rv <- reactiveValues(
      scored = NULL,
      rank = NULL,
      leaderboard = NULL
    )

    observeEvent(
      quiz_state$status,
      {
        req(identical(quiz_state$status, "results"))
        user_id <- quiz_state$user$sub
        local_rv$scored <- score_answers(
          bank,
          quiz_state$attempt$question_ids,
          db_get_answers(db, user_id)
        )
        local_rv$rank <- db_user_rank(db, user_id)
        local_rv$leaderboard <- db_leaderboard(db, settings$leaderboard_size)
      }
    )

    output$score <- renderText({
      req(local_rv$scored)
      sprintf(
        "%d / %d",
        sum(local_rv$scored$is_correct),
        nrow(local_rv$scored)
      )
    })

    output$submit_reason <- renderText({
      req(quiz_state$attempt)
      if (identical(quiz_state$attempt$submit_reason, "timeout")) {
        "Time ran out; unanswered questions count as incorrect."
      } else {
        "Submitted"
      }
    })

    output$duration <- renderText({
      req(quiz_state$attempt)
      format_duration(quiz_state$attempt$duration_seconds)
    })

    output$rank <- renderText({
      req(local_rv$rank)
      paste0("#", local_rv$rank$rank)
    })

    output$rank_total <- renderText({
      req(local_rv$rank)
      sprintf("of %d participants", local_rv$rank$total)
    })

    output$leaderboard <- renderTable(
      {
        req(local_rv$leaderboard)
        format_leaderboard(local_rv$leaderboard, quiz_state$user$sub)
      },
      sanitize.text.function = identity,
      width = "100%",
      striped = TRUE
    )

    # One-shot static review list; content depends on the attempt
    output$review <- renderUI({
      req(local_rv$scored)
      review_list(local_rv$scored)
    })
  })
}

#' Format the leaderboard for display
#'
#' @param leaderboard Data frame from `db_leaderboard()`.
#' @param user_id Current user's id, highlighted in the table.
#'
#' @importFrom htmltools htmlEscape
#' @noRd
format_leaderboard <- function(leaderboard, user_id) {
  if (!nrow(leaderboard)) {
    return(data.frame(Message = "No submissions yet."))
  }
  names <- htmltools::htmlEscape(leaderboard$name)
  is_me <- leaderboard$user_id == user_id
  names[is_me] <- paste0("<strong>", names[is_me], " (you)</strong>")
  data.frame(
    Rank = seq_len(nrow(leaderboard)),
    Name = names,
    Score = sprintf("%d / %d", leaderboard$score, leaderboard$n_questions),
    Time = vapply(leaderboard$duration_seconds, format_duration, character(1)),
    check.names = FALSE
  )
}

#' Build the per-question review list
#'
#' @param scored Data frame from `score_answers()`.
#'
#' @importFrom htmltools tags tagList
#' @noRd
review_list <- function(scored) {
  items <- lapply(seq_len(nrow(scored)), function(i) {
    row <- scored[i, ]
    status <- if (row$is_correct) {
      "Correct"
    } else if (is.na(row$answer)) {
      "Not answered"
    } else {
      "Incorrect"
    }
    htmltools::tags$li(
      class = paste(
        "list-group-item review-item",
        if (row$is_correct) "review-correct" else "review-incorrect"
      ),
      htmltools::tags$div(
        class = "d-flex justify-content-between gap-2",
        htmltools::tags$strong(sprintf("%d. %s", row$position, row$prompt)),
        htmltools::tags$span(
          class = paste(
            "badge align-self-start",
            if (row$is_correct) "text-bg-success" else "text-bg-danger"
          ),
          status
        )
      ),
      # Correct answers and explanations are deliberately not revealed
      if (!is.na(row$answer)) {
        htmltools::tags$div(
          class = "mt-1",
          "Your answer: ",
          row$answer_label
        )
      }
    )
  })
  htmltools::tags$ol(class = "list-group list-unstyled", items)
}

## To be copied in the UI
# mod_results_ui("results")

## To be copied in the server
# mod_results_server("results", quiz_state, db, bank, settings)
