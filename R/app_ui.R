#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page navset_hidden nav_panel_hidden card card_body
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page(
      title = "R/Pharma 2026 Quiz!",
      theme = quiz_theme(),
      tags$div(
        class = "chalkboard",
        tags$header(
          class = "quiz-header d-flex flex-wrap align-items-center justify-content-between gap-3 px-4 py-3",
          tags$h1(class = "quiz-title m-0", "R/Pharma Conference Quiz"),
          tags$div(
            class = "d-flex align-items-center gap-3",
            tags$span(
              id = "quiz-timer",
              class = "quiz-timer badge text-bg-secondary fs-5 d-none",
              role = "timer",
              `aria-live` = "off",
              "--:--"
            ),
            tags$span(
              class = "quiz-user",
              textOutput("user_name", inline = TRUE)
            )
          )
        ),
        tags$main(
          class = "container quiz-main py-4",
          bslib::navset_hidden(
            id = "screen",
            bslib::nav_panel_hidden(
              "loading",
              tags$p(class = "text-muted", "Loading the quiz...")
            ),
            bslib::nav_panel_hidden(
              "signin",
              bslib::card(
                bslib::card_body(
                  tags$h2(class = "h4", "Please sign in"),
                  tags$p(
                    "We could not verify who you are. Open this quiz from your",
                    "Ricochet account to take part."
                  )
                )
              )
            ),
            bslib::nav_panel_hidden(
              "intro",
              bslib::card(
                bslib::card_body(
                  tags$h2(class = "h4", textOutput("welcome", inline = TRUE)),
                  tags$p(textOutput("intro_text", inline = TRUE)),
                  tags$ul(
                    tags$li(
                      "Questions come from talks at past R/Pharma conferences."
                    ),
                    tags$li(
                      "You can go back and change answers until you submit."
                    ),
                    tags$li(
                      "When time runs out, the quiz is submitted automatically."
                    ),
                    tags$li("You get one attempt.")
                  ),
                  textInput(
                    "intro_name",
                    "Name for the leaderboard",
                    placeholder = "Your name"
                  ),
                  textOutput("intro_name_error", inline = TRUE),
                  actionButton(
                    "start",
                    "Start the quiz",
                    class = "btn-primary align-self-start"
                  )
                )
              )
            ),
            bslib::nav_panel_hidden("quiz", mod_quiz_ui("quiz")),
            bslib::nav_panel_hidden("results", mod_results_ui("results"))
          )
        ),
        # Decorative chalk and eraser on the board's ledge
        tags$div(
          class = "chalk-tray",
          `aria-hidden` = "true",
          tags$span(class = "chalk-stick"),
          tags$span(class = "chalk-stick chalk-stick-yellow"),
          tags$span(class = "chalk-eraser")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "R/Pharma Quiz"
    )
  )
}
