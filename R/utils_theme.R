#' Chalkboard theme for the quiz
#'
#' A Bootstrap 5 theme that looks like chalk on a green classroom
#' chalkboard. Colors are "chalk" tints chosen to keep readable contrast on
#' the board. The fonts are self-hosted in `inst/app/www/fonts/` and declared
#' with `@font-face` in `inst/app/www/custom.css`.
#'
#' @return A `bslib::bs_theme()` object.
#'
#' @importFrom bslib bs_theme
#' @noRd
quiz_theme <- function() {
  colors <- chalk_colors()
  bslib::bs_theme(
    version = 5,
    bg = colors[["board"]],
    fg = colors[["chalk"]],
    primary = colors[["yellow"]],
    secondary = colors[["grey"]],
    success = colors[["green"]],
    info = colors[["blue"]],
    warning = colors[["orange"]],
    danger = colors[["pink"]],
    base_font = "'Patrick Hand', 'Comic Sans MS', cursive",
    heading_font = "'Walter Turncoat', 'Patrick Hand', cursive",
    font_scale = 1.2,
    # bslib blends fg toward bg for body text; keep full chalk contrast
    "body-color" = colors[["chalk"]],
    "border-radius" = "0.5rem",
    "link-color" = colors[["yellow"]]
  )
}

#' Chalkboard palette
#'
#' @return Named character vector of hex colors.
#'
#' @noRd
chalk_colors <- function() {
  c(
    board = "#2f5240",
    chalk = "#f2f0e6",
    yellow = "#f6e27a",
    grey = "#c9d1cc",
    green = "#a8e6a1",
    blue = "#9fd3f2",
    orange = "#f7c07a",
    pink = "#f7b2c0"
  )
}
