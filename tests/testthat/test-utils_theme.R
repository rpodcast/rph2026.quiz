relative_luminance <- function(hex) {
  channels <- grDevices::col2rgb(hex)[, 1] / 255
  linear <- ifelse(
    channels <= 0.03928,
    channels / 12.92,
    ((channels + 0.055) / 1.055)^2.4
  )
  sum(c(0.2126, 0.7152, 0.0722) * linear)
}

contrast_ratio <- function(a, b) {
  lum <- sort(
    c(relative_luminance(a), relative_luminance(b)),
    decreasing = TRUE
  )
  (lum[[1]] + 0.05) / (lum[[2]] + 0.05)
}

test_that("quiz_theme() uses the chalkboard palette", {
  theme <- quiz_theme()
  expect_true(bslib::is_bs_theme(theme))
  vars <- bslib::bs_get_variables(theme, c("body-bg", "body-color", "primary"))
  colors <- chalk_colors()
  expect_equal(tolower(unname(vars[["body-bg"]])), colors[["board"]])
  expect_equal(tolower(unname(vars[["body-color"]])), colors[["chalk"]])
  expect_equal(tolower(unname(vars[["primary"]])), colors[["yellow"]])
})

test_that("every chalk color is readable on the board (WCAG AA)", {
  colors <- chalk_colors()
  chalk <- colors[names(colors) != "board"]
  ratios <- vapply(chalk, contrast_ratio, numeric(1), b = colors[["board"]])
  expect_true(all(ratios >= 4.5), info = paste(names(ratios), round(ratios, 2)))
})

test_that("quiz_theme() compiles to CSS", {
  skip_on_cran()
  css <- as.character(sass::sass(quiz_theme()))
  expect_match(css, "--bs-primary: #f6e27a", ignore.case = TRUE)
  expect_match(css, "Walter Turncoat", fixed = TRUE)
})

test_that("app_ui() renders the chalkboard layout", {
  html <- as.character(htmltools::renderTags(app_ui(NULL))$html)
  expect_match(html, "class=\"chalkboard", fixed = TRUE)
  expect_match(html, "chalk-tray", fixed = TRUE)
  expect_match(html, "id=\"quiz-timer\"", fixed = TRUE)
})
