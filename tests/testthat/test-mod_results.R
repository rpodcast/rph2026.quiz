test_that("results module scores the attempt and builds the leaderboard", {
  db <- local_test_db()
  bank <- make_bank(5)
  attempt <- db_get_or_start_attempt(db, test_user(), bank, 3, 600)
  db_save_answer(db, "u1", attempt$question_ids[[1]], "a")
  db_finalize_attempt(db, bank, "u1", "finished")
  quiz_state <- shiny::reactiveValues(
    user = test_user(),
    attempt = db_get_attempt(db, "u1"),
    status = NULL
  )

  shiny::testServer(
    mod_results_server,
    args = list(
      quiz_state = quiz_state,
      db = db,
      bank = bank,
      settings = list(leaderboard_size = 10)
    ),
    {
      quiz_state$status <- "results"
      session$flushReact()
      expect_equal(output$score, "1 / 3")
      expect_equal(output$rank, "#1")
      expect_equal(nrow(local_rv$leaderboard), 1)
    }
  )
})

test_that("format_leaderboard escapes names and marks the user", {
  board <- data.frame(
    user_id = c("u1", "u2"),
    name = c("Ada", "<b>Bob</b>"),
    score = c(3L, 2L),
    n_questions = c(3L, 3L),
    duration_seconds = c(61, 90)
  )
  out <- format_leaderboard(board, "u1")
  expect_match(out$Name[[1]], "(you)", fixed = TRUE)
  expect_equal(out$Name[[2]], "&lt;b&gt;Bob&lt;/b&gt;")
  expect_equal(out$Time, c("1:01", "1:30"))
})

test_that("review_list does not reveal correct answers or explanations", {
  bank <- make_bank(3)
  scored <- score_answers(
    bank,
    names(bank$questions),
    list(t01 = "a", t02 = "b")
  )
  html <- as.character(review_list(scored))

  expect_match(html, "Your answer:\\s+Right 1")
  expect_match(html, "Your answer:\\s+Wrong B 2")
  expect_no_match(html, "Correct answer", fixed = TRUE)
  expect_no_match(html, "Right 2", fixed = TRUE)
  expect_no_match(html, "Right 3", fixed = TRUE)
  expect_no_match(html, "Because", fixed = TRUE)
  expect_match(html, "Not answered", fixed = TRUE)
})

test_that("module ui works", {
  ui <- mod_results_ui(id = "test")
  golem::expect_shinytaglist(ui)
  expect_true("id" %in% names(formals(mod_results_ui)))
})
