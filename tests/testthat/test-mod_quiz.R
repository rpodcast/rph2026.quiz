quiz_settings_for_test <- function() {
  list(
    quiz_n_questions = 3,
    quiz_time_limit_minutes = 10,
    quiz_grace_seconds = 0,
    leaderboard_size = 10
  )
}

start_quiz_state <- function(db, bank, time_limit = 600) {
  user <- test_user()
  attempt <- db_get_or_start_attempt(db, user, bank, 3, time_limit)
  shiny::reactiveValues(user = user, attempt = attempt, status = NULL)
}

test_that("answering and finishing saves answers and submits", {
  db <- local_test_db()
  bank <- make_bank(5)
  quiz_state <- start_quiz_state(db, bank)

  shiny::testServer(
    mod_quiz_server,
    args = list(
      quiz_state = quiz_state,
      db = db,
      bank = bank,
      settings = quiz_settings_for_test()
    ),
    {
      quiz_state$status <- "quiz"
      session$flushReact()
      ids <- quiz_state$attempt$question_ids
      expect_equal(local_rv$index, 1L)

      session$setInputs(answer = encode_choice(ids[[1]], "a"))
      expect_equal(db_get_answers(db, "u1")[[ids[[1]]]], "a")

      # A stale event for another question is ignored
      session$setInputs(answer = encode_choice(ids[[3]], "a"))
      expect_null(db_get_answers(db, "u1")[[ids[[3]]]])

      session$setInputs(next_q = 1)
      expect_equal(local_rv$index, 2L)
      session$setInputs(answer = encode_choice(ids[[2]], "b"))
      session$setInputs(back = 1)
      expect_equal(local_rv$index, 1L)

      session$setInputs(next_q = 2)
      session$setInputs(next_q = 3)
      expect_equal(local_rv$index, 3L)
      session$setInputs(answer = encode_choice(ids[[3]], "a"))
      session$setInputs(next_q = 4)

      expect_equal(quiz_state$status, "results")
      expect_equal(quiz_state$attempt$score, 2L)
      expect_equal(quiz_state$attempt$submit_reason, "finished")
    }
  )
})

test_that("resuming lands on the first unanswered question", {
  db <- local_test_db()
  bank <- make_bank(5)
  quiz_state <- start_quiz_state(db, bank)
  ids <- shiny::isolate(quiz_state$attempt$question_ids)
  db_save_answer(db, "u1", ids[[1]], "a")

  shiny::testServer(
    mod_quiz_server,
    args = list(
      quiz_state = quiz_state,
      db = db,
      bank = bank,
      settings = quiz_settings_for_test()
    ),
    {
      quiz_state$status <- "quiz"
      session$flushReact()
      expect_equal(local_rv$index, 2L)
    }
  )
})

test_that("time_up after the deadline auto-submits as a timeout", {
  db <- local_test_db()
  bank <- make_bank(5)
  quiz_state <- start_quiz_state(db, bank, time_limit = -1)

  shiny::testServer(
    mod_quiz_server,
    args = list(
      quiz_state = quiz_state,
      db = db,
      bank = bank,
      settings = quiz_settings_for_test()
    ),
    {
      quiz_state$status <- "quiz"
      session$flushReact()
      session$setInputs(time_up = 1)
      expect_equal(quiz_state$status, "results")
      expect_equal(quiz_state$attempt$submit_reason, "timeout")
      expect_equal(quiz_state$attempt$score, 0L)
    }
  )
})

test_that("time_up before the deadline does not submit", {
  db <- local_test_db()
  bank <- make_bank(5)
  quiz_state <- start_quiz_state(db, bank)

  shiny::testServer(
    mod_quiz_server,
    args = list(
      quiz_state = quiz_state,
      db = db,
      bank = bank,
      settings = quiz_settings_for_test()
    ),
    {
      quiz_state$status <- "quiz"
      session$flushReact()
      session$setInputs(time_up = 1)
      expect_equal(quiz_state$status, "quiz")
    }
  )
})

test_that("jump navigates to any question and ignores invalid targets", {
  db <- local_test_db()
  bank <- make_bank(5)
  quiz_state <- start_quiz_state(db, bank)

  shiny::testServer(
    mod_quiz_server,
    args = list(
      quiz_state = quiz_state,
      db = db,
      bank = bank,
      settings = quiz_settings_for_test()
    ),
    {
      quiz_state$status <- "quiz"
      session$flushReact()
      expect_equal(local_rv$index, 1L)

      session$setInputs(jump = 3)
      expect_equal(local_rv$index, 3L)
      session$setInputs(jump = 2)
      expect_equal(local_rv$index, 2L)

      session$setInputs(jump = 99)
      expect_equal(local_rv$index, 2L)
      session$setInputs(jump = "nope")
      expect_equal(local_rv$index, 2L)
    }
  )
})

test_that("jump after the deadline submits as a timeout", {
  db <- local_test_db()
  bank <- make_bank(5)
  quiz_state <- start_quiz_state(db, bank, time_limit = -1)

  shiny::testServer(
    mod_quiz_server,
    args = list(
      quiz_state = quiz_state,
      db = db,
      bank = bank,
      settings = quiz_settings_for_test()
    ),
    {
      quiz_state$status <- "quiz"
      session$flushReact()
      session$setInputs(jump = 2)
      expect_equal(quiz_state$status, "results")
      expect_equal(quiz_state$attempt$submit_reason, "timeout")
    }
  )
})

test_that("question_nav_state flags answered questions in order", {
  expect_equal(
    question_nav_state(c("q1", "q2", "q3"), c("q3", "q1")),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(question_nav_state(c("q1", "q2"), NULL), c(FALSE, FALSE))
})

test_that("module ui works", {
  ui <- mod_quiz_ui(id = "test")
  golem::expect_shinytaglist(ui)
  expect_true("id" %in% names(formals(mod_quiz_ui)))
})
