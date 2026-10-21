test_that("sanitize_name trims, collapses whitespace, and caps length", {
  expect_equal(sanitize_name("  Ada  Lovelace  "), "Ada Lovelace")
  expect_equal(sanitize_name("Ada\tLovelace\n"), "Ada Lovelace")
  expect_equal(sanitize_name("line\r\nbreak"), "line break")
  expect_equal(sanitize_name(NA_character_), "")
  expect_equal(sanitize_name(NULL), "")
  expect_equal(sanitize_name(""), "")
  expect_equal(nchar(sanitize_name(strrep("a", 100))), 60)
  expect_equal(nchar(sanitize_name(strrep("a", 100), max_chars = 10)), 10)
})

test_that("the shipped dev bank loads and has enough questions", {
  bank <- load_quiz_bank(app_sys("app", "quiz", "dev_quiz.json"))
  expect_length(bank$questions, 50)
  expect_no_error(validate_quiz_bank(bank, n_questions = 15))
})

test_that("draw_quiz returns unique questions with shuffled full option sets", {
  bank <- make_bank(30)
  draw <- withr::with_seed(5821, draw_quiz(bank, 20))
  expect_length(draw$question_ids, 20)
  expect_false(anyDuplicated(draw$question_ids) > 0)
  expect_named(draw$option_orders, draw$question_ids)
  for (order in draw$option_orders) {
    expect_setequal(order, c("a", "b", "c"))
  }
  again <- withr::with_seed(5821, draw_quiz(bank, 20))
  expect_identical(draw, again)
})

test_that("draw_quiz draws exactly half hard questions", {
  bank <- make_bank(30)
  draw <- withr::with_seed(4412, draw_quiz(bank, 20))
  drawn_difficulty <- vapply(
    draw$question_ids,
    function(id) bank$questions[[id]]$difficulty,
    character(1)
  )
  expect_equal(sum(drawn_difficulty == "hard"), 10)
  expect_equal(sum(drawn_difficulty != "hard"), 10)
})

test_that("draw_quiz handles an odd n by favoring non-hard questions", {
  bank <- make_bank(30)
  draw <- withr::with_seed(4412, draw_quiz(bank, 15))
  drawn_difficulty <- vapply(
    draw$question_ids,
    function(id) bank$questions[[id]]$difficulty,
    character(1)
  )
  expect_equal(sum(drawn_difficulty == "hard"), 7)
  expect_equal(sum(drawn_difficulty != "hard"), 8)
})

test_that("option shuffling moves the correct answer off position 1", {
  bank <- make_bank(60)
  draw <- withr::with_seed(907, draw_quiz(bank, 40))
  first <- vapply(draw$option_orders, `[[`, character(1), 1)
  expect_true(any(first != "a"))
})

test_that("draw_quiz errors when the bank is too small", {
  expect_error(draw_quiz(make_bank(3), 5), class = "quiz_bank_error")
})

test_that("draw_quiz errors when there are too few hard questions", {
  bank <- make_bank(20, difficulties = c("easy", "medium"))
  expect_error(draw_quiz(bank, 10), class = "quiz_bank_error")
})

test_that("validate_quiz_bank catches malformed banks", {
  bank <- make_bank(3)
  bank$questions <- unname(bank$questions)

  bad_correct <- bank
  bad_correct$questions[[1]]$correct <- "z"
  expect_error(validate_quiz_bank(bad_correct), class = "quiz_bank_error")

  dup <- bank
  dup$questions[[2]]$id <- dup$questions[[1]]$id
  expect_error(validate_quiz_bank(dup), class = "quiz_bank_error")

  expect_error(
    validate_quiz_bank(bank, n_questions = 10),
    class = "quiz_bank_error"
  )
})

test_that("score_answers counts correct, wrong, and missing answers", {
  bank <- make_bank(3)
  scored <- score_answers(
    bank,
    c("t01", "t02", "t03"),
    list(t01 = "a", t02 = "b")
  )
  expect_equal(scored$is_correct, c(TRUE, FALSE, FALSE))
  expect_true(is.na(scored$answer[[3]]))
  expect_equal(scored$correct_label[[2]], "Right 2")
  expect_equal(scored$answer_label[[2]], "Wrong B 2")
})

test_that("choices encode the question id and round-trip", {
  bank <- make_bank(1)
  choices <- question_choices(bank$questions$t01, c("c", "a", "b"))
  expect_equal(names(choices), c("Wrong C 1", "Right 1", "Wrong B 1"))
  expect_equal(
    decode_choice(choices[[2]]),
    list(question_id = "t01", value = "a")
  )
  expect_null(decode_choice("garbage"))
  expect_null(decode_choice(NULL))
})

test_that("time helpers behave at the boundaries", {
  expect_equal(seconds_remaining(100, now = 40), 60)
  expect_equal(seconds_remaining(100, now = 140), 0)
  expect_equal(format_duration(65.4), "1:05")
  expect_equal(format_duration(-3), "0:00")
})
