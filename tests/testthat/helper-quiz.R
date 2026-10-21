make_bank <- function(n = 5, difficulties = c("easy", "medium", "hard")) {
  levels <- rep_len(difficulties, n)
  questions <- lapply(seq_len(n), function(i) {
    list(
      id = sprintf("t%02d", i),
      prompt = paste("Question", i),
      category = "Testing",
      difficulty = levels[[i]],
      options = list(
        list(value = "a", label = paste("Right", i)),
        list(value = "b", label = paste("Wrong B", i)),
        list(value = "c", label = paste("Wrong C", i))
      ),
      correct = "a",
      explanation = paste("Because", i),
      source = list(year = 2024, speaker = "Speaker", talk = "Talk")
    )
  })
  bank <- list(
    id = "test-quiz",
    version = "1",
    title = "Test",
    questions = questions
  )
  bank$questions <- stats::setNames(
    bank$questions,
    vapply(questions, `[[`, character(1), "id")
  )
  bank
}

local_test_db <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite3", .local_envir = env)
  # No pool: its later tasks make testServer() output reads hang
  db <- db_connect("sqlite", path, use_pool = FALSE)
  withr::defer(db_close(db), envir = env)
  db_create_tables(db)
  db
}

test_user <- function(id = "u1", name = "Ada Lovelace") {
  list(sub = id, name = name, email = paste0(id, "@example.com"))
}
