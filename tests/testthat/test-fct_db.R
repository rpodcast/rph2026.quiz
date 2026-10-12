test_that("an attempt is created once and reused", {
  db <- local_test_db()
  bank <- make_bank(10)
  first <- db_get_or_start_attempt(db, test_user(), bank, 5, 600, now = 1000)
  second <- db_get_or_start_attempt(db, test_user(), bank, 5, 600, now = 2000)
  expect_identical(first$question_ids, second$question_ids)
  expect_identical(first$option_orders, second$option_orders)
  expect_equal(second$started_at, 1000)
  expect_equal(second$deadline_at, 1600)
  expect_length(first$question_ids, 5)
})

test_that("answers upsert and late answers are rejected", {
  db <- local_test_db()
  bank <- make_bank(10)
  attempt <- db_get_or_start_attempt(db, test_user(), bank, 5, 600, now = 1000)
  q <- attempt$question_ids[[1]]

  expect_true(db_save_answer(db, "u1", q, "b", now = 1100))
  expect_true(db_save_answer(db, "u1", q, "a", now = 1200))
  expect_equal(db_get_answers(db, "u1"), stats::setNames(list("a"), q))

  expect_true(db_save_answer(db, "u1", q, "c", now = 1603, grace_seconds = 5))
  expect_false(db_save_answer(db, "u1", q, "b", now = 1700, grace_seconds = 5))
  expect_false(db_save_answer(db, "nobody", q, "a", now = 1100))
})

test_that("finalize scores once and locks the attempt", {
  db <- local_test_db()
  bank <- make_bank(10)
  attempt <- db_get_or_start_attempt(db, test_user(), bank, 3, 600, now = 1000)
  ids <- attempt$question_ids
  db_save_answer(db, "u1", ids[[1]], "a", now = 1010)
  db_save_answer(db, "u1", ids[[2]], "b", now = 1020)

  expect_true(db_finalize_attempt(db, bank, "u1", "finished", now = 1100))
  expect_false(db_finalize_attempt(db, bank, "u1", "timeout", now = 1200))

  done <- db_get_attempt(db, "u1")
  expect_equal(done$score, 1L)
  expect_equal(done$submit_reason, "finished")
  expect_equal(done$duration_seconds, 100)
  expect_false(db_save_answer(db, "u1", ids[[3]], "a", now = 1110))
})

test_that("timeout duration is capped at the time limit", {
  db <- local_test_db()
  bank <- make_bank(5)
  db_get_or_start_attempt(db, test_user(), bank, 3, 60, now = 0)
  db_finalize_attempt(db, bank, "u1", "timeout", now = 500)
  expect_equal(db_get_attempt(db, "u1")$duration_seconds, 60)
})

test_that("leaderboard ranks by score then time", {
  db <- local_test_db()
  bank <- make_bank(5)
  play <- function(id, n_right, finish_at) {
    a <- db_get_or_start_attempt(db, test_user(id, id), bank, 3, 600, now = 0)
    for (q in utils::head(a$question_ids, n_right)) {
      db_save_answer(db, id, q, "a", now = 1)
    }
    db_finalize_attempt(db, bank, id, "finished", now = finish_at)
  }
  play("slow_two", 2, 300)
  play("fast_two", 2, 100)
  play("three", 3, 500)
  db_get_or_start_attempt(db, test_user("unfinished"), bank, 3, 600, now = 0)

  board <- db_leaderboard(db, 10)
  expect_equal(board$user_id, c("three", "fast_two", "slow_two"))
  expect_equal(db_user_rank(db, "fast_two"), list(rank = 2L, total = 3L))
  expect_null(db_user_rank(db, "unfinished"))
})

test_that("the pooled SQLite backend used by the app works", {
  path <- withr::local_tempfile(fileext = ".sqlite3")
  db <- db_connect("sqlite", path)
  withr::defer(db_close(db))
  expect_s3_class(db$pool, "Pool")
  db_create_tables(db)
  attempt <- db_get_or_start_attempt(db, test_user(), make_bank(5), 3, 600)
  expect_true(db_save_answer(db, "u1", attempt$question_ids[[1]], "a"))
  expect_equal(
    db_query(db, "PRAGMA journal_mode")$journal_mode,
    "wal"
  )
})

test_that("postgres params come from PG_* variables", {
  withr::local_envvar(
    PG_HOST = "db.example.com",
    PG_PORT = "5432",
    PG_DATABASE = "quiz",
    PG_USER = "me",
    PG_PASSWORD = "secret",
    PG_SSL_MODE = "enabled"
  )
  params <- db_pg_params()
  expect_equal(params$host, "db.example.com")
  expect_identical(params$port, 5432L)
  expect_equal(params$sslmode, "require")

  withr::local_envvar(PG_SSL_MODE = "verify-full")
  expect_equal(db_pg_params()$sslmode, "verify-full")
  withr::local_envvar(PG_SSL_MODE = "sometimes")
  expect_error(db_pg_params())
  withr::local_envvar(PG_SSL_MODE = NA, PG_HOST = "postgres://me:pw@h:1/db")
  err <- expect_error(db_pg_params(), "bare host name")
  expect_false(grepl("pw", conditionMessage(err)))
  withr::local_envvar(PG_HOST = NA)
  expect_error(db_pg_params(), "PG_HOST")
})

test_that("the full quiz flow works on Postgres", {
  # Opt-in: talks to the real database, inside a throwaway schema
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("QUIZ_TEST_POSTGRES"), "true"),
    "Set QUIZ_TEST_POSTGRES=true to run Postgres tests"
  )
  schema <- paste0("quiz_test_", as.integer(Sys.time()), sample.int(1e5, 1))
  params <- db_pg_params()
  admin <- db_connect("postgres", use_pool = FALSE, pg_params = params)
  withr::defer(db_close(admin))
  db_execute(admin, paste("CREATE SCHEMA", schema))
  withr::defer(db_execute(admin, paste("DROP SCHEMA", schema, "CASCADE")))

  params$options <- paste0("-c search_path=", schema)
  db <- db_connect("postgres", pg_params = params)
  withr::defer(db_close(db))
  expect_s3_class(db$pool, "Pool")
  db_create_tables(db)
  db_create_tables(db)

  bank <- make_bank(5)
  play <- function(id, n_right, finish_at) {
    a <- db_get_or_start_attempt(db, test_user(id, id), bank, 3, 600, now = 0)
    for (q in utils::head(a$question_ids, n_right)) {
      expect_true(db_save_answer(db, id, q, "a", now = 1))
    }
    db_finalize_attempt(db, bank, id, "finished", now = finish_at)
  }
  play("slow_two", 2, 300)
  play("fast_two", 2, 100)
  play("three", 3, 500)
  again <- db_get_or_start_attempt(db, test_user("three"), bank, 3, 600)
  expect_equal(again$score, 3L)
  expect_false(db_save_answer(db, "three", again$question_ids[[1]], "b"))
  expect_false(db_finalize_attempt(db, bank, "three", "timeout"))

  board <- db_leaderboard(db, 10)
  expect_equal(board$user_id, c("three", "fast_two", "slow_two"))
  expect_equal(db_user_rank(db, "fast_two"), list(rank = 2L, total = 3L))
})

test_that("postgres placeholders are numbered", {
  pg <- list(backend = "postgres")
  expect_equal(db_sql(pg, "a = ? AND b = ?"), "a = $1 AND b = $2")
  expect_equal(db_sql(list(backend = "sqlite"), "a = ?"), "a = ?")
})
