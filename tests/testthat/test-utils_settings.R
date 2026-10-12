test_that("default config gives dev settings", {
  withr::local_envvar(GOLEM_CONFIG_ACTIVE = "default")
  settings <- quiz_settings()
  expect_equal(settings$quiz_n_questions, 15)
  expect_equal(settings$auth_mode, "mock")
  expect_equal(settings$db_backend, "sqlite")
})

test_that("production config uses ricochet auth and postgres", {
  withr::local_envvar(GOLEM_CONFIG_ACTIVE = "production")
  settings <- quiz_settings()
  expect_equal(settings$auth_mode, "ricochet")
  expect_equal(settings$db_backend, "postgres")
  expect_equal(settings$quiz_n_questions, 15)
  expect_false(is.null(parse_s3_uri(settings$quiz_bank)))
})

test_that("local_production config uses prod data sources with mock auth", {
  withr::local_envvar(GOLEM_CONFIG_ACTIVE = "local_production")
  settings <- quiz_settings()
  expect_equal(settings$auth_mode, "mock")
  expect_equal(settings$db_backend, "postgres")
  expect_false(is.null(parse_s3_uri(settings$quiz_bank)))
})

test_that("invalid settings error", {
  good <- list(
    quiz_n_questions = 15,
    quiz_time_limit_minutes = 10,
    leaderboard_size = 10,
    quiz_grace_seconds = 5,
    auth_mode = "mock",
    db_backend = "sqlite"
  )
  expect_no_error(validate_settings(good))
  expect_error(validate_settings(utils::modifyList(
    good,
    list(quiz_time_limit_minutes = 0)
  )))
  expect_error(validate_settings(utils::modifyList(
    good,
    list(auth_mode = "none")
  )))
  expect_error(validate_settings(utils::modifyList(
    good,
    list(db_backend = "mysql")
  )))
})
