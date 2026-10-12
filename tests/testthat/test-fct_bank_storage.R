test_that("parse_s3_uri splits bucket and key", {
  expect_equal(
    parse_s3_uri("s3://my-bucket/path/to/bank.json"),
    list(bucket = "my-bucket", key = "path/to/bank.json")
  )
  expect_null(parse_s3_uri("quiz/dev_quiz.json"))
  expect_error(parse_s3_uri("s3://my-bucket"), class = "quiz_bank_error")
  expect_error(parse_s3_uri("s3:///key.json"), class = "quiz_bank_error")
})

test_that("bank_s3_config normalizes endpoint and derives region", {
  withr::local_envvar(
    AWS_ACCESS_KEY = "id",
    AWS_SECRET_ACCESS_KEY = "secret",
    AWS_ENDPOINT = "us-ord-1.linodeobjects.com",
    AWS_REGION = NA
  )
  config <- bank_s3_config()
  expect_equal(config$endpoint, "https://us-ord-1.linodeobjects.com")
  expect_equal(config$region, "us-ord-1")

  withr::local_envvar(
    AWS_ENDPOINT = "https://example.com",
    AWS_REGION = "eu-1"
  )
  config <- bank_s3_config()
  expect_equal(config$endpoint, "https://example.com")
  expect_equal(config$region, "eu-1")
})

test_that("bank_s3_config errors when credentials are missing", {
  withr::local_envvar(
    AWS_ACCESS_KEY = NA,
    AWS_SECRET_ACCESS_KEY = "secret",
    AWS_ENDPOINT = "us-ord-1.linodeobjects.com"
  )
  expect_error(bank_s3_config(), class = "quiz_bank_error")
})

test_that("read_quiz_bank reads a local bank under inst/app", {
  bank <- read_quiz_bank("quiz/dev_quiz.json")
  expect_length(bank$questions, 50)
})

test_that("bad credentials give a quiz_bank_error", {
  skip_on_cran()
  skip_if_offline("us-ord-1.linodeobjects.com")
  withr::local_envvar(
    AWS_ACCESS_KEY = "not-a-real-key",
    AWS_SECRET_ACCESS_KEY = "not-a-real-secret",
    AWS_ENDPOINT = "us-ord-1.linodeobjects.com",
    AWS_REGION = NA
  )
  expect_error(
    read_quiz_bank("s3://rpodcast-misc/rpharma_quiz.json"),
    class = "quiz_bank_error"
  )
})

test_that("the production bank downloads and validates", {
  skip_on_cran()
  skip_if(
    !nzchar(Sys.getenv("AWS_ACCESS_KEY")),
    "Object storage credentials not set"
  )
  location <- withr::with_envvar(
    c(GOLEM_CONFIG_ACTIVE = "production"),
    quiz_settings()$quiz_bank
  )
  bank <- read_quiz_bank(location)
  expect_no_error(validate_quiz_bank(bank, n_questions = 15))
})
