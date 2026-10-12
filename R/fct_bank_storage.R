#' Read the quiz bank from a local path or object storage
#'
#' `location` is either a path relative to `inst/app` or an
#' `s3://bucket/key` URI. S3 objects are fetched from an S3-compatible
#' endpoint (e.g. Linode Object Storage) with credentials from environment
#' variables; see `bank_s3_config()`.
#'
#' @param location Value of the `quiz_bank` setting.
#'
#' @return A quiz bank from `load_quiz_bank()`.
#'
#' @noRd
read_quiz_bank <- function(location) {
  uri <- parse_s3_uri(location)
  if (is.null(uri)) {
    return(load_quiz_bank(app_sys("app", location)))
  }
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  download_s3_object(uri$bucket, uri$key, path, bank_s3_config())
  load_quiz_bank(path)
}

#' Parse an `s3://bucket/key` URI
#'
#' @param x A string.
#'
#' @return A list with `bucket` and `key`, or `NULL` if `x` is not an S3 URI.
#'
#' @noRd
parse_s3_uri <- function(x) {
  if (!is.character(x) || length(x) != 1 || !startsWith(x, "s3://")) {
    return(NULL)
  }
  rest <- sub("^s3://", "", x)
  bucket <- sub("/.*$", "", rest)
  key <- if (grepl("/", rest)) sub("^[^/]*/", "", rest) else ""
  if (!nzchar(bucket) || !nzchar(key)) {
    quiz_abort(paste("Invalid S3 URI (expected s3://bucket/key):", x))
  }
  list(bucket = bucket, key = key)
}

#' Object storage settings from environment variables
#'
#' Reads `AWS_ACCESS_KEY`, `AWS_SECRET_ACCESS_KEY`, `AWS_ENDPOINT` (host such
#' as `us-ord-1.linodeobjects.com`, with or without `https://`), and optionally
#' `AWS_REGION`. When `AWS_REGION` is unset, the region is the first label of
#' the endpoint host (`us-ord-1` for Linode).
#'
#' @return A list with `access_key`, `secret_key`, `endpoint`, and `region`.
#'
#' @noRd
bank_s3_config <- function() {
  vars <- c("AWS_ACCESS_KEY", "AWS_SECRET_ACCESS_KEY", "AWS_ENDPOINT")
  values <- Sys.getenv(vars)
  missing <- vars[!nzchar(values)]
  if (length(missing)) {
    quiz_abort(paste(
      "Cannot read the quiz bank from object storage. Missing environment",
      "variables:",
      paste(missing, collapse = ", ")
    ))
  }
  endpoint <- values[["AWS_ENDPOINT"]]
  if (!grepl("^https?://", endpoint)) {
    endpoint <- paste0("https://", endpoint)
  }
  host <- sub("^https?://", "", endpoint)
  region <- Sys.getenv("AWS_REGION")
  if (!nzchar(region)) {
    region <- sub("\\..*$", "", host)
  }
  list(
    access_key = values[["AWS_ACCESS_KEY"]],
    secret_key = values[["AWS_SECRET_ACCESS_KEY"]],
    endpoint = endpoint,
    region = region
  )
}

#' Download an object from S3-compatible storage
#'
#' @param bucket,key Object location.
#' @param path Local file to write.
#' @param config Settings from `bank_s3_config()`.
#'
#' @return `path`, invisibly. Errors with class `quiz_bank_error`.
#'
#' @noRd
download_s3_object <- function(bucket, key, path, config) {
  client <- paws.storage::s3(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = config$access_key,
          secret_access_key = config$secret_key
        )
      ),
      endpoint = config$endpoint,
      region = config$region
    )
  )
  body <- tryCatch(
    client$get_object(Bucket = bucket, Key = key)$Body,
    error = function(e) {
      quiz_abort(sprintf(
        "Could not download s3://%s/%s from %s: %s",
        bucket,
        key,
        config$endpoint,
        conditionMessage(e)
      ))
    }
  )
  writeBin(body, path)
  invisible(path)
}
