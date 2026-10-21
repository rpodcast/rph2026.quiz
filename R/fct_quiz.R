#' Signal a quiz bank error
#'
#' @param message Error message.
#'
#' @noRd
quiz_abort <- function(message) {
  stop(structure(
    class = c("quiz_bank_error", "error", "condition"),
    list(message = message, call = NULL)
  ))
}

#' Clean a user-entered display name for the leaderboard
#'
#' Trims surrounding whitespace, drops control characters (including newlines
#' and tabs), collapses internal whitespace runs to single spaces, and caps the
#' length. The result is safe to store and to display, and never contains line
#' breaks that would break the leaderboard layout. Parameterized SQL (not this
#' function) is what protects against injection.
#'
#' @param name A name as typed by the user (length-1 character, possibly `NA`
#'   or `NULL`).
#' @param max_chars Maximum number of characters to keep.
#'
#' @return A cleaned length-1 character string (possibly `""`).
#'
#' @noRd
sanitize_name <- function(name, max_chars = 60) {
  if (is.null(name) || length(name) != 1 || is.na(name)) {
    return("")
  }
  name <- gsub("[[:cntrl:]]", " ", as.character(name))
  name <- trimws(gsub("[[:space:]]+", " ", name))
  if (nchar(name) > max_chars) {
    name <- trimws(substr(name, 1, max_chars))
  }
  name
}

#' Load the quiz question bank
#'
#' @param path Path to the quiz bank JSON file.
#'
#' @return A list with `id`, `version`, `title`, and `questions`, where
#'   `questions` is a named list (by question id) of question lists.
#'
#' @importFrom jsonlite read_json
#' @noRd
load_quiz_bank <- function(path) {
  if (!file.exists(path)) {
    quiz_abort(paste("Quiz bank not found:", path))
  }
  bank <- jsonlite::read_json(path, simplifyVector = FALSE)
  validate_quiz_bank(bank)
  ids <- vapply(bank$questions, `[[`, character(1), "id")
  bank$questions <- stats::setNames(bank$questions, ids)
  bank
}

#' Validate the structure of a quiz bank
#'
#' @param bank A quiz bank as read from JSON (questions as unnamed list).
#' @param n_questions Optional minimum number of questions required.
#'
#' @return `bank`, invisibly. Errors with class `quiz_bank_error`.
#'
#' @noRd
validate_quiz_bank <- function(bank, n_questions = NULL) {
  questions <- bank$questions
  if (!length(questions)) {
    quiz_abort("Quiz bank has no questions.")
  }
  if (is.null(bank$version) || !nzchar(bank$version)) {
    quiz_abort("Quiz bank must have a non-empty `version`.")
  }

  ids <- vapply(
    questions,
    function(q) if (is.character(q$id)) q$id else NA_character_,
    character(1)
  )
  if (anyNA(ids)) {
    quiz_abort("Every question needs a character `id`.")
  }
  if (anyDuplicated(ids)) {
    quiz_abort(paste(
      "Duplicated question ids:",
      paste(unique(ids[duplicated(ids)]), collapse = ", ")
    ))
  }

  for (q in questions) {
    values <- option_values(q)
    if (length(values) < 2) {
      quiz_abort(paste("Question", q$id, "needs at least two options."))
    }
    if (anyDuplicated(values)) {
      quiz_abort(paste("Question", q$id, "has duplicated option values."))
    }
    if (is.null(q$prompt) || !nzchar(q$prompt)) {
      quiz_abort(paste("Question", q$id, "has no prompt."))
    }
    if (!isTRUE(q$correct %in% values)) {
      quiz_abort(paste(
        "Question",
        q$id,
        "has a `correct` value that is not one of its options."
      ))
    }
  }

  if (!is.null(n_questions) && length(questions) < n_questions) {
    quiz_abort(sprintf(
      "Quiz bank has %d questions but %d are required.",
      length(questions),
      n_questions
    ))
  }
  invisible(bank)
}

#' Option values of a question
#'
#' @param question A question list.
#'
#' @noRd
option_values <- function(question) {
  vapply(question$options, function(o) as.character(o$value), character(1))
}

#' Label of an option value
#'
#' @param question A question list.
#' @param value An option value.
#'
#' @noRd
option_label <- function(question, value) {
  if (is.null(value) || is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }
  for (o in question$options) {
    if (identical(as.character(o$value), value)) {
      return(o$label)
    }
  }
  NA_character_
}

#' Draw a random quiz from the bank
#'
#' Samples `n` questions and an independent random option order for each one,
#' so the position of the correct answer carries no information.
#'
#' @param bank A quiz bank from `load_quiz_bank()`.
#' @param n Number of questions to draw.
#'
#' @return A list with `question_ids` (character) and `option_orders` (named
#'   list of character vectors of option values in display order).
#'
#' @noRd
draw_quiz <- function(bank, n) {
  ids <- names(bank$questions)
  if (length(ids) < n) {
    quiz_abort(sprintf(
      "Cannot draw %d questions from a bank of %d.",
      n,
      length(ids)
    ))
  }
  question_ids <- ids[sample.int(length(ids), n)]
  option_orders <- lapply(question_ids, function(id) {
    values <- option_values(bank$questions[[id]])
    values[sample.int(length(values))]
  })
  list(
    question_ids = question_ids,
    option_orders = stats::setNames(option_orders, question_ids)
  )
}

#' Encode / decode an answer choice for the radio input
#'
#' Choice values carry the question id so a stale input event can never be
#' saved against the wrong question.
#'
#' @param question_id Question id.
#' @param value Option value.
#' @param choice Encoded choice string.
#'
#' @noRd
encode_choice <- function(question_id, value) {
  paste0(question_id, "::", value)
}

#' @rdname encode_choice
#' @noRd
decode_choice <- function(choice) {
  if (is.null(choice) || length(choice) != 1 || !grepl("::", choice)) {
    return(NULL)
  }
  parts <- strsplit(choice, "::", fixed = TRUE)[[1]]
  if (length(parts) != 2) {
    return(NULL)
  }
  list(question_id = parts[[1]], value = parts[[2]])
}

#' Radio choices for a question in its attempt-specific order
#'
#' @param question A question list.
#' @param order Character vector of option values in display order.
#'
#' @return Named character vector (names are labels) for `radioButtons()`.
#'
#' @noRd
question_choices <- function(question, order) {
  labels <- vapply(order, function(v) option_label(question, v), character(1))
  stats::setNames(encode_choice(question$id, order), labels)
}

#' Whether an answer is acceptable for an attempt
#'
#' @param bank A quiz bank.
#' @param attempt An attempt from `db_get_attempt()`.
#' @param question_id,value Answer to check.
#'
#' @noRd
is_valid_answer <- function(bank, attempt, question_id, value) {
  question_id %in%
    attempt$question_ids &&
    value %in% option_values(bank$questions[[question_id]])
}

#' Score a set of answers
#'
#' @param bank A quiz bank.
#' @param question_ids Question ids in the attempt, in order.
#' @param answers Named list or character vector of option values, by
#'   question id. Missing questions count as incorrect.
#'
#' @return A data frame with one row per question.
#'
#' @noRd
score_answers <- function(bank, question_ids, answers) {
  rows <- lapply(seq_along(question_ids), function(i) {
    id <- question_ids[[i]]
    q <- bank$questions[[id]]
    given <- answers[[id]]
    given <- if (is.null(given)) NA_character_ else as.character(given)
    data.frame(
      position = i,
      question_id = id,
      prompt = q$prompt,
      category = q$category %||% NA_character_,
      answer = given,
      answer_label = option_label(q, given),
      correct = q$correct,
      correct_label = option_label(q, q$correct),
      is_correct = isTRUE(identical(given, q$correct)),
      explanation = q$explanation %||% NA_character_,
      source = format_source(q$source),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Format a question's source metadata
#'
#' @param source The `source` list of a question.
#'
#' @noRd
format_source <- function(source) {
  if (is.null(source)) {
    return(NA_character_)
  }
  parts <- c(source$speaker, source$talk, source$year)
  paste(parts[!vapply(parts, is.null, logical(1))], collapse = " \u00b7 ")
}

#' Seconds remaining before a deadline
#'
#' @param deadline Deadline in epoch seconds.
#' @param now Current time in epoch seconds.
#'
#' @noRd
seconds_remaining <- function(deadline, now = epoch_now()) {
  max(0, deadline - now)
}

#' Current time in epoch seconds
#'
#' @noRd
epoch_now <- function() {
  as.numeric(Sys.time())
}

#' Format seconds as M:SS
#'
#' @param seconds Number of seconds.
#'
#' @noRd
format_duration <- function(seconds) {
  seconds <- max(0, round(seconds))
  sprintf("%d:%02d", seconds %/% 60, seconds %% 60)
}
