#' Mock user for development
#'
#' @noRd
mock_user <- function() {
  list(
    sub = "dev-test-user",
    name = "Test User",
    email = "test.user@example.com"
  )
}

#' Read the Ricochet JWT signing key
#'
#' @param ricochet_url Base URL of the Ricochet server.
#'
#' @return A public key, or `NULL` if it cannot be read.
#'
#' @importFrom jose read_jwk
#' @importFrom jsonlite fromJSON
#' @noRd
read_ricochet_pubkey <- function(ricochet_url) {
  tryCatch(
    {
      jwks <- jsonlite::fromJSON(
        file.path(ricochet_url, ".well-known", "jwks.json"),
        simplifyVector = FALSE
      )
      jose::read_jwk(jwks$keys[[1]])
    },
    error = function(cnd) {
      message("auth: could not read Ricochet JWKS: ", conditionMessage(cnd))
      NULL
    }
  )
}

#' Decode and verify a Ricochet user token
#'
#' Verifies signature and expiry, then checks that the audience matches this
#' content id.
#'
#' @param token The `X-Ricochet-User` header value.
#' @param pubkey Public key from `read_ricochet_pubkey()`.
#' @param content_id This content's Ricochet id.
#'
#' @return A user list (`sub`, `name`, `email`) or `NULL` when invalid.
#'
#' @importFrom jose jwt_decode_sig
#' @noRd
decode_ricochet_user <- function(token, pubkey, content_id) {
  if (is.null(pubkey) || is.null(token) || !nzchar(token)) {
    return(NULL)
  }
  if (is.null(content_id) || !nzchar(content_id)) {
    return(NULL)
  }
  claims <- tryCatch(
    jose::jwt_decode_sig(token, pubkey),
    error = function(cnd) NULL
  )
  if (is.null(claims) || !identical(claims$aud, content_id)) {
    return(NULL)
  }
  if (is.null(claims$sub) || !nzchar(claims$sub)) {
    return(NULL)
  }
  list(
    sub = claims$sub,
    name = claims$name %||% claims$email %||% claims$sub,
    email = claims$email %||% NA_character_
  )
}

#' Resolve the current user for a Shiny session
#'
#' @param session Shiny session.
#' @param auth_mode `"mock"` (development) or `"ricochet"` (production).
#' @param pubkey Ricochet public key (ricochet mode only).
#' @param content_id Ricochet content id (ricochet mode only).
#'
#' @return A user list, or `NULL` when no verified user is available.
#'
#' @noRd
resolve_user <- function(
  session,
  auth_mode = c("mock", "ricochet"),
  pubkey = NULL,
  content_id = Sys.getenv("RICOCHET_CONTENT_ID")
) {
  auth_mode <- match.arg(auth_mode)
  if (identical(auth_mode, "mock")) {
    return(mock_user())
  }
  decode_ricochet_user(
    session$request$HTTP_X_RICOCHET_USER,
    pubkey,
    content_id
  )
}

#' Ricochet public key, cached per R process
#'
#' @noRd
ricochet_pubkey <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- read_ricochet_pubkey(
        Sys.getenv("RICOCHET_URL", "http://localhost:6188")
      )
    }
    cache
  }
})
