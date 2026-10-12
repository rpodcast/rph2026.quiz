sign_token <- function(key, claims) {
  jose::jwt_encode_sig(do.call(jose::jwt_claim, claims), key)
}

test_that("a valid token for this content resolves to the user", {
  skip_if_not_installed("openssl")
  key <- openssl::rsa_keygen()
  token <- sign_token(
    key,
    list(
      sub = "abc",
      name = "Grace Hopper",
      email = "g@x.org",
      aud = "content-1"
    )
  )
  user <- decode_ricochet_user(token, key$pubkey, "content-1")
  expect_equal(user$sub, "abc")
  expect_equal(user$name, "Grace Hopper")
})

test_that("invalid tokens are rejected", {
  skip_if_not_installed("openssl")
  key <- openssl::rsa_keygen()
  other <- openssl::rsa_keygen()
  claims <- list(sub = "abc", name = "G", aud = "content-1")

  wrong_aud <- sign_token(key, utils::modifyList(claims, list(aud = "other")))
  expect_null(decode_ricochet_user(wrong_aud, key$pubkey, "content-1"))

  forged <- sign_token(other, claims)
  expect_null(decode_ricochet_user(forged, key$pubkey, "content-1"))

  expired <- sign_token(key, c(claims, exp = as.numeric(Sys.time()) - 3600))
  expect_null(decode_ricochet_user(expired, key$pubkey, "content-1"))

  expect_null(decode_ricochet_user(NULL, key$pubkey, "content-1"))
  expect_null(decode_ricochet_user("", key$pubkey, "content-1"))
  expect_null(decode_ricochet_user(sign_token(key, claims), NULL, "content-1"))
  expect_null(decode_ricochet_user(sign_token(key, claims), key$pubkey, ""))
})

test_that("resolve_user mocks in dev and requires a token in ricochet mode", {
  session <- list(request = list(HTTP_X_RICOCHET_USER = NULL))
  expect_equal(resolve_user(session, "mock"), mock_user())
  expect_null(resolve_user(
    session,
    "ricochet",
    pubkey = NULL,
    content_id = "x"
  ))
})
