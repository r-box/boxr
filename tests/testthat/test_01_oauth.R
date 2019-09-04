# OAuth2.0 ----------------------------------------------------------------

context("OAuth2.0")

library("fs")
library("here")
library("conflicted")

# See if you can log in
test_that("OAuth2.0 works", {

  # these files will exist locally only on developer's computer:
  #   this test will be skipped on Travis, rhub, CRAN, etc.
  skip_if_not(
    file_exists(here(".client_id")) && file_exists(here(".client_secret")) 
  )

  # save jwt token
  token_jwt <- getOption("boxr_token_jwt")
  options(boxr_token_jwt = NULL)
  
  expect_message(
    b <-
      box_auth(
        client_id     = readLines(here(".client_id")),
        client_secret = readLines(here(".client_secret")),
        interactive = FALSE,
        cache = here(".boxr-oauth")
      ),
    "Authenticated using OAuth2 as"
  )

  expect_null(b)
  
  # put jwt token back into options
  options(
    boxr_token_jwt = token_jwt,
    boxr.token = NULL
  )
})


# JWT ---------------------------------------------------------------------

context("OAuth2.0 via JWT")

# these two tests are useful for debugging gargle and its relation to the Travis CI environment
# they are not relevant to boxr beyond that

# test_that("gargle can find secret", {
#   expect_true(gargle:::secret_can_decrypt("boxr"))
# })
# 
# test_that("gargle can read secret", {
#   json <- gargle:::secret_read("boxr", "boxr-testing.json")
#   expect_type(json, "raw")
#   expect_true(length(json) > 50)
# })


test_that("JWT works", {
  skip_if_no_token()
  expect_true(has_jwt_token())
})

