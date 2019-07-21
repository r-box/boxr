# OAuth2.0 ----------------------------------------------------------------

context("OAuth2.0")

library("here")
library("conflicted")

# At the moment, you can only test locally
test_that("Credentials are in the local repo", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  # .gitignore'd files on in tld of local repo
  expect_true(file.exists(here(".client_id")))
  expect_true(file.exists(here(".client_secret")))
  expect_true(file.exists(here(".boxr-oauth")))
})

# See if you can log in
test_that("OAuth works", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  expect_message(
    b <-
      box_auth(
        client_id     = readLines(here(".client_id")),
        client_secret = readLines(here(".client_secret")),
        interactive = FALSE,
        cache = here(".boxr-oauth")
      ),
    "Authenticated at box.com"
  )
  
  expect_null(b)
})
