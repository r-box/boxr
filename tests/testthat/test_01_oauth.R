
# OAuth2.0 ----------------------------------------------------------------

context("OAuth2.0")
# At the moment, you can only test locally
test_that("Credentials are in the local repo", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  # .gitignore'd files on in tld of local repo
  expect_true(file.exists("../../.client_id"))
  expect_true(file.exists("../../.client_secret"))
  expect_true(file.exists("../../.boxr-oauth"))
})

# See if you can log in
test_that("OAuth works", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  expect_message(
    b <-
      box_auth(
        client_id     = readLines("../../.client_id"),
        client_secret = readLines("../../.client_secret"),
        interactive = FALSE,
        cache = "../../.boxr-oauth",
        write.Renv = FALSE
      ),
    "Authenticated at box.com"
  )
  
  expect_true(b)
})
