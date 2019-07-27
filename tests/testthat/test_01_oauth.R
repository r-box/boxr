# OAuth2.0 ----------------------------------------------------------------

context("OAuth2.0")

library("here")
library("conflicted")

# At the moment, you can only test locally
# test_that("Credentials are in the local repo", {
#   skip("skip")
#   skip_on_cran()
#   boxr:::skip_on_travis()
#   
#   # .gitignore'd files on in tld of local repo
#   expect_true(file.exists(here(".client_id")))
#   expect_true(file.exists(here(".client_secret")))
#   expect_true(file.exists(here(".boxr-oauth")))
# })

# See if you can log in
# test_that("OAuth works", {
#   skip('skip')
#   skip_on_cran()
#   boxr:::skip_on_travis()
#   
#   expect_message(
#     b <-
#       box_auth(
#         client_id     = readLines(here(".client_id")),
#         client_secret = readLines(here(".client_secret")),
#         interactive = FALSE,
#         cache = here(".boxr-oauth")
#       ),
#     "Authenticated at box.com"
#   )
#   
#   expect_null(b)
# })


# JWT ---------------------------------------------------------------------

context("OAuth2.0 via JWT")

# test_that("Credentials are in .Renviron", {
#   skip_if_no_token()
#   
#   expect_true(file.exists(Sys.getenv("BOX_CONFIG_FILE")))
#   expect_true(nchar(Sys.getenv("BOX_USER_ID")) > 8) # dunno about this number thing, but the IDs are long
# })

test_that("secrete can be found", {
  expect_true(gargle:::secret_can_decrypt("boxr"))
})

test_that("secrete can be read", {
  json <- gargle:::secret_read("boxr", "boxr-testing.json")
  expect_class(json, "raw")
  expect_true(length(json) > 50)
})

test_that("JWT works", {
  skip_if_no_token()
  expect_true(boxr_has_token())
})

