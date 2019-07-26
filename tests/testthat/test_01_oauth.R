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

if (gargle:::secret_can_decrypt("boxr")) {
  json <- gargle:::secret_read("boxr", "boxr-testing.json")
  box_auth_jwt(user_id = "6513355836", config_file = rawToChar(json))
}

test_that("Credentials are in .Renviron", {
  skip_on_cran()
  skip_if_no_token()
  
  expect_true(file.exists(Sys.getenv("BOX_CONFIG_FILE")))
  expect_true(nchar(Sys.getenv("BOX_USER_ID")) > 8) # dunno about this number thing, but the IDs are long
})

test_that("JWT works", {
  skip_if_no_token()
  expect_message(
    box_auth_jwt(user_id = Sys.getenv("BOX_USER_ID"),
                 config_file = Sys.getenv("BOX_CONFIG_FILE")),
    "Authenticated at box.com"
  )
})

