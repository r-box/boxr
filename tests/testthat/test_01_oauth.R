library("fs")
library("here")
library("conflicted")


# JWT ---------------------------------------------------------------------

context("OAuth2.0 via JWT")

# these two tests are useful for debugging gargle and its relation to the Travis CI environment
# they are not relevant to boxr beyond that

# path_secret <- system.file("secret", "boxr-testing.json", package = "boxr")
# env_secret <- "BOXR_KEY"
#
# test_that("gargle can find secret", {
#   expect_true(gargle::secret_has_key(env_secret))
# })
# 
# test_that("gargle can read secret", {
#   text <- gargle::secret_decrypt_json(path_secret, env_secret)
#   expect_true(inherits("x", "character")) 
#   expect_true(nchar(text) > 50)
# })


test_that("JWT works", {
  skip_if_no_token()
  expect_true(has_jwt_token())
})

