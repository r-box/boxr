library("fs")
library("here")
library("conflicted")


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

