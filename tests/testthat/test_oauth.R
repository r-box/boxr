library(boxr)
context("OAuth2.0")

# Imported from the dev build of testthat
# https://github.com/hadley/testthat/blob/0835a9e40d3a2fbaac47cbe8f86239e231623b51/R/utils.r
skip_on_travis <- function() {
  if (!identical(Sys.getenv("TRAVIS"), "true")) return()
  
  skip("On Travis")
}


# At the moment, you can only test locally
test_that("Credentials are in the local repo", {
  skip_on_cran()
  skip_on_travis()
  
  # .gitignore'd files on in tld of local repo
  expect_true(file.exists("../../.client_id"))
  expect_true(file.exists("../../.client_secret"))
  expect_true(file.exists("../../.boxr-oauth"))
})

# See if you can log in
test_that("OAuth works", {
  skip_on_cran()
  skip_on_travis()
  
  b <-
    box_auth(
      client_id     = readLines("../../.client_id"),
      client_secret = readLines("../../.client_secret"),
      interactive = FALSE,
      cache = "../../.boxr-oauth",
      write.Renv = FALSE
    )
  
  expect_true(b)
})


# Make sure the remote directory in the test account is clear
test_that("Clear out the remote directory", {
  skip_on_cran()
  skip_on_travis()
  
  # You can't git an empty dir, so create one
  dummy_dir <- "test_dir/dir_13"
  
  d <- dir.create(dummy_dir)
  
  # Tell boxr to synch the remote home directory with an empty local one
  # (i.e. delete everything)
  b <- box_push(0, dummy_dir, delete = TRUE)
  
  d <- unlink(dummy_dir, force = TRUE, recursive = TRUE)
})


# 
# 
# bf <- box_fetch(0, "test_dir")
# bp <- box_push(0, "test_dir")
# 
# uldf <- boxr:::uploadDirFiles(3110293969, "tests/testthat/test_dir/delete_me")
# dldf <- boxr:::downloadDirFiles(0, "tests/testthat/test_dir")
# 
# boxr:::box_dir_diff(0, "tests/testthat/test_dir")

