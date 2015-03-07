library(boxr)

# Yoinked from the dev build of testthat
# https://github.com/hadley/testthat/blob/0835a9e40d3a2fbaac47cbe8f86239e231623b51/R/utils.r
skip_on_travis <- function() {
  if (!identical(Sys.getenv("TRAVIS"), "true")) return()
  
  skip("On Travis")
}

# For passing vars (e.g. file_id's) between testing environments
test_vars <- new.env()
options(boxr.verbose = FALSE)

# OAuth2.0 ----------------------------------------------------------------

context("OAuth2.0")
# At the moment, you can only test locally
test_that("Credentials are in the local repo", {
  skip_on_cran()
  skip_on_travis()
  
  writeLines(getwd(), "~/output.txt")
  
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



# Clear out (& box_push()) ------------------------------------------------
context("Clear out (& box_push())")
# Make sure the remote directory in the test account is clear
test_that("Clear out the remote directory", {
  skip_on_cran()
  skip_on_travis()
  
  # You can't git an empty dir, so create one
  dummy_dir <- "test_dir/dir_13"
  
  d <- dir.create(dummy_dir, showWarnings = FALSE)
  
  # Tell boxr to synch the remote home directory with an empty local one
  # (i.e. delete everything)
  b <- box_push(0, dummy_dir, delete = TRUE)
  
  d <- unlink(dummy_dir, force = TRUE, recursive = TRUE)
})


# Upload/download ---------------------------------------------------------
context("Upload/download")

# Write a test file to upload
writeLines("Completely Original File\n", "test_dir/testfile.txt")

test_that("Uploading a new file", {
  skip_on_cran()
  skip_on_travis()
  
  # This dir_id doesn't exist
  expect_error(box_ul(1, "test_dir/testfile.txt"))
  # This file to upload doesn't exist
  expect_error(box_ul(0, "test_dir/ololol.txt"))
  
  b <- box_ul(0, "test_dir/testfile.txt")
  
  test_vars$file_id <- b$entries[[1]]$id
  
})

test_that("Downloading a file", {
  skip_on_cran()
  skip_on_travis()
  
  # Overwrite default to FALSE, local copy present
  expect_error(box_dl(test_vars$file_id, "test_dir"))
  # This file_id doesn't exist
  expect_error(box_dl(1, "test_dir"))
  # This directory doesn't exist
  expect_error(box_dl(test_vars$file_id, "ololol"))
  # Overwrite should be bool
  expect_error(box_dl(test_vars$file_id, "test_dir", overwrite = "ololol"))
  
  # Should work if overwrite
  d <- box_dl(test_vars$file_id, "test_dir", overwrite = TRUE)
})


test_that("Updating a file", {
  skip_on_cran()
  skip_on_travis()
  
  # This dir_id doesn't exist
  expect_error(box_ul(1, "test_dir/testfile.txt"))
  # This file to upload doesn't exist
  expect_error(box_ul(0, "test_dir/ololol.txt"))
  
  b <- box_ul(0, "test_dir/testfile.txt")
  
  file_id <- b$entries[[1]]$id
  
})



# Tidying up --------------------------------------------------------------

unlink("test_dir/testfile.txt")







# 
# 
# bf <- box_fetch(0, "test_dir")
# bp <- box_push(0, "test_dir")
# 
# uldf <- boxr:::uploadDirFiles(3110293969, "tests/testthat/test_dir/delete_me")
# dldf <- boxr:::downloadDirFiles(0, "tests/testthat/test_dir")
# 
# boxr:::box_dir_diff(0, "tests/testthat/test_dir")

