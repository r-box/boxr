# For passing vars (e.g. file_id's) between testing environments
test_vars <- new.env(parent = globalenv())

# Upload/download ---------------------------------------------------------
context("Upload/download/update")

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
  options(boxr.verbose = FALSE)
  
  # This dir_id doesn't exist
  expect_error(box_ul(1, "test_dir/testfile.txt"))
  # This file to upload doesn't exist
  expect_error(box_ul(0, "test_dir/ololol.txt"))
  # This should work
  expect_message(b <- box_ul(0, "test_dir/testfile.txt"), "version")  
})