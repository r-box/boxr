
# Fetch -------------------------------------------------------------------

context("box_fetch")

test_that("box_fetch a dir", {
  skip_on_cran()
  skip_if_no_token()
  
  options(boxr.verbose = FALSE)
  
  # (Re)create local dir structure
  boxr:::create_test_dir()
  # Clear out whatever's already in there
  boxr:::clear_box_dir(0)

  td <- fs::path_temp("test_dir")  
  # Push the new files
  b <- box_push(0, td)
  
  # Works when dir_id is character
  b <- box_fetch("0", td, overwrite = FALSE, delete = FALSE)
  
  # No new files
  expect_equal(nrow(b$file_list[[1]]),  0)
  
  # Create some remote changes
  boxr:::modify_remote_dir()
  
  b <- box_fetch(0, td, overwrite = FALSE, delete = FALSE)
  
  # Two new downloads
  expect_equal(nrow(b$file_list[[1]]),  2)
  # No local files deleted
  expect_equal(nrow(b$file_list[[12]]), 0)
  # One local folder created
  expect_equal(nrow(b$file_list[[16]]), 1)
  # No local folders deleted
  expect_equal(nrow(b$file_list[[14]]), 0)

  b <- box_fetch(0, td, overwrite = TRUE, delete = FALSE)
  
  # One new download
  expect_equal(nrow(b$file_list[[1]]),  1)
  # No local files deleted
  expect_equal(nrow(b$file_list[[12]]), 0)
  # One local folder created
  expect_equal(nrow(b$file_list[[16]]), 0)
  # No local folders deleted
  expect_equal(nrow(b$file_list[[14]]), 0)
  
  b <- box_fetch(0, td, overwrite = TRUE, delete = TRUE)
  
  # No new downloads
  expect_equal(nrow(b$file_list[[1]]),  0)
  # One local file deleted
  expect_equal(nrow(b$file_list[[12]]), 1)
  # No local folders created
  expect_equal(nrow(b$file_list[[16]]), 0)
  # One local folder deleted (and it's contents)
  expect_equal(nrow(b$file_list[[14]]), 1)
  
})
