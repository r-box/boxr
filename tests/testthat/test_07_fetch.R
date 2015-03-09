
# Push -------------------------------------------------------------------

context("box_fetch")

test_that("box_fetch a dir", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  options(boxr.verbose = FALSE)
  
  # (Re)create local dir structure
  boxr:::create_test_dir()
  # Clear out whatever's already in there
  boxr:::clear_box_dir(0)
  
  # Push the new files
  b <- box_push(0, "test_dir")
  
  # Create some remote changes
  boxr:::modify_remote_dir()
  
  b <- box_fetch(0, "test_dir", overwrite = FALSE, delete = FALSE)
  
  # Two new downloads
  expect_equal(nrow(b$file_list[[1]]),  2)
  # No local files deleted
  expect_equal(nrow(b$file_list[[12]]), 0)
  # One local folder created
  # expect_equal(nrow(b$file_list[[16]]), 0)
  # No local folders deleted
  # expect_equal(nrow(b$file_list[[14]]), 0)

  b <- box_fetch(0, "test_dir", overwrite = TRUE, delete = FALSE)
  
  # One new download
  expect_equal(nrow(b$file_list[[1]]),  1)
  # No local files deleted
  expect_equal(nrow(b$file_list[[12]]), 0)
  # One local folder created
  # expect_equal(nrow(b$file_list[[16]]), 0)
  # No local folders deleted
  # expect_equal(nrow(b$file_list[[14]]), 0)
  
  
  b <- box_fetch(0, "test_dir", overwrite = TRUE, delete = TRUE)
  
  # No new downloads
  expect_equal(nrow(b$file_list[[1]]),  0)
  # One local file deleted
  expect_equal(nrow(b$file_list[[12]]), 1)
  # One local folder created
  # expect_equal(nrow(b$file_list[[16]]), 0)
  # No local folders deleted
  # expect_equal(nrow(b$file_list[[14]]), 0)
  
})

