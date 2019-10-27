
# Push -------------------------------------------------------------------

context("box_push")

test_that("box_push a dir", {
  skip_on_cran()
  skip_if_no_token()
  
  options(boxr.verbose = FALSE)
  
  # (Re)create local dir structure
  boxr:::create_test_dir()
  # Clear out whatever's already in there
  boxr:::clear_box_dir(0)
  
  # Push the new files
  b <- box_push(0, "test_dir")
  
  # Is it an object of the right class?
  expect_equal(class(b), "boxr_dir_wide_operation_result")
  
  # Change the local files
  boxr:::modify_test_dir()
  
  b <- box_push(0, "test_dir", overwrite = FALSE, delete = FALSE)
  
  # One new file uploaded
  expect_equal(nrow(b$file_list[[5]]),  1)
  # No files updated
  expect_equal(nrow(b$file_list[[3]]),  0)
  # No remote folders deleted
  expect_equal(nrow(b$file_list[[10]]), 0)
  # No remote files deleted
  expect_equal(nrow(b$file_list[[8]]),  0)
  # One remote folder created
  expect_equal(nrow(b$file_list[[17]]), 1)


  b <- box_push(0, "test_dir", overwrite = TRUE, delete = FALSE)
  
  # No new files uploaded
  expect_equal(nrow(b$file_list[[5]]),  0)
  # One file updated
  expect_equal(nrow(b$file_list[[3]]),  1)
  # No remote folders deleted
  expect_equal(nrow(b$file_list[[10]]), 0)
  # No remote files deleted
  expect_equal(nrow(b$file_list[[8]]),  0)
  # No remote folders created
  expect_equal(nrow(b$file_list[[17]]), 0)
  
  b <- box_push(0, "test_dir", overwrite = TRUE, delete = TRUE)
  
  # No new files uploaded
  expect_equal(nrow(b$file_list[[5]]),  0)
  # No files updated
  expect_equal(nrow(b$file_list[[3]]),  0)
  # One remote folder deleted
  expect_equal(nrow(b$file_list[[10]]), 1)
  # One remote file deleted
  expect_equal(nrow(b$file_list[[8]]),  1)
  # No remote folders created
  expect_equal(nrow(b$file_list[[17]]), 0)
  
})

