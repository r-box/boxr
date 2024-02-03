context("Load/Save")

test_that("object can be saved, retrieved, and deleted", {
  skip_on_cran()
  skip_if_no_token()
  
  # Here's an R object
  test_ref <- list(data.frame(), 1:10, letters)
  
  test_list <- test_ref
  rda_name <- "test.RData"  
  
  b_save <- box_save(test_list, dir_id = 0, file_name = rda_name)
  expect_equal(rda_name, b_save$name)  
  
  rm("test_list")
  
  # will load data into `test_list`
  b_load <- box_load(b_save$id)
  expect_identical(b_load, "test_list")
  expect_equal(test_ref, test_list)
  
  # clean up
  box_delete_file(b_save$id)
  
})
  
