context("Tidying up")

test_that("Box directory is emptied", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  # push empty local dir to top level on Box
  b <- box_push(0, "test_dir/dir_12/dir_121/dir_1211", delete = TRUE)
  
  expect_equal(nrow(as.data.frame(box_ls(0))), 0)
})

test_that("Local directory is deleted", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  unlink("test_dir", recursive = TRUE, force = TRUE)
  
  expect_equal(sum(grepl("test_dir", list.dirs(recursive = F))), 0)
})
