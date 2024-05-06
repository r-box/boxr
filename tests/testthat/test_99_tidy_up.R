context("Tidying up")

test_that("Box directory is emptied", {
  skip_on_cran()
  boxr:::skip_on_travis()
  skip_if_no_token()
  
  boxr:::clear_box_dir(0)
  
  expect_equal(nrow(as.data.frame(box_ls(0))), 0)
})

test_that("Local directory is deleted", {
  skip_on_cran()
  boxr:::skip_on_travis()
  skip_if_no_token()
  
  unlink("test_dir", recursive = TRUE, force = TRUE)
  
  expect_equal(sum(grepl("test_dir", list.dirs(recursive = F))), 0)
})
