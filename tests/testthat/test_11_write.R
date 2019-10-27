
# Write ----------------------------------------------------------------

context("Writing R Objects")

test_that("Write some data.frames", {

  skip_if_no_token()
  
  options(boxr.verbose = FALSE)
  
  df <- data.frame(a = 1:3, b = letters[1:3])
  
  expect_is(box_write(df, "test.csv"),  "boxr_file_reference")
  expect_is(box_write(df, "test.tsv"),  "boxr_file_reference")
  expect_is(box_write(df, "test.xlsx"), "boxr_file_reference")
  expect_is(box_write(df, "test.json"), "boxr_file_reference")
  
})
