
# Write ----------------------------------------------------------------

context("Writing R Objects")

df <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)

test_that("Write some data.frames", {

  skip_if_no_token()
  
  options(boxr.verbose = FALSE)

  expect_is(box_write(df, "test.csv"),  "boxr_file_reference")
  expect_is(box_write(df, "test.tsv"),  "boxr_file_reference")
  expect_is(box_write(df, "test.xlsx"), "boxr_file_reference")
  expect_is(box_write(df, "test.json"), "boxr_file_reference")
  
})

test_that("Dots are being passed to underlying read/write functions", {
  skip_if_no_token()
  
  b <- box_write(df, "test2.csv", sep = ";")
  df2 <- box_read(b$id, sep = ";")
  
  expect_equal(df, df2)
})

rm(df)
