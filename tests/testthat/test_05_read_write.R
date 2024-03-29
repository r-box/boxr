
# Source/read/write --------------------------------------------------------------

context("Source/read/write")

test_that("You can source a remote R script", {
  skip_on_cran()
  skip_if_no_token()
  
  # Write a little R script
  tf <- withr::local_tempfile(fileext = ".R")
  writeLines("test_vector <- 1:10\n", tf)
  
  # Upload it, so that you can 'source it' back down
  b <- box_ul(0, tf)
  
  # Can you source it in?
  box_source(b$id)
  # Did the script execute correctly?
  expect_equal(1:10, test_vector)
})


test_that("You can write/read a remote .csv file", {
  skip_on_cran()
  skip_if_no_token()
  
  # Write a little .csv file
  tf <- withr::local_tempfile(fileext = ".csv")
  # Note: It looks like although httr says it uses read.csv for the .csv files,
  # it doesn't obey the usual R behaviour of treating strings as factors by
  # default. So to get two objects that match, you'll need to make sure they're
  # just strings in the original, too.
  df <- data.frame(a = letters[1:5], b = 1:5, c = rnorm(5), 
                   stringsAsFactors = FALSE)
  write.csv(df, tf, row.names = FALSE)
  
  # Upload it, so that you can 'read it' back down
  b <- box_ul(0, tf)
  
  # box_read()
  #
  # Can you read it in?
  expect_message(df2 <- box_read(b$id), "read")
  # Did the script execute correctly?
  expect_equal(df, df2)
  
  # box_read_csv()
  #
  # Can you read it in?
  expect_message(df2 <- box_read_csv(b$id, stringsAsFactors = FALSE), "read")
  # Did the script execute correctly?
  expect_equal(df, df2)
})

test_that("You can write/read a remote .tsv file", {
  skip_on_cran()
  skip_if_no_token()
  
  # Write a little .tsv file
  tf <- withr::local_tempfile(fileext = ".tsv")
  
  df <- data.frame(a = letters[1:5], b = 1:5, c = rnorm(5), 
                   stringsAsFactors = FALSE)
  
  write.table(df, tf, sep = "\t", col.names = TRUE, row.names = FALSE)
  
  # Upload it, so that you can 'read it' back down
  b <- box_ul(0, tf)
  
  # box_read()
  #
  # Can you read it in?
  expect_message(df2 <- box_read(b$id), "read")
  # Did the script execute correctly?
  expect_equal(df, df2)
  
  # box_read_tsv()
  #
  # Can you read it in?
  expect_message(df2 <- box_read_tsv(b$id), "read")
  # Did the script execute correctly?
  expect_equal(df, df2)
})

test_that("You can write/read a remote .json file", {
  skip_on_cran()
  skip_if_no_token()
  
  # Write a little .json file
  tf <- withr::local_tempfile(fileext = ".json")
  df <- data.frame(a = letters[1:5], b = 1:5, c = rnorm(5),
                   stringsAsFactors = FALSE)
  l  <- list(a = 1:10, b = matrix(1, 3, 3), c = df)
  
  writeLines(jsonlite::toJSON(l, digits = NA), tf) # default will round digits to 4 decimal places 
  
  b <- box_ul(0, tf)
  
  # passing `read_fun` directly until rio (>= 0.5.18) goes to CRAN
  expect_message(l2 <- box_read(b$id, read_fun = jsonlite::fromJSON), "read")
  expect_equivalent(l, l2)

  expect_message(l2 <- box_read_json(b$id), "read")
  expect_equal(l, l2)
})


# RDS --------------------------------------------------------

test_that("RDS files work", {
  skip_on_cran()
  skip_if_no_token()
  
  dat <- data.frame(a = letters[1:5], b = 1:5, c = rnorm(5))
  b <- box_save_rds(dat)
  
  expect_message( dat2 <- box_read_rds(b$id), "read" )
  expect_equivalent(dat, dat2)
  
})
