
# Source/read/write --------------------------------------------------------------

context("Source/read/write")

test_that("You can source a remote R script", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  # Write a little R script
  tf <- paste(tempfile(), ".R")
  writeLines("test_vector <- 1:10\n", tf)
  
  # Upload it, so that you can 'source it' back down
  b <- box_ul(0, tf)
  
  # Can you source it in?
  box_source(b$entries[[1]]$id)
  # Did the script execute correctly?
  expect_equal(1:10, test_vector)
})


test_that("You can write/read a remote .csv file", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  # Write a little .csv file
  tf <- paste0(tempfile(), ".csv")
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
  expect_message(df2 <- box_read(b$entries[[1]]$id), "read")
  # Did the script execute correctly?
  expect_equal(df, df2)
  
  # box_read_csv()
  #
  # Can you read it in?
  expect_message(df2 <- box_read_csv(b$entries[[1]]$id), "read")
  # Did the script execute correctly?
  expect_equal(df, df2)
})

test_that("You can write/read a remote .tsv file", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  # Write a little .tsv file
  tf <- paste0(tempfile(), ".tsv")
  
  df <- data.frame(a = letters[1:5], b = 1:5, c = rnorm(5), 
                   stringsAsFactors = FALSE)
  
  write.table(df, tf, sep = "\t", col.names = TRUE, row.names = FALSE)
  
  # Upload it, so that you can 'read it' back down
  b <- box_ul(0, tf)
  
  # box_read()
  #
  # Can you read it in?
  expect_message(df2 <- box_read(b$entries[[1]]$id), "read")
  # Did the script execute correctly?
  expect_equal(df, df2)
  
  # box_read_tsv()
  #
  # Can you read it in?
  expect_message(df2 <- box_read_tsv(b$entries[[1]]$id), "read")
  # Did the script execute correctly?
  expect_equal(df, df2)
})

test_that("You can write/read a remote .json file", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  # Write a little .json file
  tf <- paste0(tempfile(), ".json")
  df <- data.frame(a = letters[1:5], b = 1:5, c = round(rnorm(5), 3), 
                   stringsAsFactors = FALSE)
  l  <- list(a = 1:10, b = matrix(1, 3, 3), c = df)
  
  writeLines(jsonlite::toJSON(l), tf)
  
  # Upload it, so that you can 'read it' back down
  b <- box_ul(0, tf)
  
  # box_read()
  #
  # Can you read it in?
  expect_message(l2 <- box_read(b$entries[[1]]$id), "read")
  # Did the script execute correctly?
  expect_equal(l, l2)
  
  # box_read_json()
  #
  # Can you read it in?
  expect_message(l2 <- box_read_json(b$entries[[1]]$id), "read")
  # Did the script execute correctly?
  expect_equal(l, l2)
})

