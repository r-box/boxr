# Informal testing script (testthat isn't ideal for this package)


install.packages("C:/Users/brendan.rocks/Documents/boxr_0.0.0.9000.zip", repos = NULL)
library(boxr)

b <-
  box_auth(
    client_id     = readLines(".client_id"),
    client_secret = readLines(".client_secret"),
    interactive = FALSE,
    cache = "tests/testthat/.boxr-oauth",
    write.Renv = FALSE
  )

bf <- box_fetch(0, "tests/testthat/test_dir")
bp <- box_push(0, "tests/testthat/test_dir")

uldf <- boxr:::uploadDirFiles(3110293969, "tests/testthat/test_dir/delete_me")
dldf <- boxr:::downloadDirFiles(0, "tests/testthat/test_dir")

boxr:::box_dir_diff(0, "tests/testthat/test_dir")
