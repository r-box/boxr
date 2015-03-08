library(boxr)

# Yoinked from the dev build of testthat
# https://github.com/hadley/testthat/blob/0835a9e40d3a2fbaac47cbe8f86239e231623b51/R/utils.r
skip_on_travis <- function() {
  if (!identical(Sys.getenv("TRAVIS"), "true")) return()
  
  skip("On Travis")
}

# setwd("tests/testthat")

# For passing vars (e.g. file_id's) between testing environments
test_vars <- new.env()
options(boxr.verbose = FALSE)

# OAuth2.0 ----------------------------------------------------------------

context("OAuth2.0")
# At the moment, you can only test locally
test_that("Credentials are in the local repo", {
  skip_on_cran()
  skip_on_travis()
  
  # .gitignore'd files on in tld of local repo
  expect_true(file.exists("../../.client_id"))
  expect_true(file.exists("../../.client_secret"))
  expect_true(file.exists("../../.boxr-oauth"))
})

# See if you can log in
test_that("OAuth works", {
  skip_on_cran()
  skip_on_travis()
  
  expect_message(
    b <-
      box_auth(
        client_id     = readLines("../../.client_id"),
        client_secret = readLines("../../.client_secret"),
        interactive = FALSE,
        cache = "../../.boxr-oauth",
        write.Renv = FALSE
      ),
    "Authenticated at box.com"
  )
  
  expect_true(b)
})



# Clear out (& box_push()) ------------------------------------------------
context("Clear out)")
# Make sure the remote directory in the test account is clear
test_that("Clear out the remote directory", {
  skip_on_cran()
  skip_on_travis()
  
  # You can't git an empty dir, so create one
  dummy_dir <- "test_dir/dir_13"
  
  d <- dir.create(dummy_dir, showWarnings = FALSE)
  
  # Tell boxr to synch the remote home directory with an empty local one
  # (i.e. delete everything)
  b <- box_push(0, dummy_dir, delete = TRUE)
  
  d <- unlink(dummy_dir, force = TRUE, recursive = TRUE)
})


# Upload/download ---------------------------------------------------------
context("Upload/download")

# Write a test file to upload
writeLines("Completely Original File\n", "test_dir/testfile.txt")

test_that("Uploading a new file", {
  skip_on_cran()
  skip_on_travis()
  
  # This dir_id doesn't exist
  expect_error(box_ul(1, "test_dir/testfile.txt"))
  # This file to upload doesn't exist
  expect_error(box_ul(0, "test_dir/ololol.txt"))
  
  b <- box_ul(0, "test_dir/testfile.txt")
  
  test_vars$file_id <- b$entries[[1]]$id
  
})

test_that("Downloading a file", {
  skip_on_cran()
  skip_on_travis()
  
  # Overwrite default to FALSE, local copy present
  expect_error(box_dl(test_vars$file_id, "test_dir"))
  # This file_id doesn't exist
  expect_error(box_dl(1, "test_dir"))
  # This directory doesn't exist
  expect_error(box_dl(test_vars$file_id, "ololol"))
  # Overwrite should be bool
  expect_error(box_dl(test_vars$file_id, "test_dir", overwrite = "ololol"))
  
  # Should work if overwrite
  d <- box_dl(test_vars$file_id, "test_dir", overwrite = TRUE)
})


test_that("Updating a file", {
  skip_on_cran()
  skip_on_travis()
  
  # This dir_id doesn't exist
  expect_error(box_ul(1, "test_dir/testfile.txt"))
  # This file to upload doesn't exist
  expect_error(box_ul(0, "test_dir/ololol.txt"))
  
  expect_message(b <- box_ul(0, "test_dir/testfile.txt"), "version")
  
  file_id <- b$entries[[1]]$id
  
})


# Load/Save ---------------------------------------------------------------
# For some reason (probably related to environements, these run fine in the 
# console/terminal, but testthat can't find the files when running them with
# devtools::test())
#
# Leaving not-run, for now
if(FALSE){
context("Load/Save")
  
  test_that("Saving R Object Remotely", {
    skip_on_cran()
    skip_on_travis()
    
    # Here's an R object
    test_list <- list(data.frame(), 1:10, letters)
    test_vars$test_list <- test_list
    rda_name <- "test.RData"  
    
    # The upload should throw an error if it doesn't work
    b <- box_save(test_list, envir = globalenv(), dir_id = 0, file_name = rda_name)
    
    # Put the id in an environment variable for subsequent tests
    test_vars$object_return <- b
    
    # Did the file end up with the right name?
    expect_equal(rda_name, b$entries[[1]]$name)  
  })
  
  test_that("Loading remote R object", {
    skip_on_cran()
    skip_on_travis()
    
    rm("object")
    
    # Can you load the remote file which stores the R object?
    b <- box_load(test_vars$object_return$entries[[1]]$id)
    
    # Did it return the right object name?
    expect_equal("object", b)
    # Is the R object the same after it's journey?
    expect_equal(object, test_vars$object)
  })

}



test_that("You can source a remote R script", {
  skip_on_cran()
  skip_on_travis()
  
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
  skip_on_travis()
  
  # Write a little .csv file
  tf <- paste0(tempfile(), ".csv")
  # Note: It looks like although httr says it uses read.csv for the .csv files,
  # it doesn't obey the usual R behaviour of treating strings as factors by
  # default. So to get two objects that match, you'll need to make sure they're
  # just strings in the original, too.
  df <- data.frame(a = letters[1:5], b = 1:5, c = rnorm(5), 
                   stringsAsFactors = FALSE)
  write.csv(df, tf, row.names = FALSE)
  
  # Upload it, so that you can 'source it' back down
  b <- box_ul(0, tf)
  
  # Can you source it in?
  expect_message(df2 <- box_read(b$entries[[1]]$id), "read")
  # Did the script execute correctly?
  expect_equal(df, df2)
})


test_that("You can write/read a remote .json file", {
  skip_on_cran()
  skip_on_travis()
  
  # Write a little .json file
  tf <- paste0(tempfile(), ".json")
  df <- data.frame(a = letters[1:5], b = 1:5, c = round(rnorm(5), 3), 
                   stringsAsFactors = FALSE)
  l  <- list(a = 1:10, b = matrix(1, 3, 3), c = df)
  
  writeLines(jsonlite::toJSON(l), tf)
  
  # Upload it, so that you can 'source it' back down
  b <- box_ul(0, tf)
  
  # Can you source it in?
  l2 <- box_read(b$entries[[1]]$id)
  # Did the script execute correctly?
  expect_equal(l, l2)
})


# Tidying up --------------------------------------------------------------

unlink("test_dir/testfile.txt")

# 
# 
# bf <- box_fetch(0, "test_dir")
# bp <- box_push(0, "test_dir")
# 
# uldf <- boxr:::uploadDirFiles(3110293969, "tests/testthat/test_dir/delete_me")
# dldf <- boxr:::downloadDirFiles(0, "tests/testthat/test_dir")
# 
# boxr:::box_dir_diff(0, "tests/testthat/test_dir")

