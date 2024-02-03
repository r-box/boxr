# Clear out (& box_push()) ------------------------------------------------
context("Clear out")


test_that("Local directory is created", {
  skip_if_no_token()
  
  # Set up the local dir structure
  # TODO: I think this writes to the working directory, which is a no-no
  #   we should write (and work) instead in the temp directory - IJL
  boxr:::create_test_dir()
  
  expect_true(any(grepl("test_dir", list.dirs(fs::path_temp()))))
})

# Make sure the remote directory in the test account is clear
test_that("Clear out the remote directory", {
  skip_if_no_token()
  
  options(boxr.verbose = FALSE)
  # Tell boxr to sync the remote home directory with an empty local one
  # (i.e. delete everything)
  boxr:::clear_box_dir(0)
  
  expect_length(box_ls(0), 0)
  
})
