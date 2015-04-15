# Clear out (& box_push()) ------------------------------------------------
context("Clear out")

# Set up the local dir structure
create_test_dir()

# Make sure the remote directory in the test account is clear
test_that("Clear out the remote directory", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  options(boxr.verbose = FALSE)
  # Tell boxr to synch the remote home directory with an empty local one
  # (i.e. delete everything)
  b <- box_push(0, "test_dir/dir_12/dir_121/dir_1211", delete = TRUE)
})
