# Load/Save ---------------------------------------------------------------
# For some reason (probably related to environements), these run fine in the 
# console/terminal, but testthat can't find the files when running them with
# devtools::test()
#
# Leaving not-run, for now
if (FALSE) {
  context("Load/Save")
  
  test_that("Saving R Object Remotely", {
    skip_on_cran()
    skip_if_no_token()
    
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
    skip_if_no_token()
    
    rm("object")
    
    # Can you load the remote file which stores the R object?
    b <- box_load(test_vars$object_return$entries[[1]]$id)
    
    # Did it return the right object name?
    expect_equal("object", b)
    # Is the R object the same after it's journey?
    expect_equal(object, test_vars$object)
  })
  
}
