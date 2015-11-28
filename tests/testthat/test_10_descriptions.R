
# Descriptions ----------------------------------------------------------------

context("Descriptions")

test_that("You can add a description", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  options(boxr.verbose = FALSE)
  
  description <- "This is a cool description"
  
  fr1 <- box_write(data.frame("This"), "file.txt")
  
  # Try to add a descripton
  fr2 <- box_add_description(fr1, description)
  
  # Check that it worked
  expect_is(fr2, "boxr_file_reference")
  expect_equal(fr2$description, description)
  
})
