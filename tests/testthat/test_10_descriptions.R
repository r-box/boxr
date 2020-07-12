
# Descriptions ----------------------------------------------------------------

# Note: These are currently failing with HTTP error code 500: the request is
#   fine but something has gone wrong on the box.com end. Commenting out the 
#   test while box resolve this.

# context("Descriptions")
#
# test_that("You can add a description", {
#   skip_on_cran()
#   boxr:::skip_on_travis()
#   
#   options(boxr.verbose = FALSE)
#   
#   description <- "This is a cool description"
#   
#   fr1 <- box_write(data.frame("This"), "file.txt")
#   
#   # Try to add a descripton
#   fr2 <- box_add_description(fr1, description)
#   
#   # Check that it worked
#   expect_is(fr2, "boxr_file_reference")
#   expect_equal(fr2$description, description)
#   
# })


# Comments ----------------------------------------------------------------

context("Comments")

test_that("Comments work", {
  skip_on_cran()
  boxr:::skip_on_travis()

  fr1 <- box_write(data.frame("This"), "file1.txt")
  
  msg <- "hi there"
  
  expect_message(
    resp <- box_comment(fr1$id, msg),
    "Comment"
  )
  
  coms <- box_get_comments(resp[["item"]][["id"]])
  
  expect_s3_class(coms, "data.frame")
  expect_equal(nrow(coms), 1)
  expect_equal(coms$message, msg)
})
