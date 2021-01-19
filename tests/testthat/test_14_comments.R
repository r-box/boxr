
# Comments ----------------------------------------------------------------

context("Comments")

test_that("Item helper-function works", {
  
  file_id <- 123
  comment_id <- 987
  
  expect_identical(
    comment_item_helper(file_id = file_id, comment_id = NULL),
    list(id = "123", type = "file")
  )
  
  expect_identical(
    comment_item_helper(file_id = NULL, comment_id = comment_id),
    list(id = "987", type = "comment")
  )
  
  expect_error(
    comment_item_helper(file_id = file_id, comment_id = comment_id),
    regexp = "specify only one"
  )
  
  expect_error(
    collab_item_helper(dir_id = NULL, file_id = NULL),
    regexp = "at least one"
  )
}) 

test_that("Comments work", {
  skip_on_cran()
  boxr:::skip_on_travis()
  
  file_upload <- box_write(data.frame("This"), "file1.txt")
  
  msg <- "hi there"
  
  expect_message(
    x <- box_comment_create(file_upload$id, msg),
    "Comment"
  )
  expect_s3_class(x, "boxr_comment")
  expect_type(x, "list")
  
  datf <- as.data.frame(x)
  expect_s3_class(datf, "data.frame")
  expect_s3_class(as_tibble(x), "tbl_df")
  
  expect_equal(nrow(datf), 1)
  expect_equal(file_upload$id, datf$item.id)
  expect_equal(msg, datf$message)
  
  x <- box_comment_get(datf[['item.id']])
  
  expect_s3_class(x, "boxr_comment_list")
  expect_type(x, "list")
  
  datf <- as.data.frame(x)
  expect_s3_class(datf, "data.frame")
  expect_s3_class(as_tibble(x), "tbl_df")
  expect_equal(nrow(datf), 1)
  expect_equal(datf$message[1], msg)
})
