context("Box collaborations")

regexp <- "specify only one"

test_that("Access helper-function works", {
  
  user_id <- 123
  group_id <- 456
  login <- "jobs@apple.com"
  
  expect_identical(
    collab_access_helper(user_id = user_id, group_id = NULL, login = NULL),
    list(type = "user", id = "123", login = NULL)
  )
  
  expect_identical(
    collab_access_helper(user_id = NULL, group_id = group_id, login = NULL),
    list(type = "group", id = "456", login = NULL)
  )
  
  expect_identical(
    collab_access_helper(user_id = NULL, group_id = NULL, login = login),
    list(type = "user", id = NULL, login = login)
  )
  
  # must provide exactly one
  expect_error(
    collab_access_helper(user_id = NULL, group_id = NULL, login = NULL),
    regexp = regexp
  )
  
  expect_error(
    collab_access_helper(user_id = user_id, group_id = group_id, login = NULL),
    regexp = regexp
  )
  

}) 

test_that("Item helper-function works", {
  
  dir_id <- 123
  file_id <- 456
  
  expect_identical(
    collab_item_helper(dir_id = dir_id, file_id = NULL),
    list(id = "123", type = "folder")
  )
  
  expect_identical(
    collab_item_helper(dir_id = NULL, file_id = file_id),
    list(id = "456", type = "file")
  )

  expect_error(
    collab_item_helper(dir_id = dir_id, file_id = file_id),
    regexp = regexp
  )
  
  expect_error(
    collab_item_helper(dir_id = NULL, file_id = NULL),
    regexp = "at least one"
  )
}) 

test_that("Collaborations can be created/detected/deleted", {
  skip_on_cran()
  skip_if_no_token()
  
  # file setup prep
  create_test_dir()
  tf <- fs::path_temp("test_dir", "collab.txt")
  writeLines("collab test file", tf)
  
  # upload them
  file <- box_ul(0, tf)
  dir <- box_dir_create("collab")
  
  # create collab with the boxr tester account
  boxr_tester_acct <- 9459307839
  collab_dir <- box_collab_create(dir$id, boxr_tester_acct)
  collab_file <- box_collab_create(file_id = file$id, user_id = boxr_tester_acct)
  
  
  some_bigish_int <- 1e10 # Box IDs are (so far) always integers
  expect_gt(as.numeric(collab_file$id), some_bigish_int)
  expect_gt(as.numeric(collab_dir$id), some_bigish_int)
  
  expect_message(
    dir_collab <- box_collab_get(dir$id),
    "1 collaborator"
  )
  expect_message(
    file_collab <- box_collab_get(file_id = file$id),
    "1 collaborator"
  )
  expect_s3_class(dir_collab, "data.frame")
  expect_s3_class(file_collab, "data.frame")
  
  box_collab_delete(collab_dir$id)
  box_collab_delete(collab_file$id)
  
  expect_error(box_collab_get(file_id = file$id), NULL)
  expect_error(box_collab_get(dir$id), NULL)
})

