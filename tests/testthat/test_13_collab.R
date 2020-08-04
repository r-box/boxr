context("Box collaborations")

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

