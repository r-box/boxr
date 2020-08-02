context("Box collaborations")

test_that("Collaborations can be created/detected/deleted", {
  skip_on_cran()
  skip_if_no_token()
  
  # file setup prep
  tf <- fs::path_temp("test_dir", "collab.txt")
  writeLines("collab test file", fs::path_temp("test_dir", "collab.txt"))
  
  # upload them
  file <- box_ul(0, tf)
  folder <- box_dir_create("collab")
  
  # create collab with the boxr tester account
  boxr_tester_acct <- 9459307839
  collab_file <- box_create_collab(file_id = file$id, user_id = boxr_tester_acct)
  collab_folder <- box_create_collab(folder$id, boxr_tester_acct)
  
  some_bigish_int <- 1e10 # Box IDs are (so far) always integers
  expect_gt(as.numeric(collab_file$id), some_bigish_int)
  expect_gt(as.numeric(collab_folder$id), some_bigish_int)
  
  expect_message(
    folder_collab <- box_get_collab(folder$id),
    "1 collaborator"
  )
  expect_message(
    file_collab <- box_get_collab(file_id = file$id),
    "1 collaborator"
  )
  expect_s3_class(folder_collab, "data.frame")
  expect_s3_class(file_collab, "data.frame")
  
  box_delete_collab(collab_file$id)
  box_delete_collab(collab_folder$id)
  
  expect_error(box_get_collab(file_id = file$id), NULL)
  expect_error(box_get_collab(folder$id), NULL)
})

