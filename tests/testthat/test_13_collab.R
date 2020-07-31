context("Box collaborations")

writeLines("collab test file", file.path("test_dir", "collab.txt"))

# upload them
file <- box_ul(0, "test_dir/collab.txt")
folder <- box_dir_create("collab")

# create collabs with the boxr tester account
boxr_tester_acct <- 9459307839
collab_file <- box_create_collab(file_id = file$id, user_id = boxr_tester_acct)
collab_folder <- box_create_collab(folder$id, boxr_tester_acct)

test_that("Collaborations can be created", {
  skip_on_cran()
  skip_if_no_token()
  
  some_bigish_int <- 1e10 # Box IDs are (so far) always integers
  expect_gt(as.numeric(collab_file$id), some_bigish_int)
  expect_gt(as.numeric(collab_folder$id), some_bigish_int)
})

test_that("Collaborations can be detected", {
  skip_on_cran()
  skip_if_no_token()
  
  expect_message(
    folder_collab <- box_get_collab(folder$id),
    "1 collaborator"
  )
  expect_message(
    file_collab <- box_get_collab(file_id = file$id),
    "1 collaborator"
  )
})

test_that("Collaborations can be deleted", {
  skip_on_cran()
  skip_if_no_token()
  
  box_delete_collab(collab_file$id)
  box_delete_collab(collab_folder$id)
  
  expect_error(box_get_collab(file_id = file$id), NULL)
  expect_error(box_get_collab(folder$id), NULL)
  
})
