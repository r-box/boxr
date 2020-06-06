context("Managing collaborations")

test_that("creating a collaboration works", {
  
  skip_if_no_token()
  
  # (Re)create local dir structure
  boxr:::create_test_dir()
  boxr::box_push(0, "test_dir/")
  writeLines("collab test file", file.path("test_dir", "collab.txt"))
 
  # upload them
  file <- box_ul(0, "test_dir/collab.txt")
  folder <- box_dir_create("collab")
  
  # create collabs with the boxr tester account
  collab_file <- box_create_collab_file(file$id, 9459307839)
  collab_folder <- box_create_collab_dir(folder$id, 9459307839)
  
  test_that("Collaborations are created", {
    expect_gt(as.numeric(collab_file$id), 10000)
    expect_gt(as.numeric(collab_folder$id), 10000)
  })
  
  test_that("Collaborations can be detected", {
    
    expect_message(
      folder_collab <- box_get_collab_dir(folder$id),
      "1 collaborator"
    )
    expect_message(
      file_collab <- box_get_collab_file(file$id),
      "1 collaborator"
    )
  })
  
  test_that("Collaborations can be deleted", {
    skip()
  })
})
