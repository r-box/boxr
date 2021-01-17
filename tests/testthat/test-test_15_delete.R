test_that("box_folder_delete() works", {
  
  name_dir <- "test-folder-delete"
  
  folder <- box_dir_create(name_dir, parent_dir_id = 0)

  # check that box_delete_folder returns invisible(NULL)
  expect_invisible(
    expect_null(
      box_delete_folder(folder$id)
    )
  )
  
  # check that name_dir is not in box directory
  names <- box_ls(0) %>% as.data.frame() %>% `[[`("name")
  expect_false(name_dir %in% names)
  
})


test_that("box_file_delete() works", {
  
  name_file <- "test-file-delete.rds"
  
  file_upload <- box_save_rds("test", file_name = name_file, dir_id = 0)

  # check that box_delete_file returns invisible(NULL)
  expect_invisible(
    expect_null(
      box_delete_file(file_upload$id)
    )
  )
  
  # check that name_file is not in box directory
  names <- box_ls(0) %>% as.data.frame() %>% `[[`("name")
  expect_false(name_file %in% names)
  
})