context("Listing remote files")

# function to create a file in a local directory
#
# @param x    numeric, number of the file
# @param dir  character, name of the directory
#
fn_create <- function(x, dir){
  
  name <- paste0("file_", formatC(x, width = 4, flag = "0"), ".txt")
  filename <- file.path(dir, name)
  
  writeLines("test file", filename)
}

# given a boxr_object_list, return a vector of names
#
# @param b boxr_object_list
#
get_filenames <- function(b){
  b %>%
    as.data.frame() %>%
    dplyr::arrange(name) %>%
    `[[`("name")
}

test_that("file listing works", {
  
  skip_on_cran()
  skip_if_no_token()

  # (Re)create local dir structure
  boxr:::create_test_dir()
  
  # create ls directory
  dir_ls <- fs::path_temp("test_dir", "ls")
  dir.create(dir_ls)

  # create files in the ls directory
  purrr::walk(seq_len(10), fn_create, dir_ls)
 
  files_local <- list.files(dir_ls)
  
  # push the local directory
  box_push(0, fs::path_temp("test_dir"), overwrite = TRUE, delete = TRUE)
  
  # get the id of the remote ld directory
  id_ls <- 
    box_ls(0) %>%
    as.data.frame() %>%
    dplyr::filter(name == "ls") %>%
    dplyr::select(id) %>%
    `[[`(1) %>%
    as.numeric()
  
  # get the listing without pagination
  files_box <- 
    box_ls(id_ls) %>% 
    get_filenames()

  # get the listing with pagination
  files_box_page <-
    box_ls(id_ls, limit = 5) %>%
    get_filenames()
  
  # test
  expect_identical(files_box, files_local)
  expect_identical(files_box_page, files_local)
 
})
