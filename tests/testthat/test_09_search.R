
# Search ------------------------------------------------------------------

context("Search")

test_that("Searching works", {
# Unfortunately, it seems like box.com is unable to find files via search, if 
# they've just been uploaded. This is the same, even in the web-app, so it
# doesn't seem like an issue peculiar to the API or this package. Ho hum!

  
#
#   skip_on_cran()
#   boxr:::skip_on_travis()
#   
#   # Uploading
#   n_files <- 9
#   
#   # Paths for tempfiles to upload
#   temp_dir <- paste0(tempdir(),"/filetest/")
#   
#   dir.create(temp_dir)
#   
#   tf <- normalizePath(
#     paste0(temp_dir, "testfile", 1:n_files, c(".R", ".txt", ".py")),
#     mustWork = FALSE
#   )
#   
#   # Contents for the files to upload
#   contents   <- paste("This is the contents of file", 1:n_files)
#   
#   # Write the text files out
#   mapply(writeLines, contents, tf)
#   
#   # Upload them
#   b <- box_push(0, temp_dir, delete = TRUE)

})
