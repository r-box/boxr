
# Versions ----------------------------------------------------------------

context("Versions")

test_that("Versions work", {
  skip_on_cran()
  skip_if_no_token()
  
  options(boxr.verbose = FALSE)
  
  # Uploading
  boxr:::create_test_dir()
  tf <- fs::path_temp("test_dir", "versiontest.txt")
  td <- fs::path_temp("test_dir")
  
  n_versions <- 5
  contents   <- paste("This is version", 1:n_versions)
  
  # Upload the first version
  writeLines(contents[1], tf)
  ul <- box_ul(0, tf)
  expect_is(ul, "boxr_file_reference")
  
  v_file_id <- ul$id
  
  expect_message(box_version_history(v_file_id), "No previous versions")
  
  expect_message(box_version_number(v_file_id), "version 1")
  
  # Upload subsequent versions
  for (v in 2:n_versions) {
    writeLines(contents[v], tf)
    
    # Is the user notified of the upadte operation and version number?
    expect_message(
      ul <- box_ul(0, tf),
      paste0("Attempting to upload new version \\(V", v, "\\)")
    )
    
    # Do they have the right class?
    expect_is(ul, "boxr_file_reference")
    
    # Has the file_id remained constant?
    expect_equal(ul$id, v_file_id)
    
    # Is the version being incremented?
    expect_equal(box_version_number(v_file_id), v)
  }
  
  # Downloading
  # Are there n_versions-1 previous versions of the file?
  v_df <- box_version_history(v_file_id)
  
  expect_equal(nrow(v_df), n_versions - 1)
  
  # Test that the id parameter works (note: there's no id for the fifth version)
  for (v in 1:(n_versions - 1)) {
    dl <- box_dl(v_file_id, version_id = v_df$file_version_id[v], 
                 overwrite = TRUE, local_dir = td)
    
    # Did box_dl do the right thing?
    expect_true(file.exists(dl))
    
    # Does the remote file have the right contents?
    expect_true(readLines(dl) == contents[v])
  }
  
  # Test that the numeric version parameter works
  for (v in 1:n_versions) {
    dl <- box_dl(v_file_id, version_no = v, overwrite = TRUE, 
                 local_dir = td)
    
    # Did box_dl do the right thing?
    expect_true(file.exists(dl))
    
    # Does the remote file have the right contents?
    expect_true(readLines(dl) == contents[v])
  }
})
