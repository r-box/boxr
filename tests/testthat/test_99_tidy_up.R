# Tidying up --------------------------------------------------------------

# Synch the highest level box.com dir with an empty local one to clear it out
b <- box_push(0, "test_dir/dir_12/dir_121/dir_1211", delete = TRUE)

# Remove the local dir
unlink("test_dir", recursive = TRUE, force = TRUE)