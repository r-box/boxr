# Tidying up --------------------------------------------------------------
b <- box_push(0, "test_dir/dir_12/dir_121/dir_1211", delete = TRUE)

unlink("test_dir", recursive = TRUE, force = TRUE)