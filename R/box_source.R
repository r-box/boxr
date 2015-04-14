
#' @rdname box_save
#' @export
box_source <- function(file_id){
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  source(temp_file, local = globalenv())
}
