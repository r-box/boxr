#' Execute R code stored on box.com
#' 
#' `box_source` will download a remote R script stored on box.com, and then
#' attempt to execute it, using [source()].
#' 
#' @inheritParams box_dl
#' 
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
#' 
#' @seealso
#' * [box_dl()] for saving files to disk, 
#' * [box_save()] for working with R workspaces, and 
#' * [box_read()] for reading files into memory as R objects.
#' @export
box_source <- function(file_id) {
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  source(temp_file, local = globalenv())
}
