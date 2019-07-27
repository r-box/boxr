#' Source R code from a Box file
#' 
#' This function downloads a file from Box, then runs its 
#' contents, as R code, using [source()].
#' 
#' @inheritParams box_dl
#' @return Object returned by [source()].
#' 
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
#' 
#' @seealso [box_dl()], [box_save()], [box_read()] 
#' @export
#' 
box_source <- function(file_id) {
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  source(temp_file, local = globalenv())
}
