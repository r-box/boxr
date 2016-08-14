#' Execute R code stored on box.com
#' 
#' \code{box_source} will download a remote R script stored on box.com, and then
#' attempt to execute it, using \code{\link{source}}.
#' 
#' @inheritParams box_dl
#' 
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
#' 
#' @export
#' 
#' @seealso \code{\link{box_dl}} for saving files to disk, 
#'   \code{\link{box_save}} for working with R workspaces, and 
#'   \code{\link{box_read}} for reading files into memory as R objects.
#'   
box_source <- function(file_id) {
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  source(temp_file, local = globalenv())
}
