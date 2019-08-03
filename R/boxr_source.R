#' Source R code from a Box file
#' 
#' This function downloads a file from Box, then runs its 
#' contents, as R code, using [source()]. 
#' 
#' @inheritParams box_dl
#' @inheritParams base::source
#' @param ... Other arguments passed to [source()].
#' @return Object returned by [source()], 
#'   called for side-effect of modifying an environment.
#' 
#' @seealso [box_dl()], [box_save()], [box_read()] 
#' @export
#' 
box_source <- function(file_id, local = globalenv(), ...) {
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  source(temp_file, local = local, ...)
}
