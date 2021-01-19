#' Source R code from a Box file
#' 
#' @description 
#' **Note**: please use this function with extreme caution, as it is 
#' possible for the contents of a Box file to change without
#' your knowledge.
#' 
#' This function downloads a file from Box, then runs its 
#' contents, as R code, using [source()]. 
#' 
#' @inheritParams box_browse
#' @inheritParams base::source
#' @param ... Other arguments passed to [source()].
#' @return Object returned by [source()], 
#'   called for side-effect of modifying an environment.
#' 
#' @seealso [box_dl()], [box_save()], [box_read()] 
#' @keywords internal
#' @export
#' 
box_source <- function(file_id, local = globalenv(), ...) {
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  source(temp_file, local = local, ...)
}
