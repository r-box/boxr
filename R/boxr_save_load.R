#' Save and load \code{R} workspaces via box.com
#' 
#' These convenience functions aim to provide analagous functionality to 
#' \code{\link[base]{load}} and \code{\link[base]{save.image}} (or 
#' \code{\link[base]{save}}), but for \code{.RData} files stored on box.com, as 
#' opposed to locally.
#' 
#' @aliases box_load
#' 
#' @inheritParams box_dl
#' @param ... The objects to be saved. Quoted or unquoted. Passed to 
#'   \code{\link{save}}.
#' @param dir_id The box.com folder id where the objects will be stored as a
#'   \code{.RData} file.
#' @param file_name The name you'd like your \code{.Rdata} file saved as. For
#'   example, "myworkspace.RData"
#' @param file_id For \code{box_load}, the box.com id of the \code{.RData} or
#'   \code{.rda} file you'd like to load into your workspace.
#'
#' @details \code{box_save} saves an .RData file using 
#'   \code{\link[base]{save.image}} if \code{objects} is not supplied or 
#'   \code{\link[base]{save}} if it is. The file is then uploaded to box.com via 
#'   \code{\link{box_ul}}.
#' 
#'   \code{box_load} downloads a file from box.com using \code{\link{box_dl}},
#'   and then \code{\link[base]{load}}s it into the current workspace.
#' 
#' @return \code{box_load} returns a character vector of the names of objects 
#'   created, invisibly. \code{box_save} and \code{box_save_image} are used for 
#'   their side effects, and doen't return anything.
#'   
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' 
#' @seealso The base R functions which these wrap; \code{\link{save}},
#'   \code{\link{save.image}} and \code{\link{load}}.
#'   
#' @export
box_save <- function(..., dir_id = box_getwd(), file_name = ".RData", 
                     description = NULL) {
  temp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  save(..., file = temp_file)
  box_ul(dir_id, temp_file, description = description)
}

#' @rdname box_save
#' @export
box_save_image <- function(dir_id = box_getwd(), file_name = ".RData", 
                           description = NULL) {
  temp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  save.image(file = temp_file)
  
  box_ul(dir_id, temp_file, description = description)
}

#' @rdname box_save
#' @export
box_load <- function(file_id) {  
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  load(temp_file, envir = globalenv())
}
