#' Download/upload an R workspace from/to a Box file
#' 
#' Use these functions to save and load workspaces or collections of objects
#' to or from Box. Similar to [save()], [save.image()], and [load()]: 
#' these functions operate on files at Box instead of on local files.
#' 
#' \describe{
#'   \item{`box_save()`}{Save object(s) using [save()], write to Box.}
#'   \item{`box_save_image()`}{Save workspace image using [save.image()], 
#'     write to Box.}
#'   \item{`box_load()`}{Read from Box, load using [load()].}
#' }
#' 
#' @aliases box_load
#' 
#' @inheritParams box_ul
#' @param ... Objects to be saved, quoted or unquoted; passed to `save()`.
#'
#' @return 
#' \describe{
#'   \item{`box_save(), box_save_image()`}{Object with S3 class 
#'     [`boxr_file_reference`][boxr_S3_classes].}
#'   \item{`box_load()`}{From [load()], a character vector of the names of
#'     objects created, invisibly.}
#' }
#' 
#' @seealso [save()], [save.image()], [load()]
#'   
#' @export
#' 
box_save <- function(..., dir_id = box_getwd(), file_name = ".RData", 
                     description = NULL) {
  
  temp_file <- withr::local_tempfile(pattern = file_name)
  
  save(..., file = temp_file)
  
  box_ul(dir_id, temp_file, description = description)
}

#' @rdname box_save
#' @export
#' 
box_save_image <- function(dir_id = box_getwd(), file_name = ".RData", 
                           description = NULL, filename) {
  
  # TODO: in future version, remove argument
  if (!missing(filename)) {
    
    warning(
      "argument `filename` is deprecated; please use `file_name` instead.", 
      call. = FALSE
    )
    
    if (is.null(file_name)) {
      file_name <- filename
    }
  }
  
  # TODO: fs
  temp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  on.exit(fs::file_delete(temp_file))
  
  save.image(file = temp_file)
  
  box_ul(dir_id, temp_file, description = description)
}

#' @rdname box_save
#' @export
#' 
box_load <- function(file_id) {  
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  on.exit(fs::file_delete(temp_file))
  
  load(temp_file, envir = globalenv())
}
