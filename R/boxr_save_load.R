#' Download/upload an R workspace from/to a Box file
#' 
#' Similar to [save()], [save.image()], and [load()]; these functions operate on 
#' files at Box instead of on local files.
#' 
#' \describe{
#'   \item{`box_save()`}{Save object(s) using [save()], write to Box}
#'   \item{`box_save_image()`}{Save image using [save.image()], write to Box}
#'   \item{`box_saveRDS()`}{Save object using [`saveRDS()`], write to Box.}
#'   \item{`box_load()`}{Read from Box, load using [load()].}
#' }
#' 
#' @aliases box_load
#' @md
#' 
#' @inheritParams box_dl
#' @inheritParams box_write
#' @param ... Objects to be saved, quoted or unquoted; passed to `save()`.
#' @param object R object to serialize.
#'
#' @return 
#' \describe{
#'   \item{`box_save()`}{Object with S3 class [`boxr_file_reference`][boxr_S3_classes].}
#'   \item{`box_save_image()`}{Object with S3 class [`boxr_file_reference`][boxr_S3_classes].}
#'   \item{`box_load()`}{From [load()], a character vector of the names of objects 
#'   created, invisibly.}
#' }
#' 
#' @seealso [save()], [save.image()], [load()]
#'   
#' @export
#' 
box_save <- function(..., dir_id = box_getwd(), file_name = ".RData", 
                     description = NULL) {
  
  temp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  save(..., file = temp_file)
  box_ul(dir_id, temp_file, description = description)
}

#' @rdname box_save
#' @export
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
  
  temp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  save.image(file = temp_file)
  
  box_ul(dir_id, temp_file, description = description)
}

#' @rdname box_save
#' @export
box_save_rds <- function(object, dir_id = box_getwd(), file_name = ".RDS", ...,
                     description = NULL) {
  
  temp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  saveRDS(object, temp_file)
  box_ul(as.integer(dir_id), temp_file, description = description)
}

#' @rdname box_save
#' @export
box_load <- function(file_id) {  
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  load(temp_file, envir = globalenv())
}
