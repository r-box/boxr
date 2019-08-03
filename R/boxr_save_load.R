#' Download/upload an R workspace from/to a Box file
#' 
#' Similar to [save()], [save.image()], and [load()]; these functions operate on 
#' files at Box instead of on local files.
#' 
#' \describe{
#'   \item{`box_save()`}{save object(s) using [save()], write to Box file}
#'   \item{`box_save_image()`}{save image using [save.image()], write to Box file}
#'   \item{`box_load()`}{read from Box file, load using [load()]}
#' }
#' 
#' @aliases box_load
#' 
#' @inheritParams box_dl
#' @inheritParams box_write
#' @param file_name `character`, **deprecated**: use `filename` instead
#' @param ... objects to be saved, quoted or unquoted; passed to [save()].
#'
#' @return 
#' \describe{
#'   \item{`box_save()`}{Object with S3 class [`boxr_file_reference`][boxr_S3_classes]}
#'   \item{`box_save_image()`}{Object with S3 class [`boxr_file_reference`][boxr_S3_classes]}
#'   \item{`box_load()`}{From [load()], a character vector of the names of objects 
#'   created, invisibly.}
#' }
#' 
#' @seealso [save()], [save.image()], [load()]
#'   
#' @export
#' 
box_save <- function(..., dir_id = box_getwd(), filename = ".RData", 
                     description = NULL, file_name) {
  
  # TODO: in future version, remove argument
  if (!missing(file_name)) {
  
    warning(
      "argument `file_name` is deprecated; please use `filename` instead.", 
      call. = FALSE
    )

    filename <- file_name
  }
  
  temp_file <- normalizePath(file.path(tempdir(), filename), mustWork = FALSE)
  save(..., file = temp_file)
  box_ul(dir_id, temp_file, description = description)
}

#' @rdname box_save
#' @export
box_save_image <- function(dir_id = box_getwd(), filename = ".RData", 
                           description = NULL, file_name) {
  
  # TODO: in future version, remove argument
  if (!missing(file_name)) {
    
    warning(
      "argument `file_name` is deprecated; please use `filename` instead.", 
      call. = FALSE
    )
    
    filename <- file_name
  }
  
  temp_file <- normalizePath(file.path(tempdir(), filename), mustWork = FALSE)
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
