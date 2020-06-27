#' Write an R object to a Box file
#' 
#' This function is used to serialize an R object and write it
#' to a Box file. For example, you may wish to write a `data.frame`
#' to Box as a CSV file.
#' 
#' This is a two-step process. The first is to serialize the contents
#' of the R object, the second is to upload that serialization to a Box file.
#' The default serialization-function is [rio::export()].
#' 
#' The [rio::export()] function currently only 
#' supports `data.frame`; to serialize lists, you may wish to use 
#' `jsonlite::toJSON()`.
#' 
#' Please note that `box_write()` is used to write R objects to Box files 
#' using standard formats. To write R objects as `.RData` files, 
#' you can use [box_save()].
#'
#' @inheritParams box_ul
#' @param x Object to be written.
#' @param file_name `character`, name of the new Box file.
#' @param write_fun `function`, used to write (serialize) the content from R; 
#'  default function is [rio::export()].
#' @param ... Other arguments passed to `write_fun`.
#' @param filename `character`, **deprecated**: use `file_name` instead.
#' 
#' @return Object with S3 class [`boxr_file_reference`][boxr_S3_classes].
#' 
#' @export
box_write <- function(x, file_name, dir_id = box_getwd(), description = NULL,
                    write_fun = rio::export, filename, ...) {
  
  # TODO: in future version, remove argument
  if (!missing(filename)) {
    
    warning(
      "argument `filename` is deprecated; please use `file_name` instead.", 
      call. = FALSE
    )
    
    if (missing(file_name)) {
      file_name <- filename
    }
  }
  
  temp_file <- paste0(tempdir(), "/", file_name)
  write_fun(x, temp_file, ...)
  box_ul(dir_id = dir_id, file = temp_file, description = description)
}
