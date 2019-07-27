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
#' [jsonlite::toJSON()].
#' 
#' Please note that `box_write()` is used to write  Robjects to Box files 
#' using standard formats. To write R objects as `.RData` files, 
#' you can use [box_save()].
#'
#' @inheritParams box_ul
#' @param x object to be written
#' @param filename `character`, name of the new Box file
#' @param write_fun `function`, used to write (serialize) the content from R; 
#'  default function is [rio::export()]
#' @param ... additional arguments passed to `write_fun`
#' 
#' @return Object with S3 class [`boxr_file_reference`][boxr_S3_classes]
#' 
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
#' 
#' @export
box_write <- function(x, filename, dir_id = box_getwd(), description = NULL,
                    write_fun = rio::export, ...) {
  temp_file <- paste0(tempdir(), "/", filename)
  write_fun(x, temp_file)
  box_ul(dir_id = dir_id, file = temp_file, description = description)
}
