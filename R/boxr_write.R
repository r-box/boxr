#' Write R Objects to Files Remotely Hosted on box.com
#' 
#' @description
#' A fast and lazy way to upload R objects to box.com in a commonly readable
#' file format. `read_fun` is used to convert R objects to files, which
#' by default is the [export()] function from the `rio` 
#' package.
#' 
#' [rio()]'s [rio::export()] function currently only 
#' supports `data.frame`s; for lists [jsonlite::toJSON()] may 
#' be more appropriate.
#' 
#' Note: `box_write` is for writing files in standard formats to box.com.
#' To upload R objects as `.RData` files, see [box_save()].
#'
#' @inheritParams box_ul
#' @param x An R object
#' @param filename The name for the file to be uploaded
#' @param write_fun The function used to write the R object to a file
#' @param ... Additional arguments passed to `read_fun`
#' 
#' @return An object of class [boxr_file_reference][boxr_S3_classes].
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
