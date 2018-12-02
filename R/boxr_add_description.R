#' Add a description to a file hosted remotely on box.com
#' 
#' Files hosted on box.com can have small descriptions underneath their
#' filenames. This can be useful to explain the contents of the file, or even to
#' leave 'git commit' style messages about the latest changes made to them.
#' 
#' @md
#' @inheritParams box_dl
#' @param description The description which you'd like to add to the file. 
#'   [character()].
#'   
#' @return An object of class [boxr_file_reference][boxr_S3_classes].
#'
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
#' 
#' @export
box_add_description <- function(file_id, description) {
  file_id <- handle_file_id(file_id)
  
  req <- httr::PUT(
    paste0("https://api.box.com/2.0/files/", file_id),
    body = paste0('{"description":"', description, '"}'),
    httr::config(token = getOption("boxr.token"))
  )
  
  httr::stop_for_status(req)
  add_file_ref_class(httr::content(req))
}
