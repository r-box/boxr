#' Add description to Box file
#' 
#' Files hosted at Box can have small descriptions. 
#' These can be useful to explain the contents of files, or even to
#' leave 'git commit' style messages about the latest changes made to them.
#' 
#' @inheritParams box_dl
#'   
#' @return S3 object with class [boxr_file_reference][boxr_S3_classes].
#'
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
#' 
#' @export
#' 
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
