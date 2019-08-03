#' Add description to Box file
#' 
#' This function will attach a description to a Box file; it will
#' overwrite the Box file's existing description.
#' 
#' Files hosted at Box can have small text-descriptions that 
#' you can be use to annotate files, or even to
#' leave 'git commit' style messages.
#' 
#' @inheritParams box_dl
#' @return Object with S3 class [`boxr_file_reference`][boxr_S3_classes]
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
