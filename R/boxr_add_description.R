#' Add description to a Box file
#' 
#' These functions will attach a description or comment to a Box file. A new
#' description will overwrite an existing one.
#' 
#' Files hosted at Box can have small text-descriptions that 
#' you can be use to annotate files, or even to
#' 
#' @inheritParams box_dl
#' 
#' @return Object with S3 class [`boxr_file_reference`][boxr_S3_classes].
#' 
#' @export
box_add_description <- function(file_id, description) {
  
  file_id <- as_box_id(file_id)
  file_id <- handle_file_id(file_id)
  
  req <- httr::RETRY(
    "PUT",
    paste0("https://api.box.com/2.0/files/", file_id),
    body = paste0('{"description":"', description, '"}'),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  httr::stop_for_status(req)
  add_file_ref_class(httr::content(req))
}
