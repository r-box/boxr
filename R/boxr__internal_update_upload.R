#' Upload or update a file
#' 
#' Internal functions used by [box_ul()] (who's use is recommended as
#' an alternative to these functions).
#' 
#' The box.com api requires different API calls to upload a new file, and to
#' upload a new version of a file which already exists (incrementing the version
#' number).
#' 
#' `box_upload_new` make the API call to upload a new file.
#' `box_update_file` makes the API call to update an existing file.
#' 
#' @aliases box_update_file
#' @param file_id the box.com id of the file you'd like to update
#' @param file A path to a file stored locally
#' @param dir_id The box.com id for the folder that you'd like to upload to
#' @return The [httr()] object returned by the api call
#' @keywords internal
#' 
box_upload_new <- function(dir_id, file, pb = FALSE) {
  
  dir_id <- as_box_id(dir_id)
  
  httr::RETRY(
    "POST",
    "https://upload.box.com/api/2.0/files/content",
    get_token(),
    encode = "multipart",
    if (pb)
      httr::progress(),
    body = 
      list(
        attributes = 
          paste0(
            '{"name": "', basename(file), '", "parent": {"id":"', dir_id
            ,'"}}'
          ),
        file = httr::upload_file(file)
      ),
    terminate_on = box_terminal_http_codes()
  )
}

#' @rdname box_upload_new
#' @keywords internal
box_update_file <- function(file_id, file, dir_id, pb = FALSE) {
  
  dir_id <- as_box_id(dir_id)
  file_id <- as_box_id(file_id)
  
  httr::RETRY(
    "POST",
    paste0("https://upload.box.com/api/2.0/files/", file_id, 
           "/content"),
    get_token(),
    encode = "multipart",
    if (pb)
      httr::progress(),
    body = 
      list(
        attributes = 
          paste0(
            '{"name": "', basename(file), '", "parent": {"id":"', dir_id
            ,'"}}'
          ),
        file = httr::upload_file(file)
      ),
    terminate_on = box_terminal_http_codes()
  )
}
