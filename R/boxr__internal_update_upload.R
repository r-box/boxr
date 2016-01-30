#' Upload or update a file
#' 
#' Internal functions used by \code{\link{box_ul}} (who's use is recommended as
#' an alternative to these functions).
#' 
#' The box.com api requires different API calls to upload a new file, and to
#' upload a new version of a file which already exists (incrementing the version
#' number).
#' 
#' \code{box_upload_new} make the API call to upload a new file.
#' \code{box_update_file} makes the API call to update an existing file.
#' 
#' @aliases box_update_file
#' @param file_id the box.com id of the file you'd like to update
#' @param file A path to a file stored locally
#' @param dir_id The box.com id for the folder that you'd like to upload to
#' @return The \code{\link{httr}} object returned by the api call
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' @keywords internal
box_upload_new <- function(dir_id, file, pb = FALSE) {
  httr::POST(
    "https://upload.box.com/api/2.0/files/content",
    httr::config(token = getOption("boxr.token")),
    encode = "multipart",
    if (pb)
      httr::progress(),
    body = 
      list(
        attributes = 
          paste0(
            '{"name": "', basename(file), '", "parent": {"id":"', box_id(dir_id)
            ,'"}}'
          ),
        file = httr::upload_file(file)
      )
  )
}

#' @rdname box_upload_new
#' @keywords internal
box_update_file <- function(file_id, file, dir_id, pb = FALSE) {
  httr::POST(
    paste0("https://upload.box.com/api/2.0/files/", box_id(file_id), 
           "/content"),
    httr::config(token = getOption("boxr.token")),
    encode = "multipart",
    if (pb)
      httr::progress(),
    body = 
      list(
        attributes = 
          paste0(
            '{"name": "', basename(file), '", "parent": {"id":"', box_id(dir_id)
            ,'"}}'
          ),
        file = httr::upload_file(file)
      )
  )
}
