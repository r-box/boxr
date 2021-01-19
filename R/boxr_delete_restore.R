#' Move files within Box, from/to trash directory
#' 
#' In the Box context, deleting a file moves it to a special folder
#' within your Box account: 'Trash'. As of mid-2019, Box' default
#' [policy](https://support.box.com/hc/en-us/articles/360044196093-Manage-Trash)
#' is to retain files in Trash for 30 days.
#'
#' \describe{
#'   \item{`box_delete_file()`}{Move a file to Trash.}
#'   \item{`box_restore_file()`}{Restore a file from Trash.}
#'   \item{`box_delete_folder()`}{Move a folder, including contents, to Trash.}
#'   \item{`box_restore_folder()`}{Restore a folder, including contents, from Trash.}
#' }
#' 
#' @aliases box_delete_folder box_restore_file box_delete_folder
#' 
#' @inheritParams box_browse
#' @return \describe{
#'   \item{`box_delete_file()`}{Invisible `NULL`, called for side effects.}
#'   \item{`box_restore_file()`}{Object with S3 class [`boxr_file_reference`][boxr_S3_classes].}
#'   \item{`box_delete_folder()`}{Invisible `NULL`, called for side effects.}
#'   \item{`box_restore_folder()`}{Object with S3 class [`boxr_folder_reference`][boxr_S3_classes].}
#' }
#' 
#' @export
box_delete_file <- function(file_id) {
  boxDeleteFile(file_id)
  invisible(NULL)
}

#' @rdname box_delete_file
#' @export
box_restore_file <- function(file_id) {
  req <- httr::RETRY(
    "POST",
    paste0(
      "https://api.box.com/2.0/file/",
      file_id
    ),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  if (httr::http_status(req)$message == "Success: (201) Created")
    catif(paste0("file id ", file_id, " sucessfully restored from trash."))
  
  # You could add something here to try and anticipate what happened;
  # the messages from box aren't terribly informative.
  # e.g. you get a 403 if the folder already exists, which you really shouldn't
  httr::stop_for_status(req)
  add_file_ref_class(httr::content(req))
}

#' @rdname box_delete_file
#' @export
box_delete_folder <- function(dir_id) {
  boxDeleteFolder(dir_id)
  invisible(NULL)
}


#' @rdname box_delete_file
#' @export
box_restore_folder <- function(dir_id) {
  req <- httr::RETRY(
    "POST",
    paste0(
      "https://api.box.com/2.0/folders/",
      dir_id
    ),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  if (httr::http_status(req)$message == "Success: (201) Created")
    catif(paste0("dir id ", dir_id, " sucessfully restored from trash."))
  
  # You could add something here to try and anticipate what happened;
  # the messages from box aren't terribly informative.
  # e.g. you get a 403 if the folder already exists, which you really shouldn't
  httr::stop_for_status(req)
  add_folder_ref_class(httr::content(req))
}



# Internal versions -------------------------------------------------------
# The following are internal versions, needed to return additional information
# used by the dir-wide ops (e.g. deleteRemoteObjects used by box_push), not
# retained by the s3 classes

#' @keywords internal
boxDeleteFile <- function(file_id) {
  req <- httr::RETRY(
    "DELETE",
    paste0(
      "https://api.box.com/2.0/files/",
      file_id
    ),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  if (httr::http_status(req)$message == "Success: (204) No Content")
    catif(paste0(
      "file id ", file_id, " sucessfully moved to trash."
    ))
  
  httr::stop_for_status(req)
  req
}


#' @keywords internal
boxDeleteFolder <- function(dir_id) {
  req <- httr::RETRY(
    "DELETE",
    paste0(
      "https://api.box.com/2.0/folders/",
      dir_id, "?recursive=true"
    ),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  if (httr::http_status(req)$message == "Success: (204) No Content")
    catif(paste0(
      "folder id ", dir_id, " sucessfully moved to trash."
    ))
  
  httr::stop_for_status(req)
  req
}

