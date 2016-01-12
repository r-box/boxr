#' Move files to and from the box.com trash folder
#' 
#' \itemize{
#'   \item \code{box_delete_file} moves a file to the trash folder.
#'   \item \code{box_delete_folder} moves a whole folder (and all of it's 
#'     contents) to the trash folder.
#'   \item \code{box_restore_file} moves a file from the trash, to wherever it 
#'     was before
#'   \item \code{box_delete_folder} does the same thing for a folder.
#' }
#' 
#' @aliases box_delete_folder box_restore_file box_delete_folder
#' @param file_id The box.com id for the file that you'd like delete/restore
#' @param dir_id The box.com id for the folder that you'd like delete/restore 
#' 
#' @details 'Deleting' a file in this case means moving it to a special folder
#'   within your box.com account called the 'Trash'. At the time of writing,
#'   the files are stored for three months before being deleted, and the 
#'   contents of the folder can be accessed from the top right hand menu of the 
#'   web interface.
#' 
#' @return An object of class 
#'   \code{\link[=boxr_S3_classes]{boxr_file_reference}}.
#' 
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' 
#' @seealso \code{\link{box_ul}}, for putting files there in the first place
#' 
#' @export
box_delete_file <- function(file_id) {
  add_file_ref_class(httr::content(boxDeleteFile(file_id)))
}


#' @rdname box_delete_file
#' @export
box_delete_folder <- function(dir_id) {
  add_folder_ref_class(httr::content(boxDeleteFolder(dir_id)))
}


#' @rdname box_delete_file
#' @export
box_restore_folder <- function(dir_id) {
  req <- httr::POST(
    paste0(
      "https://api.box.com/2.0/folders/",
      dir_id
    ),
    httr::config(token = getOption("boxr.token"))
  )
  
  if (httr::http_status(req)$message == "Success: (201) Created")
    catif(paste0("dir id ", dir_id, " sucessfully restored from trash."))
  
  # You could add something here to try and anticipate what happened;
  # the messages from box aren't terribly informative.
  # e.g. you get a 403 if the folder already exists, which you really shouldn't
  httr::stop_for_status(req)
  add_folder_ref_class(httr::content(req))
}


#' @rdname box_delete_file
#' @export
box_restore_file <- function(file_id) {
  req <- httr::POST(
    paste0(
      "https://api.box.com/2.0/file/",
      file_id
    ),
    httr::config(token = getOption("boxr.token"))
  )
  
  if (httr::http_status(req)$message == "Success: (201) Created")
    catif(paste0("file id ", file_id, " sucessfully restored from trash."))
  
  # You could add something here to try and anticipate what happened;
  # the messages from box aren't terribly informative.
  # e.g. you get a 403 if the folder already exists, which you really shouldn't
  httr::stop_for_status(req)
  add_file_ref_class(httr::content(req))
}


# Internal versions -------------------------------------------------------
# The following are internal versions, needed to return additional information
# used by the dir-wide ops (e.g. deleteRemoteObjects used by box_push), not
# retained by the s3 classes

#' @keywords internal
boxDeleteFile <- function(file_id) {
  req <- httr::DELETE(
    paste0(
      "https://api.box.com/2.0/files/",
      file_id
    ),
    httr::config(token = getOption("boxr.token"))
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
  req <- httr::DELETE(
    paste0(
      "https://api.box.com/2.0/folders/",
      dir_id, "?recursive=true"
    ),
    httr::config(token = getOption("boxr.token"))
  )
  
  if (httr::http_status(req)$message == "Success: (204) No Content")
    catif(paste0(
      "folder id ", dir_id, " sucessfully moved to trash."
    ))
  
  httr::stop_for_status(req)
  req
}

