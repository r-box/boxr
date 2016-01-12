#' Download and upload individual files from box.com
#' 
#' @description {
#'   Functions to download (\code{box_dl}), and upload (\code{box_ul}).
#' 
#'   \code{box_dl} takes the \code{id} of a file hosted on box.com, downloads 
#'     it and writes it to disk.
#' 
#'   \code{box_ul} uploads a file stored locally to a specified box.com folder.
#'     If a file with the same name already exists, it will upload the file as
#'     a new version.
#' }
#' 
#' @section Versions:
#'   \describe{
#'     \code{box_dl} can accept one of two parameters to specify file versions:
#'     \bold{\code{version_id}} and \bold{\code{version_no}}.
#'     
#'     The box.com API refers to file versions using 11 digit ids (which can be
#'     accessed via \code{\link{box_previous_versions}}) - you can specify these
#'     using the \code{version_id} parameter.
#'     
#'     However, this isn't terribly intuative. As a result, \code{box_dl} 
#'     provides the \code{version_no} parameter, which accepts a whole number, 
#'     and corresponds to the versions that you'll see via the web UI. For 
#'     example to download the version marked 'V2' on box.com, specify
#'     \code{version_no = 2}. This works by making an internal call to 
#'     \code{\link{box_previous_versions}} to retrieve the \code{version_id},
#'     which makes it slightly slower.
#'   }
#' 
#' @param file_id The box.com id for the file that you'd like to download
#' @param overwrite \code{logical}. Should existing files with the same name be 
#'   overwritten?
#' @param local_dir A file path to a local directory which you'd like the file
#'   to be downloaded to.
#' @param filename Optional. An alternate filename for the local version of the 
#'   file. The default, \code{NULL}, uses the name from box.com.
#' @param file the path to the local file that you'd like to upload (if there is
#'  one)
#' @param dir_id If uploading, the box.com folder id that you'd like to upload
#'   to.
#' @param version_id If downloading an older version, the \code{version_id} of 
#'   the desired file
#' @param version_no The version of the file you'd like to download (starting at
#'   1)
#' @param pb Should a progress bar be shown? (via 
#'   \code{\link{setTxtProgressBar}})
#' @param description Optional. \code{character}. A string to be used as the
#'   description caption for the file (added via 
#'   \code{\link{box_add_description}}). Useful for describing the contents of a
#'   file, or describing the latest changes made to it. If \code{NULL} (the 
#'   default), no description is added.
#' 
#' @return
#'   \code{box_dl} returns the path of the newly downloaded file if successful,
#'     and throw an error otherwise.
#'   
#'   \code{box_ul} will return an object of class 
#'   \code{\link[=boxr_S3_classes]{boxr_file_reference}}
#' 
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' 
#' @seealso \code{\link{box_fetch}} and \code{\link{box_push}} for 
#'   directory-wide equivalents, \code{\link{box_delete_file}} for removing 
#'   uploaded files, \code{\link{box_source}} for R code, and 
#'   \code{\link{box_save}}/\code{\link{box_load}} for remote R objects.
#' 
#' @export
box_dl <- function(file_id, local_dir = getwd(), overwrite = FALSE, 
                   filename = NULL, version_id = NULL, version_no = NULL,
                   pb = options()$boxr.progress) {
  
  checkAuth()
  assertthat::assert_that(assertthat::is.dir(local_dir))
  assertthat::assert_that(!is.na(overwrite))
  assertthat::assert_that(is.logical(overwrite))
  
  # If the user's supplied a filename that's already present 
  # & overwrite == FALSE, fail early
  if (!overwrite & !is.null(filename) && file.exists(filename))
    stop("File already exists locally, and overwrite = FALSE")
  
  # Get a temp file
  temp_file <- tempfile()
  
  # Download to a tempfile with boxGet
  req <- boxGet(file_id = file_id, version_id = version_id,
                version_no = version_no, download = TRUE, 
                local_file = temp_file, pb = pb)

  # Extract remote filename from request headers
  remote_filename <- gsub(
    'filename=\"|\"', '',
    stringr::str_extract(
      req$headers["content-disposition"][[1]],
      'filename=\"(.*?)\"'
    )
  )
  
  # If the user hasn't supplied a filename, use the remote one
  if (is.null(filename))
    filename <- remote_filename
  
  # The full path for the new file
  new_file <- suppressWarnings(normalizePath(paste0(local_dir, "/", filename)))
  
  # If the filetype has changed, let them know
  ext <- function(x) {
    ext <- stringr::str_extract(gsub(".*\\/|.*\\\\", "", x), "\\..*$")
    ifelse(is.na(ext), "", ext)
  } 
  
  if (ext(remote_filename) != ext(new_file))
    warning("Different local and remote file extensions")
  
  # Stop if you can't overwrite an existing file
  if (!overwrite & file.exists(new_file))
    stop("File already exists locally, and overwrite = FALSE")
  
  # Move the data from the temp file, to it's new local home
  cp <- file.copy(temp_file, new_file, overwrite = TRUE, recursive = FALSE)
  
  # Stop if the copy operation failed
  if (!cp)
    stop("Problem writing file to ", new_file, 
         ".\n Check that directory is writable.")
  
  # Remove the tempfile to free up space
  file.remove(temp_file)
  
  return(new_file)
}

#' @rdname box_dl
#' @inheritParams box_add_description
#' @export
box_ul <- function(dir_id = box_getwd(), file, pb = options()$boxr.progress,
                   description = NULL) {
  checkAuth()
  
  # Validate filename
  file <- box_filename(file)
  
  # First try and upload it
  ul_req <- box_upload_new(dir_id, file, pb = pb)
  
  # If uploading worked, end it here
  if (httr::http_status(ul_req)$cat == "Success")
    return(add_file_ref_class(httr::content(ul_req)$entries[[1]]))
  
  # If it didn't work, because there's already a file with that name (http
  # error code 409), use the 'update' api
  if (httr::content(ul_req)$status == 409) {
    message(
      "File '", basename(file),"' aleady exists. Attempting to upload new ",
      "version",
      " (V", 
      as.numeric(httr::content(ul_req)$context_info$conflicts$sequence_id) + 2,
      ")."
    )
    
    ud_req <- box_update_file(httr::content(ul_req)$context_info$conflicts$id,
                              file, dir_id, pb = pb)
    
    # If updating worked...
    if (httr::http_status(ud_req)$cat == "Success") {
      out <- add_file_ref_class(httr::content(ud_req)$entries[[1]])
      
      if (is.null(description)) {
        # If there's no message to add, return the classed object
        return(out)
      } else {
        # Otherwise, add a description
        return(box_add_description(out, description))
      }
    }
      
    
    # If it doesn't, try to end informatively
    ud_error_msg <- httr::content(ud_req)$context_info$errors[[1]]$message
    
    if (!is.null(ud_error_msg))
      stop(ud_error_msg)
    
    httr::stop_for_status(ud_req)
  }
  
  # If it doesn't, try to end as informatively
  ul_error_msg <- httr::content(ul_req)$context_info$errors[[1]]$message
  
  if (!is.null(ul_error_msg))
    stop(ul_error_msg)
  
  httr::stop_for_status(ul_req)
}
