#' Issue a get request for a file stored on box.com
#' 
#' This internal function is shared by \code{\link{box_dl}}, and the 
#' \code{\link{box_read}} family of functions, to issue GET requests, while
#' handling things like version numbers etc. It can be used to download a file,
#' or just read it into memory.
#' 
#' 
#' @keywords internal
boxGet <- function(file_id, local_file, version_id = NULL, version_no = NULL,
                   download = FALSE) {
  
  # Dealing with versions
  if (!is.null(version_id) & !is.null(version_no)) {
    # If both file_version & file_no are specified, fail informatively
    stop("Only one of version_id and version_no may be supplied")
  } 
  
  if (is.null(version_id) & !is.null(version_no)) {
    # If just the version number was supplied, find its id
    # Check that version_no looks valid
    version_no <- as.numeric(version_no)
    assertthat::assert_that(is.numeric(version_no))
    
    versions <- box_previous_versions(file_id)
    
    # Check that the version number exists!
    if (version_no > (nrow(versions) + 1)) {
      stop(paste("version_no supplied is higher than the number of versions 
                 that box.com has for file id", file_id))
    }
    
    # If the version number is for the current version, NULL it out for a normal
    # download
    if (version_no == (nrow(versions) + 1)) {
      version_no <- NULL
    }
    
    # If the version number is for an old version, take the id
    if (!is.null(version_no) && version_no <= nrow(versions)) {
      version_id <- versions$file_version_id[version_no]
    }
  }
  
  if (!is.null(version_id)) {
    # If you have a version_id, check that it looks reasonable (11 digits)
    version_id <- as.numeric(version_id)
    
    if (!grepl("[[:digit:]]{11}", version_id))
      stop("version_id must be an integer, 11 characters in length")
    
    # The call with the version url parameter
    req <- httr::GET(
      paste0(
        "https://api.box.com/2.0/files/",
        file_id, "/content", "?version=", version_id
      ),
      if (download)
        httr::write_disk(local_file, TRUE),
      httr::config(token = getOption("boxr.token"))
    ) 
  } else {
    # The call without the version url parameter (e.g the latest version)
    req <- httr::GET(
      paste0(
        "https://api.box.com/2.0/files/",
        file_id, "/content"
      ),
      if (download)
        httr::write_disk(local_file, TRUE),
      httr::config(token = getOption("boxr.token"))
    ) 
  }
  
  # This could be more informative, but would require more requests
  if (httr::http_status(req)$cat != "success") {
    stop("Error downloading file id ", file_id, ": ", 
         httr::http_status(req)$message)
  }
  
  return(req)
}
