#' Download and upload individual files from box.com
#' 
#' \code{box_dl} takes the \code{id} of a file hosted on box.com, downloads 
#' it and writes it to disk.
#' 
#' \code{box_read} does the same, but reads it into memory as an \code{R}
#' object. This can be useful, for example, to read a \code{.csv} file into
#' memory as a \code{\link{data.frame}}.
#' 
#' \code{box_ul} uploads a file stored locally to a specified box.com folder.
#' If a file with the same name already exists, it will store a new version of 
#' the file.
#' 
#' @aliases box_read
#' @param file_id The box.com id for the file that you'd like to download
#' @param overwrite \code{logical}. Should existing files with the same name be 
#' overwritten?
#' @param local_dir A file path to a local directory which you'd like the file
#' to be downloaded to.
#' @param filename Optional. An alternate filename for the local version of the 
#' file. The default, \code{NULL}, uses the name from box.com.
#' @param file the path to the local file that you'd like to upload (if there is
#'  one)
#' @param dir_id If uploading, the box.com folder id that you'd like to upload
#' to.
#' 
#' @inheritParams dirTreeRecursive
#' 
#' @return \code{TRUE}. Used for it's side-effect (a downloaded file)
#' @export
box_dl <- function(file_id, overwrite = FALSE, local_dir = getwd(), 
                   filename = NULL){
  
  checkAuth()
  
  # If the user's tried to upload a file reference object s3 class, help 'em out
  if(class(file_id) == "boxr_file_reference")
    file_id <- file_id$entries[[1]]$id
  
  if(is.null(filename))
    filename <- "TEMP"
  
  req <- 
    httr::GET(
      paste0(
        "https://api.box.com/2.0/files/",
        file_id, "/content"
      ),
      httr::config(token = getOption("boxr.token")),
      httr::write_disk(paste0(local_dir, "/", filename), overwrite)
    )
  
  # This coult be more informative, but would require more requests
  if(httr::http_status(req)$cat != "success"){
      stop(
        "Error downloading file id ", file_id, ": ", 
        httr::http_status(req)$message
      )
  }
    
  if(filename != "TEMP")
    return(paste0(local_dir, "/", filename))
  
  # If not supplied, extract filename from request headers
  # Extract filename
  filename <- 
    gsub(
      'filename=\"|\"', '',
      stringr::str_extract(
        req$headers["content-disposition"][[1]],
        'filename=\"(.*?)\"'
      )
    )
  
  # Rename the file if it's got a temporary name
  if(overwrite | !file.exists(filename))
    file.rename(
      paste0(local_dir, "/TEMP"),
      paste0(local_dir, "/", filename)
    )
  
  if(!overwrite & file.exists(filename))
    stop("File already exists, and overwrite = FALSE")
  
  paste0(local_dir, "/", filename)
}

#' @rdname box_dl
#' @export
box_ul <- function(file, dir_id = getOption("boxr.wd")$id){
  checkAuth()
  
  # First try and upload it
  ul_req <- box_upload_new(file, dir_id)
  
  # If uploading worked, end it here
  if(httr::http_status(ul_req)$cat == "success")
    # You should add an s3 class first
    return(httr::content(ul_req))
  
  # If it didn't work, because there's already a file with that name (http
  # error code 409), use the 'update' api
  if(httr::content(ul_req)$status == 409){
    message(
      "File '", basename(file),"' aleady exists. Attempting to upload new ",
      "version",
      " (V", 
      as.numeric(httr::content(ul_req)$context_info$conflicts$sequence_id) + 2,
      ")."
    )
    
    ud_req <- 
      box_update_file(
        file, 
        httr::content(ul_req)$context_info$conflicts$id,
        dir_id
      )
    
    # If updating worked, end it here
    if(httr::http_status(ud_req)$cat == "success")
      return(httr::content(ud_req))
    
    # If it doesn't, try to end as informatively as possible!
    ud_error_msg <- httr::content(ud_req)$context_info$errors[[1]]$message
    
    if(!is.null(ud_error_msg))
      stop(ud_error_msg)
    
    httr::stop_for_status(ud_req)
  }
  
  # If it doesn't, try to end as informatively as possible!
  ul_error_msg <- httr::content(ul_req)$context_info$errors[[1]]$message
  
  if(!is.null(ul_error_msg))
    stop(ul_error_msg)
  
  httr::stop_for_status(ul_req)
  
}

#' @rdname box_dl
#' @export
box_read <- function(file_id){
  checkAuth()
  
  req <- 
    httr::GET(
      paste0(
        "https://api.box.com/2.0/files/",
        file_id, "/content"
      ),
      httr::config(token = getOption("boxr.token"))
    )
  
  filename <- 
    gsub(
      'filename=\"|\"', '',
      stringr::str_extract(
        req$headers["content-disposition"][[1]],
        'filename=\"(.*?)\"'
      )
    )
  
  cont <- httr::content(req)
  if(is.raw(cont))
    warning(filename, " appears to be a binary file.")
  
  message(filename, " read into memory.\n")
  
  return(cont)
}

#' Save and load \code{R} workspaces via box.com
#' 
#' These convenience functions aim to provide analagous functionality to 
#' \code{\link[base]{load}} and \code{\link[base]{save.image}} (or 
#' \code{\link[base]{save}}), but for \code{.RData} files stored on box.com, as 
#' opposed to locally.
#' 
#' @aliases box_load
#' 
#' @param file_name The name you'd like your \code{.Rdata} file saved as. For
#' example, "myworkspace.RData"
#' @param objects Optional. A \code{\link[base]{list}} of \code{R} objects to be 
#' saved. If ommitted, all the objects in the current workspace will be saved.
#' @param dir_id The box.com folder id where the objects will be stored as a
#' \code{.RData} file.
#' @param file_id For \code{box_load}, the box.com id of the \code{.RData} or
#' \code{.rda} file you'd like to load into your workspace.
#' 
#' @details \code{box_save} saves an .RData file using 
#' \code{\link[base]{save.image}} if \code{objects} is not supplied or 
#' \code{\link[base]{save}} if it is. The file is then uploaded to box.com via 
#' \code{\link{box_ul}}.
#' 
#' \code{box_load} downloads a file from box.com using \code{\link{box_dl}},
#' and then \code{\link[base]{load}}s it into the current workspace.
#' 
#' @return \code{box_load} returns a character vector of the names of objects 
#' created, invisibly. \code{box_load} doesn't return anything.
#' @export
box_save <- function(file_name = ".RData", objects = character(), dir_id){
  checkAuth()
  
  temp_file <- file.path(tempdir(), file_name)
  
  if(length(objects) == 0L){
    save.image(temp_file)
  } else {
    save(objects, file = temp_file)
  }
  
  box_ul(temp_file, dir_id)
  
}

#' @rdname box_save
#' @export
box_load <- function(file_id){
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  load(temp_file)
}
