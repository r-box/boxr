#' Download and upload individual files from box.com
#' 
#' @description {
#'   Functions to download (\code{box_dl}), and upload (\code{box_ul}), files, 
#'   as well as read remote files straight into memory (\code{box_read}).
#' 
#'   \code{box_dl} takes the \code{id} of a file hosted on box.com, downloads 
#'     it and writes it to disk.
#' 
#'   \code{box_read} does the same, but reads it into memory as an \code{R}
#'     object. This can be useful, for example, to read a \code{.csv} file into
#'     memory as a \code{\link{data.frame}}.
#' 
#'   \code{box_ul} uploads a file stored locally to a specified box.com folder.
#'     If a file with the same name already exists, it will store a new version 
#'     of the file.
#' }
#' 
#' @aliases box_read
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
#' @param type Passed to \code{\link[httr]{content}}. MIME type (aka internet
#'   media type) used to override the content type returned by the server. See 
#'   http://en.wikipedia.org/wiki/Internet_media_type for a list of common types
#' 
#' @inheritParams dirTreeRecursive
#' 
#' @details
#'   \code{box_read} will attempt to coerce the remote file to an 
#'   \bold{\code{R}} object using httr's \code{\link[httr]{content}} function,
#'   which in general does a good job, especially converting \code{csv} files to
#'   a \code{\link{data.frame}}.
#'   
#'   However, at the time of writing, this isn't always successful with 
#'   JSON files, so \code{box_read} will try and convert any files with a 
#'   \code{.json} extension using \code{\link[jsonlite]{toJSON}}.
#' 
#' @return
#'   \code{box_dl} will return \code{TRUE} for a successful download, and throw 
#'     an error otherwise
#'   
#'   \code{box_ul} will return an object describing the new remote file
#' 
#'   \code{box_read} will reaturn an \bold{\code{R}} object
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
                   filename = NULL, version_id = NULL, version_no = NULL){
  
  checkAuth()
  assertthat::assert_that(assertthat::is.dir(local_dir))
  assertthat::assert_that(!is.na(overwrite))
  assertthat::assert_that(is.logical(overwrite))
  
  # If the user's supplied a filename that's already present 
  # & overwrite == FALSE, fail early
  if(!overwrite & !is.null(filename) && file.exists(filename))
    stop("File already exists locally, and overwrite = FALSE")
  
  # If the user's tried to upload a file reference object s3 class, help 'em out
  if(class(file_id) == "boxr_file_reference")
    file_id <- file_id$entries[[1]]$id
  
  # Get a temp file
  temp_file <- tempfile()
  
  # Dealing with versions
  if(!is.null(version_id) & !is.null(version_no)){
    # If both file_version & file_no are specified, fail informatively
    stop("Only one of version_id and version_no may be supplied")
  } 
  
  if(is.null(version_id) & !is.null(version_no)){
    
    # If just the version number was supplied, find its id
    # Check that version_no looks valid
    version_no <- as.numeric(version_no)
    assertthat::assert_that(is.numeric(version_no))
    
    versions <- box_previous_versions(file_id)
    
    # Check that the version number exists!
    if(version_no > (nrow(versions) + 1)){
      stop(paste("version_no supplied is higher than the number of versions 
                   that box.com has for file id", file_id))
    }
    
    # If the version number is for the current version, NULL it out for a normal
    # download
    if(version_no == (nrow(versions) + 1)){
      version_no <- NULL
    }
    
    # If the version number is for an old version, take the id
    if(!is.null(version_no) && version_no <= nrow(versions)){
      version_id <- versions$file_version_id[version_no]
    }
    
  }
  
  if(!is.null(version_id)){
    # If you have a version_id, check that it looks reasonable (11 digits)
    version_id <- as.numeric(version_id)
    
    if(!grepl("[[:digit:]]{11}", version_id))
      stop("version_id must be an integer, 11 characters in length")
    
    # The call with the version url parameter
    req <- 
      httr::GET(
        paste0(
          "https://api.box.com/2.0/files/",
          file_id, "/content", "?version=", version_id
        ),
        httr::config(token = getOption("boxr.token")),
        httr::write_disk(temp_file, TRUE)
      ) 
  } else {
    # The call without the version url parameter (e.g the latest version)
    req <- 
      httr::GET(
        paste0(
          "https://api.box.com/2.0/files/",
          file_id, "/content"
        ),
        httr::config(token = getOption("boxr.token")),
        httr::write_disk(temp_file, TRUE)
      )  
  }
  
  # This could be more informative, but would require more requests
  if(httr::http_status(req)$cat != "success"){
    stop(
      "Error downloading file id ", file_id, ": ", 
      httr::http_status(req)$message
    )
  }
  
  # Extract remote filename from request headers
  remote_filename <- 
    gsub(
      'filename=\"|\"', '',
      stringr::str_extract(
        req$headers["content-disposition"][[1]],
        'filename=\"(.*?)\"'
      )
    )
  
  if(is.null(filename))
    filename <- remote_filename
  
  # The full path for the new file
  new_file <- suppressWarnings(normalizePath(paste0(local_dir, "/", filename)))
  
  # If the filetype has changed, let them know
  ext <- function(x){
    y <- strsplit(x, "\\.")[[1]]
    y[length(y)]
  } 
  
  if(ext(remote_filename) != ext(new_file))
    warning("Different local and remote file extensions")
  
  # Stop if you can't overwrite an existing file
  if(!overwrite & file.exists(new_file))
    stop("File already exists locally, and overwrite = FALSE")
  
  # Move the data from the temp file, to it's new local home
  cp <- file.copy(temp_file, new_file, overwrite = TRUE, recursive = FALSE)
  
  # Stop if the copy operation failed
  if(!cp)
    stop("Problem writing file to ", new_file, 
         ".\n Check that directory is writable.")
  
  # Remove the tempfile to free up space
  file.remove(temp_file)
  
  return(new_file)
}


#' @rdname box_dl
#' @export
box_ul <- function(dir_id = box_getwd(), file){
  checkAuth()
  
  add_class <- function(x){
    class(x) <- "boxr_file_reference"
    x
  }
  
  # First try and upload it
  ul_req <- box_upload_new(dir_id, file)
  
  # If uploading worked, end it here
  if(httr::http_status(ul_req)$cat == "success")
    return(add_class(httr::content(ul_req)))
  
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
        httr::content(ul_req)$context_info$conflicts$id,
        file, 
        dir_id
      )
    
    # If updating worked, end it here
    if(httr::http_status(ud_req)$cat == "success")
      return(add_class(httr::content(ud_req)))
    
    # If it doesn't, try to end informatively
    ud_error_msg <- httr::content(ud_req)$context_info$errors[[1]]$message
    
    if(!is.null(ud_error_msg))
      stop(ud_error_msg)
    
    httr::stop_for_status(ud_req)
  }
  
  # If it doesn't, try to end as informatively
  ul_error_msg <- httr::content(ul_req)$context_info$errors[[1]]$message
  
  if(!is.null(ul_error_msg))
    stop(ul_error_msg)
  
  httr::stop_for_status(ul_req)
  
}

#' @rdname box_dl
#' @export
box_read <- function(file_id, type = NULL){
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
  
  # Currently, httr works well with .csv files, but doesn't to a great job with
  # json.
  probably_json <- grepl("\\.json$", filename)
  
  if(is.null(type) & probably_json){
    cont <- jsonlite::fromJSON(httr::content(req, as = "text"))
  } else {
    cont <- httr::content(req, type = type)
  }
  
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
#' @param ... The objects to be saved. Quoted or unquoted. Passed to 
#'   \code{\link{save}}.
#' @param dir_id The box.com folder id where the objects will be stored as a
#'   \code{.RData} file.
#' @param file_name The name you'd like your \code{.Rdata} file saved as. For
#'   example, "myworkspace.RData"
#' @param file_id For \code{box_load}, the box.com id of the \code{.RData} or
#'   \code{.rda} file you'd like to load into your workspace.
#'
#' @details \code{box_save} saves an .RData file using 
#'   \code{\link[base]{save.image}} if \code{objects} is not supplied or 
#'   \code{\link[base]{save}} if it is. The file is then uploaded to box.com via 
#'   \code{\link{box_ul}}.
#' 
#'   \code{box_load} downloads a file from box.com using \code{\link{box_dl}},
#'   and then \code{\link[base]{load}}s it into the current workspace.
#' 
#' @return \code{box_load} returns a character vector of the names of objects 
#'   created, invisibly. \code{box_save} and \code{box_save_image} are used for 
#'   their side effects, and doen't return anything.
#'   
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' 
#' @seealso The base R functions which these wrap; \code{\link{save}},
#'   \code{\link{save.image}}, \code{\link{load}}, and \code{\link{source}}
#'   
#' @export
box_save <- function(..., dir_id = box_getwd(), file_name = ".RData"){
  temp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  save(..., file = temp_file)
  box_ul(dir_id, temp_file)
}

#' @rdname box_save
#' @export
box_save_image <- function(dir_id = box_getwd(), file_name = ".RData", ...){
  temp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  save.image(file = temp_file, ...)
  
  box_ul(dir_id, temp_file)
}

#' @rdname box_save
#' @export
box_load <- function(file_id){  
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  load(temp_file, envir = globalenv())
}


#' @rdname box_save
#' @export
box_source <- function(file_id){
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  source(temp_file, local = globalenv())
}
