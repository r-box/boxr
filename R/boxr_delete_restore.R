#' Move files to and from the box.com trash folder
#' 
#' \code{box_delete_file} moves a file to the trash folder.
#' \code{box_delete_folder} moves a whole folder (and all of it's contents) to
#' the trash folder.
#' \code{box_restore_file} moves a file from the trash, to wherever it was
#' before it was 'deleted'/moved to trash.
#' \code{box_delete_folder} does the same thing for a folder.
#' 
#' 
#' @aliases box_read box_delete_folder box_restore_file box_delete_folder
#' @param file_id The box.com id for the file that you'd like delete/restore
#' @param dir_id The box.com id for the folder that you'd like delete/restore 
#' 
#' @return Nothing. Used for their side effects.
#' @export
box_delete_file <- function(file_id){
  req <- 
    httr::DELETE(
      paste0(
        "https://api.box.com/2.0/files/",
        file_id
      ),
      httr::config(token = getOption("boxr.token"))
    )
  
  if(httr::http_status(req)$message == "success: (204) No Content")
    catif(paste0(
      "file id ", file_id, " sucessfully moved to trash.\n If this was a mistake,",
      "see with box_restore_file(file_id)."
    ))
  
  httr::stop_for_status(req)
  req
}

#' @rdname box_delete_file
#' @export
box_delete_folder <- function(dir_id){
  req <- 
    httr::DELETE(
      paste0(
        "https://api.box.com/2.0/folders/",
        dir_id, "?recursive=true"
      ),
      httr::config(token = getOption("boxr.token"))
    )
  
  if(httr::http_status(req)$message == "success: (204) No Content")
    catif(paste0(
      "folder id ", dir_id, " sucessfully moved to trash.\n If this was a mistake,",
      "see box_restore_folder(dir_id)."
    ))
  
  httr::stop_for_status(req)
  req
}


#' @rdname box_delete_file
#' @export
box_restore_folder <- function(dir_id){
  req <- 
    httr::POST(
      paste0(
        "https://api.box.com/2.0/folders/",
        dir_id
      ),
      httr::config(token = getOption("boxr.token"))
    )
  
  if(httr::http_status(req)$message == "success: (201) Created")
    catif(paste0("dir id ", dir_id, " sucessfully restored from trash."))
  
  # You could add something here to try and anticipate what happened;
  # the messages from box aren't terribly informative.
  # e.g. you get a 403 if the folder already exists, which you really shouldn't
  httr::stop_for_status(req)
  req
}

#' @rdname box_delete_file
#' @export
box_restore_file <- function(file_id){
  req <- 
    httr::POST(
      paste0(
        "https://api.box.com/2.0/file/",
        file_id
      ),
      httr::config(token = getOption("boxr.token"))
    )
  
  if(httr::http_status(req)$message == "success: (201) Created")
    catif(paste0("file id ", file_id, " sucessfully restored from trash."))
  
  # You could add something here to try and anticipate what happened;
  # the messages from box aren't terribly informative.
  # e.g. you get a 403 if the folder already exists, which you really shouldn't
  httr::stop_for_status(req)
  req
}
