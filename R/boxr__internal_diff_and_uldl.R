#' Single Directory Operations for Downloading and Uploading mutiple Files
#' 
#' Download or upload the contents of a box.com directory, not including 
#' subdirectories
#' 
#' @aliases uploadDirFiles
#' 
#' @inheritParams dirTreeRecursive
#' 
#' @return \code{TRUE} for a successful sync, \code{NULL} if the box.com folder 
#' is empty.
#' @keywords internal
downloadDirFiles <- function(dir_id, local_dir = getwd(), overwrite = TRUE, 
                             dir_str = getwd()){
  
  box_dd <- box_dir_diff(dir_id, local_dir, load = "down")
  if(is.null(box_dd))
    return(NULL)
  
  if(!overwrite & nrow(box_dd$new) > 0)
    to_dl <- box_dd$new
  
  if(overwrite & nrow(box_dd$new) > 0)
    to_dl <- dplyr::bind_rows(box_dd$new, box_dd$to_update)
  
  if(!overwrite & nrow(box_dd$new) < 1)
    to_dl <- NULL
  
  if(overwrite & nrow(box_dd$new) < 1)
    to_dl <- box_dd$to_update
  
  dl_ids <- setNames(to_dl$id, to_dl$name)
  
  # Note, specifies filenames from the names of the dl_ids vector
  # to write straight to disk
  downloads <- list()
  
  if(length(dl_ids) > 0)
    for(i in 1:length(dl_ids)){
      catif(paste0(
        " in dir ", trimDir(dir_str)," downloading file (",i, "/", 
        length(dl_ids), "): ",  names(dl_ids[i])
      ))
      
      downloads[[i]] <-
        try(box_dl(dl_ids[i], filename = names(dl_ids[i]), overwrite = TRUE, 
                   local_dir = local_dir), silent = TRUE)
    }
  
  # An output object
  
  successful_downloads   <- unlist(downloads[class(downloads) != "try-error"])
  unsuccessful_downloads <- downloads[class(downloads) == "try-error"]
  
  # Retrieve the error messages for any failed downloads
  unsuccessful_downloads <- 
    unlist(lapply(unsuccessful_downloads, function(x) x[1]))
  
  out <- 
    list(
      successful_downloads   = successful_downloads,
      unsuccessful_downloads = unsuccessful_downloads,
      up_to_date             = paste0(local_dir, "/", box_dd$up_to_date$name)
    )
  
  return(out)
}


#' @rdname downloadDirFiles
uploadDirFiles <- function(dir_id, local_dir = getwd(), overwrite = TRUE){
  
  box_dd <- box_dir_diff(dir_id, local_dir, load = "up")
  if(is.null(box_dd))
    return(NULL)
  
  # Run through the files to update, and upload up dates
  # NOTE: insert messages/progress bars here
  updates <- list()
  uploads <- list()
  
  if(overwrite && nrow(box_dd$to_update) > 0)
    for(i in 1:nrow(box_dd$to_update)){
      catif(
        paste0(
          "Updating file (", i,"/",nrow(box_dd$to_update),"): ", 
          box_dd$to_update$name[i]
        )
      )
      updates[[i]] <- 
        box_update_file(
          box_dd$to_update$id[i],
          file.path(local_dir, box_dd$to_update$name[i]),
          dir_id
        )
    }
  
  # Run through the files to upload, and upload up dates
  # NOTE: insert messages/progress bars here
  if(nrow(box_dd$new) > 0)
    for(i in 1:nrow(box_dd$new)){
      catif(
        paste0(
          "Uploading new file (", i,"/",nrow(box_dd$new),"): ", 
          box_dd$new$name[i]
        )
      )
      uploads[[i]] <- 
        box_upload_new(dir_id, file.path(local_dir, box_dd$new$name[i]))
    }
  
  # An output object
  upload_success <- 
    unlist(
      lapply(uploads, function(x) httr::http_status(x)$category == "success")
    )
  
  update_success <- 
    unlist(
      lapply(updates, function(x) httr::http_status(x)$category == "success")
    )
  
  
  # Initialize these, for the sake of error handling
  successful_uploads <- unsuccessful_uploads <- successful_updates <- 
    unsuccessful_updates <- data.frame()
  
  
  if(length(upload_success) > 0){
    successful_uploads   <- box_dd$new[ upload_success]
    unsuccessful_uploads <- box_dd$new[!upload_success]
  }
  
  if(length(update_success) > 0){
    successful_updates     <- box_dd$to_update$names[ update_success]
    unsuccessful_updates   <- box_dd$to_update$names[!update_success]
  }
  
  out <- 
    list(
      successful_uploads   = successful_uploads,
      unsuccessful_uploads = unsuccessful_uploads,
      successful_updates   = successful_updates,
      unsuccessful_updates = unsuccessful_updates,
      up_to_date           = paste0(local_dir, "/", box_dd$up_to_date$name)
    )
  
  return(out)
}
