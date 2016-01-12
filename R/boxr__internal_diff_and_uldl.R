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
                             dir_str = getwd()) {
  
  box_dd <- box_dir_diff(dir_id, local_dir, load = "down", folders = FALSE)
  if (is.null(box_dd))
    return(NULL)
  
  if (!overwrite & nrow(box_dd$new) > 0)
    to_dl <- box_dd$new
  
  if (overwrite & nrow(box_dd$new) > 0)
    to_dl <- dplyr::bind_rows(box_dd$new, box_dd$to_update)
  
  if (!overwrite & nrow(box_dd$new) < 1)
    to_dl <- NULL
  
  if (overwrite & nrow(box_dd$new) < 1)
    to_dl <- box_dd$to_update
  
  # Note, specifies filenames from the names of the dl_ids vector
  # to write straight to disk
  downloads <- list()
  
  if (length(to_dl$id) > 0)
    for (i in 1:length(to_dl$id)) {
      catif(paste0(
        " in dir ", trimDir(dir_str)," downloading file (",i, "/", 
        length(to_dl$id), "): ",  names(to_dl$name[i])
      ))
      
      downloads[[i]] <-
        try(box_dl(to_dl$id[i], filename = names(to_dl$id[i]), overwrite = TRUE, 
                   local_dir = local_dir, pb = FALSE), silent = TRUE)
    }
  
  # An output object
  successful_downloads   <- 
    to_dl[unlist(lapply(downloads, class)) != "try-error",]
  
  unsuccessful_downloads <- 
    to_dl[unlist(lapply(downloads, class)) == "try-error",]
  
  # Up-to-date: files only
  up_to_date <- box_dd$up_to_date[box_dd$up_to_date$type == 'file',]
  
  out <- 
    list(
      successful_downloads   = successful_downloads,
      unsuccessful_downloads = unsuccessful_downloads,
      up_to_date             = up_to_date
    )
  
  return(out)
}


#' @rdname downloadDirFiles
uploadDirFiles <- function(dir_id, local_dir = getwd(), overwrite = TRUE) {
  
  box_dd <- box_dir_diff(dir_id, local_dir, load = "up")
  if (is.null(box_dd))
    return(NULL)
  
  # Run through the files to update, and upload up dates
  updates <- list()
  uploads <- list()
  
  if (overwrite && nrow(box_dd$to_update) > 0)
    for (i in 1:nrow(box_dd$to_update)) {
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
          dir_id,
          pb = FALSE
        )
    }
  
  # Run through the files to upload, and upload up dates
  if (nrow(box_dd$new) > 0)
    for (i in 1:nrow(box_dd$new)) {
      catif(
        paste0(
          "Uploading new file (", i,"/",nrow(box_dd$new),"): ", 
          box_dd$new$name[i]
        )
      )
      uploads[[i]] <- 
        box_upload_new(dir_id, file.path(local_dir, box_dd$new$name[i]),
                       pb = FALSE)
    }
  
  # An output object
  upload_success <- 
    unlist(
      lapply(uploads, function(x) httr::http_status(x)$category == "Success")
    )
  
  update_success <- 
    unlist(
      lapply(updates, function(x) httr::http_status(x)$category == "Success")
    )
  
  
  # Initialize these, for the sake of error handling
  successful_uploads <- unsuccessful_uploads <- successful_updates <- 
    unsuccessful_updates <- data.frame()
  
  
  if (length(upload_success) > 0) {
    successful_uploads   <- box_dd$new[ upload_success,]
    unsuccessful_uploads <- box_dd$new[!upload_success,]
  }
  
  if (length(update_success) > 0) {
    successful_updates     <- box_dd$to_update[ update_success,]
    unsuccessful_updates   <- box_dd$to_update[!update_success,]
  }
  
  # Up-to-date: files only
  up_to_date <- box_dd$up_to_date[box_dd$up_to_date$type == 'file',]
  
  out <- 
    list(
      successful_uploads   = successful_uploads,
      unsuccessful_uploads = unsuccessful_uploads,
      successful_updates   = successful_updates,
      unsuccessful_updates = unsuccessful_updates,
      up_to_date           = up_to_date
    )
  
  return(out)
}
