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
#' @param file A path to a file stored locally
#' @param file_id the box.com id of the file you'd like to update
#' @param dir_id The box.com id for the folder that you'd like to upload to
#' @return The \code{\link{httr}} object returned by the api call
#' @keywords internal
box_upload_new <- function(file, dir_id, pb = FALSE){
  httr::POST(
    "https://upload.box.com/api/2.0/files/content",
    httr::config(token = getOption("boxr.token")),
    encode = "multipart",
    if(pb)
      httr::progress(),
    body = 
      list(
        attributes = 
          paste0(
            '{"name": "', basename(file), '", "parent": {"id":"', dir_id,'"}}'
          ),
        file       = httr::upload_file(file)
      )
  )
}

#' @rdname box_upload_new
#' @keywords internal
box_update_file <- function(file, file_id, dir_id, pb = FALSE){
  httr::POST(
    paste0("https://upload.box.com/api/2.0/files/", file_id, "/content"),
    httr::config(token = getOption("boxr.token")),
    encode = "multipart",
    if(pb)
      httr::progress(),
    body = 
      list(
        attributes = 
          paste0(
            '{"name": "', basename(file), '", "parent": {"id":"', dir_id,'"}}'
          ),
        file = httr::upload_file(file)
      )
  )
}

#' Create a data.frame of metadata of the contents of a local directory
#' 
#' @inheritParams dirTreeRecursive
#' @return A data.frame of metadata.
#' @keywords internal
create_loc_dir_df <- function(local_dir = getwd()){
  fs <- list.files(local_dir, full.names = TRUE)
  if(length(fs) < 1L)
    return(NULL)
  
  # Create a data.frame of metadata on the contents
  # of a local directory
  # You need to make this fail gracefully if the home directory is
  # empty (e.g.) return null
  fi <- file.info(fs)
  
  df <- 
    data.frame(
      name = fs,
      fi,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  
  df$type <- ifelse(df$isdir, "folder", "file")
  df$sha1 <- NA
  
  if(sum(!df$isdir) > 0L){
    sha1 <-
      sapply(
        df$name[!df$isdir], 
        function(f)
          digest::digest(f, "sha1", file = TRUE)
      )
    
    df$sha1 <- sha1[df$name]
  }
  
  return(df)
}

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
  
  loc_dir_df <- create_loc_dir_df(local_dir)
  box_dir_df <- box_ls(dir_id)
  
  # Lists of the local and box files
  bf <- box_dir_df$name[box_dir_df$type == "file"]
  if(length(bf) < 1L)
    return(NULL)
  
  lf <- gsub(".*/", "", loc_dir_df$name[loc_dir_df$type == "file"])
  
  absent  <- base::setdiff(bf, lf)
  present <- base::intersect(bf, lf)
  
  # Do any of the existing files need updating?
  to_update <- vector()
  
  if(length(lf) > 0L){
    b_sha1 <- setNames(box_dir_df$sha1[box_dir_df$type == "file"], bf)
    l_sha1 <- setNames(loc_dir_df$sha1[loc_dir_df$type == "file"], lf)
    
    to_update  <- present[!l_sha1[present] == b_sha1[present]]
    up_to_date <- present[ l_sha1[present] == b_sha1[present]]
  }
  
  # If overwrite is false, ignore to_update
  if(!overwrite)
    to_update <- ""
  
  # The ids of files to download
  dl_ids <- setNames(box_dir_df$id, box_dir_df$name)[c(absent, to_update)]
  
  # Note, specifies filenames from the names of the dl_ids vector
  # to write straight to disk
  downloads <- list()
  
  if(length(dl_ids) > 0)
    for(i in 1:length(dl_ids)){
      catif(paste0(
        "\r in dir ", trimDir(dir_str)," downloading file (",i, "/", 
        length(dl_ids), "): ",  names(dl_ids[i]), "\r"
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
      up_to_date             = paste0(local_dir, "/", up_to_date)
    )
  
  return(out)
}

#' @rdname downloadDirFiles
uploadDirFiles <- function(dir_id, local_dir = getwd(), overwrite = TRUE){
  
  loc_dir_df <- create_loc_dir_df(local_dir)
  box_dir_df <- box_ls(dir_id)
  
  # Lists of the local and box files
  bf <- box_dir_df$name[box_dir_df$type == "file"]
  
  lf <- gsub(".*/", "", loc_dir_df$name[loc_dir_df$type == "file"])
  if(length(lf) < 1L)
    return(NULL)
  
  absent  <- base::setdiff(lf, bf)
  present <- base::intersect(bf, lf)
  
  # Do any of the existing files need updating?
  to_update <- vector()
  
  if(length(bf) > 0L){
    b_sha1 <- setNames(box_dir_df$sha1[box_dir_df$type == "file"], bf)
    l_sha1 <- setNames(loc_dir_df$sha1[loc_dir_df$type == "file"], lf)
    
    to_update  <- present[!l_sha1[present] == b_sha1[present]]
    up_to_date <- present[ l_sha1[present] == b_sha1[present]]
  }
  
  
  update_names <- box_dir_df$name[box_dir_df$name %in% to_update]
  update_ids   <- box_dir_df$id[box_dir_df$name %in% to_update]
  
  # Run through the files to update, and upload up dates
  # NOTE: insert messages/progress bars here
  updates <- list()
  uploads <- list()
  
  
  if(overwrite & length(update_names) > 0)
    for(i in 1:length(update_names)){
      catif(
        paste0(
          "\rUpdating file (", i,"/",length(update_names),"): ", 
          update_names[i],
          "\r"
        )
      )
      updates[[i]] <- 
        box_update_file(
          file.path(local_dir, update_names[i]),
          update_ids[i],
          dir_id
        )
    }
      
  # Run through the files to upload, and upload up dates
  # NOTE: insert messages/progress bars here
  if(length(absent) > 0)
    for(i in 1:length(absent)){
      catif(
        paste0(
          "\rUploading new file (", i,"/",length(absent),"): ", absent[i],
          "\r"
        )
      )
      uploads[[i]] <- 
        box_upload_new(file.path(local_dir, absent[i]), dir_id)
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


  if(length(absent) > 0){
    successful_uploads   <- absent[ upload_success]
    unsuccessful_uploads <- absent[!upload_success]
  }
  
  if(length(update_names) > 0){
    successful_updates     <- update_names[ update_success]
    unsuccessful_updates   <- update_names[!update_success]
  }
  
  out <- 
    list(
      successful_uploads   = successful_uploads,
      unsuccessful_uploads = unsuccessful_uploads,
      successful_updates   = successful_updates,
      unsuccessful_updates = unsuccessful_updates,
      up_to_date           = paste0(local_dir, "/", up_to_date)
    )
  
  return(out)
}

# Something for keeping dir strings a constant length for calls to cat
trimDir <- function(x, limit = 25){
  n <- nchar(x)
  if(n > limit)
    return(paste0("...", substr(x, n - limit + 3, n)))
  
  if(n < limit)
    return(paste0(paste(rep(" ", limit - n), collapse = ""), x)) else x
}

#' Obtain a data.frame of the sub-directories in a box.com folder
#' 
#' Takes the \code{id} of a box folder and returns a data.frame of it's 
#' subdirectories, indluding thier equivalent paths in the local directory.
#' 
#' @param dir_id The box.com id for the folder that you'd like to query
#' @param local_dir The local directory which you'd like \code{dir_id} to 
#' correspond to. If you're not interested in mapping local to hosted 
#' directories, this isn't terribly important.
#' @return A data.frame describing the contents directory structure of the 
#' box.com folder corresponding to \code{dir_id}.
#' @keywords internal
dirTreeRecursive <- function(dir_id, local_dir = getwd()){
  
  dir_row <- 
    data.frame(
      type = "folder", id = dir_id, sequence_id = 0,
      etag = 0, name = "",
      local_dir = local_dir,
      path = "~"
    )
  
  dirDirs <- function(dir_id){
    df <- box_ls(dir_id)
    return(df[df$type == "folder",])
  }
  
  dirTreeList <- function(dir_row){
    sub_dirs <- dirDirs(dir_row[,"id"])
    
    if(!is.null(sub_dirs))
      if(nrow(sub_dirs) > 0){
        sub_dirs$local_dir <- paste0(dir_row[,"local_dir"], "/", sub_dirs$name)
        sub_dirs$path      <- paste0(dir_row[,"path"], "/", sub_dirs$name)
      }
    
    tmp <- list(sub_dirs)
    
    if(!is.null(sub_dirs))
      if(nrow(sub_dirs) > 0)
        for(i in 1:nrow(sub_dirs))
          tmp <- c(tmp, Recall(sub_dirs[i,]))
    tmp
  }
  
  d <- dirTreeList(dir_row)
  
  plyr::rbind.fill(d)
}

# A version of cat which only works if the package options are set to verbose,
# and pads out the message with spaces so that it fills/wipes the console
catif <- function(..., do_cat = getOption("boxr.verbose")){
  if(do_cat){
    txt <- paste(...)
    cat(txt, rep(" ", getOption("width") - nchar(txt) - 1))
  }
}

checkAuth <- function(){
  if(is.null(getOption("boxr.token")))
    stop("It doesn't look like you've set up authentication for boxr yet.\n",
         "See ?box_auth")
}


# A function to exit, returning an object of class
# boxr_dir_wide_operation_result
returnDwOp <- function(op_detail){
  
  # As this is supposed to be a list of lists (not just lists), put a list in a
  # list, if it's just a list.
  if(! "list" %in% class(op_detail[[1]][[1]])){
    op_detail <- list(op_detail)
  }
  
  # My God this is ugly. If a list item doesn't exist, null will be returned
  successful_downloads <- 
    dplyr::rbind_all(lapply(
      op_detail$files, function(x) data.frame(x$successful_downloads)
    ))
  
  unsuccessful_downloads <-
    dplyr::rbind_all(lapply(
      op_detail$files, function(x) data.frame(x$unsuccessful_downloads)
    ))
  
  up_to_date <-
    dplyr::rbind_all(lapply(
      op_detail$files, function(x) data.frame(x$up_to_date)
    ))
  
  successful_updates <-
    dplyr::rbind_all(lapply(
      op_detail$files, function(x) data.frame(x$successful_updates)
    ))
  
  unsuccessful_updates <-
    dplyr::rbind_all(lapply(
      op_detail$files, function(x) data.frame(x$unsuccessful_updates)
    ))
  
  successful_uploads <-
    dplyr::rbind_all(lapply(
      op_detail$files, function(x) data.frame(x$successful_uploads)
    ))
  
  unsuccessful_uploads <-
    dplyr::rbind_all(lapply(
      op_detail$files, function(x) data.frame(x$unsuccessful_uploads)
    ))
  
  file_list <- 
    list(
      successful_downloads   = successful_downloads,
      unsuccessful_downloads = unsuccessful_downloads,
      successful_updates     = successful_updates,
      unsuccessful_updates   = unsuccessful_updates,
      successful_uploads     = successful_uploads,
      unsuccessful_uploads   = unsuccessful_uploads,
      up_to_date             = up_to_date
    )
  
  msg_list <- 
    list(
      "files downloaded from box.com",
      "files were NOT downloaded from box.com",
      "files updated on box.com",
      "files were NOT updated on box.com",
      "new files uploaded to box.com",
      "new files were NOT uploaded to box.com",
      "files were already up-to-date on box.com (nothing done)"
    )
  
  # Lose all the dplyr stuff
  file_list <- lapply(file_list, function(x) data.frame(x))
  
  out <- 
    structure(
      list(
        operation    = op_detail$operation,
        start        = op_detail$t1,
        end          = Sys.time(),
        local_tld    = op_detail$local_tld,
        # box_tld_name = op_detail$box_tld_name,
        box_tld_id   = op_detail$box_tld_id,
        file_list    = file_list,
        msg_list     = msg_list
      ),
      class = "boxr_dir_wide_operation_result"
    )
  
  return(out)
}
