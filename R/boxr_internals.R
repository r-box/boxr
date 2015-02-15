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
  
#   loc_dir_df <- create_loc_dir_df(local_dir)
#   box_dir_df <- box_ls(dir_id)
#   
#   # Lists of the local and box files
#   bf <- box_dir_df$name[box_dir_df$type == "file"]
#   if(length(bf) < 1L)
#     return(NULL)
#   
#   lf <- gsub(".*/", "", loc_dir_df$name[loc_dir_df$type == "file"])
#   
#   absent  <- base::setdiff(bf, lf)
#   present <- base::intersect(bf, lf)
#   
#   # Do any of the existing files need updating?
#   to_update <- vector()
#   
#   if(length(lf) > 0L){
#     b_sha1 <- setNames(box_dir_df$sha1[box_dir_df$type == "file"], bf)
#     l_sha1 <- setNames(loc_dir_df$sha1[loc_dir_df$type == "file"], lf)
#     
#     to_update  <- present[!l_sha1[present] == b_sha1[present]]
#     up_to_date <- present[ l_sha1[present] == b_sha1[present]]
#   }
#   
#   # If overwrite is false, ignore to_update
#   if(!overwrite)
#     to_update <- NULL
#   
#   # The ids of files to download
  
  box_dd <- box_dir_diff(dir_id, local_dir, load = "down")
  
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
  
#   loc_dir_df <- create_loc_dir_df(local_dir)
#   box_dir_df <- box_ls(dir_id)
#   
#   # Lists of the local and box files
#   bf <- box_dir_df$name[box_dir_df$type == "file"]
#   
#   lf <- gsub(".*/", "", loc_dir_df$name[loc_dir_df$type == "file"])
#   if(length(lf) < 1L)
#     return(NULL)
#   
#   absent  <- base::setdiff(lf, bf)
#   present <- base::intersect(bf, lf)
#   
#   # Do any of the existing files need updating?
#   to_update <- vector()
#   
#   if(length(bf) > 0L){
#     b_sha1 <- setNames(box_dir_df$sha1[box_dir_df$type == "file"], bf)
#     l_sha1 <- setNames(loc_dir_df$sha1[loc_dir_df$type == "file"], lf)
#     
#     to_update  <- present[!l_sha1[present] == b_sha1[present]]
#     up_to_date <- present[ l_sha1[present] == b_sha1[present]]
#   }
#   
  
  box_dd <- box_dir_diff(dir_id, local_dir, load = "up")
  
  # Run through the files to update, and upload up dates
  # NOTE: insert messages/progress bars here
  updates <- list()
  uploads <- list()
  
  if(overwrite & nrow(box_dd$to_update) > 0)
    for(i in 1:nrow(box_dd$to_update)){
      catif(
        paste0(
          "Updating file (", i,"/",nrow(box_dd$to_update),"): ", 
          box_dd$to_update$name[i]
        )
      )
      updates[[i]] <- 
        box_update_file(
          file.path(local_dir, box_dd$to_update$name[i]),
          box_dd$to_update$id[i],
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
        box_upload_new(file.path(local_dir, box_dd$new$name[i]), dir_id)
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
  
  # My GOD this is ugly. If a list item doesn't exist, null will be returned
  suppressWarnings({
    successful_downloads <- 
      dplyr::bind_rows(lapply(
        op_detail$files, function(x) data.frame(x$successful_downloads)
      ))
    
    unsuccessful_downloads <-
      dplyr::bind_rows(lapply(
        op_detail$files, function(x) data.frame(x$unsuccessful_downloads)
      ))
    
    up_to_date <-
      dplyr::bind_rows(lapply(
        op_detail$files, function(x) data.frame(x$up_to_date)
      ))
    
    successful_updates <-
      dplyr::bind_rows(lapply(
        op_detail$files, function(x) data.frame(x$successful_updates)
      ))
    
    unsuccessful_updates <-
      dplyr::bind_rows(lapply(
        op_detail$files, function(x) data.frame(x$unsuccessful_updates)
      ))
    
    successful_uploads <-
      dplyr::bind_rows(lapply(
        op_detail$files, function(x) data.frame(x$successful_uploads)
      ))
    
    unsuccessful_uploads <-
      dplyr::bind_rows(lapply(
        op_detail$files, function(x) data.frame(x$unsuccessful_uploads)
      ))
    
    remote_new_dirs <-
      dplyr::bind_rows(lapply(
        op_detail$files, function(x) data.frame(x$box_dc)
      ))
    
    local_new_dirs <-
      dplyr::bind_rows(lapply(
        op_detail$files, function(x) data.frame(x$local_dc)
      ))
  })
  
  file_list <- 
    list(
      successful_downloads   = successful_downloads,
      unsuccessful_downloads = unsuccessful_downloads,
      successful_updates     = successful_updates,
      unsuccessful_updates   = unsuccessful_updates,
      successful_uploads     = successful_uploads,
      unsuccessful_uploads   = unsuccessful_uploads,
      up_to_date             = up_to_date,
      local_new_dirs         = local_new_dirs,
      remote_new_dirs        = remote_new_dirs
    )
  
  msg_list <- 
    list(
      "files downloaded from box.com",
      "files were NOT downloaded from box.com",
      "files updated on box.com",
      "files were NOT updated on box.com",
      "new files uploaded to box.com",
      "new files were NOT uploaded to box.com",
      "files were already up-to-date on box.com (nothing done)",
      "new local directories created",
      "new remote directories created"
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

# A version of cat which only works if the package options are set to verbose,
# and pads out the message with spaces so that it fills/wipes the console
catif <- function(...){
  if(getOption("boxr.verbose")){
    txt <- paste(...)
    cat(paste0("\r", txt, rep(" ", getOption("width") - nchar(txt) - 1)))
  }
}

# A function to convert the datetime strings that the box api uses, to something
# R can understand
box_datetime <- function(x){
  # R has trouble figuring out the time format
  # Split out the date/time part
  dt <- substr(x, 1, nchar(x) - 6)
  # and the timezone offset
  tz <- substr(x, nchar(x) - 5, nchar(x))
  
  tz <- gsub(":", "", tz)
  
  # Note, the timzeone of the datetime boject will be the system default,
  # bit it's value will have been adjusted to account for the timzone of x
  as.POSIXct(paste0(dt, tz), format = "%Y-%m-%dT%H:%M:%S%z")
}

# You should create a seperate dir diff function, shared between
# uploadDirFiles and downloadDirFiles
box_dir_diff <- function(dir_id, local_dir, load = "up"){
  
  if(!load %in% c("up", "down"))
    stop('load must be either "up" or "down"')
  
  loc_dir_df <- create_loc_dir_df(local_dir)
  box_dir_df <- box_ls(dir_id)
  
  b <- box_dir_df[box_dir_df$type == "file",]
  l <- loc_dir_df[loc_dir_df$type == "file",]
  
  # Remove the filepath from the local name
  l$name <- gsub(".*\\/", "", l$name)
  
  # Set the dates to use as a criteria
  # For box.com, use the modified_at date. Note, this isn't the date that the
  # *content* was modified. However, you'd want to use this date in the context
  # of someone's replacing a new, bad file, with an old good one
  #
  # Note: reverting to an old version, creates a new version (with a new 
  # modified_at).
  b$mod <- b$modified_at
  # For the local directory, set this to the *content* modified time. This is 
  # because, unlike box, for the same file in the same place to have changed
  # it's content must have, too.
  l$mod <- l$mtime
  
  if(load == "up"){
    origin <- l
    destin <- b
  }
  
  if(load == "down"){
    origin <- b
    destin <- l
  }
  
  # If there's nothing that could be moved, end it.
  if(length(origin) < 1L)
    return(NULL)
  
  absent  <- origin[!origin$name %in% destin$name,]
  present <- origin[ origin$name %in% destin$name,]
  
  # Same content?
  if(nrow(present) > 0L){
    o_sha1 <- setNames(origin$sha1, origin$name)
    d_sha1 <- setNames(destin$sha1, destin$name)
    
    # Files in the origin which have changed, or not
    changed <- present[!d_sha1[present$name] == o_sha1[present$name],]
    nchange <- present[ d_sha1[present$name] == o_sha1[present$name],]
  } else {
    changed <- nchange <- data.frame()
  }
  
  # to_update: changed files, where the mod date at the origin is later than
  # the destination
  if(nrow(changed) > 0L){
    changed <- merge(changed, destin, by = "name", all.x = TRUE, all.y = FALSE)
    to_update <- changed[changed$mod.x > changed$mod.y,]
    behind    <- changed[changed$mod.x < changed$mod.y,]
  } else {
    to_update <- behind <- data.frame()
  }
  
  list(
    new = absent,
    to_update = to_update,
    up_to_date = nchange,
    behind = behind    
  )
}
