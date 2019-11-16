#' Download/upload directories from/to Box
#' 
#' @description
#' 
#' \describe{
#'   \item{`box_fetch()`}{download the contents of a Box folder to a local 
#'     directory}
#'   \item{`box_push()`}{upload the contents of a local directory to a Box
#'     folder}
#' }
#' 
#' Files present in the origin but not the destination will be
#' copied over. 
#' 
#' Behaviour when a file exists in both depends on the arguments supplied.
#' 
#' @aliases box_push box_fetch
#' 
#' @inheritParams box_setwd
#' @param local_dir `character`, path to local directory.
#' @param recursive `logical`, indicates to include subdirectories.
#' @param overwrite `logical`, indicates that newer files at origin will
#'   overwrite older files at destination.
#' @param delete `logical`, indicates to delete files that exist at destination,
#'   but not at origin.
#' @param ignore_dots `logical`, indicates to ignore directories with names 
#'   that begin with dots, e.g. `.git` and `.Rproj.user`.
#' 
#' @section Overwrite/Update:
#' 
#' In the interests of preventing mishaps, `overwrite` is by default set
#' to `FALSE`, which means that files which exist in the destination,
#' but which are out of date, are not modified.
#' 
#' Setting `overwrite` to `TRUE` is likely to produce expected
#' behavior for most users.
#' 
#' This is a conservative precaution to prevent users unexpectedly overwriting
#' their files, and may change as a default in later releases. 
#' 
#' However, files at Box are versioned, and most
#' operating systems have file recovery features (e.g. 'Trash'
#' (Ubuntu/Debian/OSX), or 'Recycle Bin' (Windows)), so unintended 
#' modification of files will be revertable for most users.
#' 
#' @section Implementation:
#' 
#' At the time of writing, the Box API allows for only one file at a time
#' to be uploaded/downloaded. As a result, boxr recursively scans the
#' directory tree, uploading/downloading files in loops. Because the Box
#' API can send, but not accept, gzipped files, downloading tends to be faster
#' than uploading.
#' 
#' `box_fetch()`/`box_push()` rely on the internal function 
#' [box_dir_diff()] to determine how to process individual files
#'   (i.e. which to update, which to leave as is, etc.). See its help page for
#'   details.
#' 
#' @return Object with S3 class [`boxr_dir_wide_operation_result`][boxr_S3_classes].
#' 
#' @seealso [box_dl()]/[box_ul()] for single file 
#'   operations, [box_dir_diff()] 
#'   determines how files should be processed
#' 
#' @export
box_fetch <- function(dir_id = box_getwd(), local_dir = getwd(), 
                      recursive = TRUE, overwrite = FALSE, delete = FALSE) {
  checkAuth()
  
  t1 <- Sys.time()
  
  # Initialize a variable to log downloads
  fetch_log <- c()
  dir_c     <- c()
  
  # Define a function which outputs the object so far, on exit
  fetchExit <- function() {
    returnDwOp(list(
      files      = 
        c(fetch_log, 
          list(list(local_new_dirs = data.frame(full_path = dir_c)))), 
      operation  = "box_fetch",
      local_tld  = local_dir,
      box_tld_id = dir_id,
      t1         = t1
    ))
  }
  
  # Recursively scan the box dir for folders
  d <- dirTreeRecursive(dir_id, local_dir)
  
  # Update the tld
  dl <- downloadDirFiles(dir_id, local_dir = local_dir, overwrite = overwrite)
  
  dl$relative_path <- paste0(local_dir, "/", dl$name)
  
  fetch_log <- c(list(dl), fetch_log)
  
  if (delete) {
    deletions <- deleteLocalObjects(dir_id, local_dir)
    fetch_log <- c(list(deletions), fetch_log)
  }
  
  # If there are no subdirectories (or user's not interested in them), update 
  # and exit
  if (nrow(d) < 1 | !recursive)
    return(fetchExit())
  
  # Loop through the box dirs. If they don't exist, create them.
  # Once they do, fill 'em up!
  for (i in 1:nrow(d)) {
    
    catif(paste0(
        "Comparing remote dir ", i,"/", nrow(d) ,": ", d$path[i]
    ))

    dc <- dir.create(
      normalizePath(d$local_dir[i], mustWork = FALSE), 
      showWarnings = FALSE
    )
    
    # If a local dir was created, log it
    if (dc)
      dir_c <- c(dir_c, d$local_dir[i])
    
    dl <- downloadDirFiles(
      d$id[i], d$local_dir[i], overwrite = overwrite,
      dir_str = 
        paste0("(", i, "/", nrow(d), "): ", trimDir(d$local_dir[i], 5))
    )
    
    # Add a variable which has the local path on there
    # Note: The !is.na(x$sha1) part is due to downloadDirFiles occasionally
    # returning 1x0 data.frames. Not quite sure why that's happening at the 
    # moment! sha1 is used just because it comes from things which are 
    # verifiably files.
    dl <- 
      lapply(
        dl,
        function(x) {
          if (
            !is.null(x) && nrow(x) > 0 && !is.null(x$sha1) && # Sorry you have
              sum(!is.na(x$sha1)) > 0                         # to see this.
          )
            x$relative_path <- paste0(d$local_dir[i], "/", x$name)
          x
        }
      )
    
    fetch_log <- c(list(dl), fetch_log)
    
    # Delete remote files and folders
    if (delete) {
      deletions <- deleteLocalObjects(d$id[i], d$local_dir[i])
      fetch_log <- c(list(deletions), fetch_log)
    }
  }
  
  return(fetchExit())
}


#' @rdname box_fetch
#' @export
box_push <- function(dir_id = box_getwd(), local_dir = getwd(), 
                     ignore_dots = TRUE, overwrite = FALSE, delete = FALSE) {
  
  checkAuth()
  
  t1 <- Sys.time()
  
  # Define a function which outputs the object so far, on exit
  pushExit <- function() {
    returnDwOp(list(
      files      = 
        c(push_log, 
          list(list(remote_new_dirs = data.frame(full_path = dir_c)))), 
      operation  = "box_push",
      local_tld  = local_dir,
      box_tld_id = dir_id,
      t1         = t1
    ))
  }
  
  # Initializing a running total of file operations for the course of the 
  # functions
  push_log <- c()
  dir_c    <- c()
  
  # First update the files in the first level of the directory
  ul <- uploadDirFiles(dir_id, local_dir, 
                       overwrite = overwrite)
  
  # Append new file operations
  push_log <- c(list(ul), push_log)
  
  if (delete) {
    deletions <- deleteRemoteObjects(dir_id, local_dir)
    push_log <- c(list(deletions), push_log)
  }

  local_dirs <- list.dirs(normalizePath(local_dir), full.names = FALSE)[-1]
  
  if (ignore_dots)
    local_dirs <- local_dirs[!grepl("\\/\\.|^\\.", local_dirs)]
  
  dir_depth <- unlist(
    lapply(
      gregexpr("\\/", local_dirs),
      function(x) sum(attr(x, "match.length") > 0)
    )
  )
  
  # Remove the confusing dot-slash thing (if it's there)
  local_dirs <- gsub("^\\.\\/", "", local_dirs)
  
  # If tree-depth is 0, end
  if (length(dir_depth) < 1)
    return(pushExit())
  
  # Order the dirs by depth
  local_dirs <- local_dirs[order(dir_depth)]
  
  # A version where the dir names will be replaced with thier box.com ids.
  # Started off by appending the current folder id
  box_dirs <- paste0(dir_id, "/", local_dirs)
  
  for (i in 1:length(box_dirs)) {
    catif(paste0(
      "Comparing local dir ", i, "/", length(local_dirs), ": ", local_dirs[i]
    ))
    
    new_dir <- boxDirCreate(
      dir_name = basename(box_dirs[i]),
      parent_dir_id = 
        gsub(
          ".*/", "", 
          gsub(
            paste0("/", basename(box_dirs)[i]), "", box_dirs[i]
          )
        )
    )
    
    # If the folder is brand new, take it's id
    if (new_dir$status == 201) {
      new_dir_id <- httr::content(new_dir)$id
      catif(
        paste0("Created box.com folder (id: ", new_dir_id, ") ", local_dirs[i])
      )
      
      dir_c <- c(
        dir_c, 
        paste0(
          local_dirs[i], " (id: ",
          httr::content(new_dir)$id, ")"
        )
      )
    }
    
    # If the folder already exists, take it's id
    if (new_dir$status == 409)
      new_dir_id <- httr::content(new_dir)$context_info$conflicts[[1]]$id
    
    # String where the name of the local dir is replaced by it's
    # box.com folder id
    rep_str <- gsub(basename(box_dirs[i]), new_dir_id, box_dirs[i])
    
    # Where you see the old path, replace it
    box_dirs <- gsub(box_dirs[i], rep_str, box_dirs)
    
    # Upload the files in the directory
    ul <- uploadDirFiles(
      new_dir_id, 
      paste0(local_dir, "/", local_dirs[i]),
      overwrite = overwrite
    )
    
    # Add the uploads to the running total
    push_log <- c(list(ul), push_log)
    
    # Delete remote files and folders
    if (delete) {
      deletions <- deleteRemoteObjects(
        new_dir_id, 
        paste0(local_dir, "/", local_dirs[i])
      )
      
      push_log <- c(list(deletions), push_log)
    }
    
  }
  
  # Return using internal function to extract elements from the list,
  # and smack a class on it
  return(pushExit())
}

# #' @rdname box_fetch
# #' @export
# box_merge <- function(dir_id = box_getwd(), local_dir = getwd(), 
#                       ignore_dots = TRUE, recursive = TRUE, overwrite = FALSE,
#                       delete = FALSE) {
#   
#   checkAuth()
#   %>% 
#   bp <- box_push(dir_id, local_dir, ignore_dots = ignore_dots)
#   bf <- box_fetch(dir_id, local_dir)
#   
#   # You need to figure out a way to combine the outputs here
#   bm <- bp
#   
#   bind <- function(x, y) data.frame(dplyr::bind_rows(x, y))
#   # bind <- rbind
#   
#   # Combine the output objects
#   bm$file_list <- mapply(bind, bf$file_list, bp$file_list)
#   
#   # De-dupe the output objects
#   bm$file_list[grepl("up-to-date", bm$msg_list)]
#   
#   bm$operation <- "box_merge"
#   bm
#   
#   # Notes: 
#   # * For some reason the column name is being printed with summary()
#   #     (you don't want that).
#   #     
#   # * Also, you're getting dupes under already up to date (obviously)
#   # 
#   
# }

