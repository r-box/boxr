#' Recursive, Directory-wide Operations to Synchronize Local and box.com
#' directories
#' 
#' These functions take a path to a local directory, and a box.com folder id,
#' and perform the update/sychronization operations.
#' 
#' \code{box_fetch} Will create local versions of files and directories which
#' are present on box.com, but not locally. If \code{overwrite} is true, files
#' which are present both locally and on box.com will be overwritten with the 
#' box.com versions.
#' 
#' \code{box_push} Will create box.com versions of files and directories which
#' are present locally, but not on box.com. Files which already appear to exist
#' will be uploaded as new versions.
#' 
#' \code{box_fetch} A convenience function, effectively 
#' \code{box_fetch();box_push()}
#' 
#' @aliases box_push box_fetch
#' 
#' @param recursive \code{logical}. Should the call include subdirectories and 
#' thier contents?
#' @param delete \code{logical}. Should files which exist in the destination,
#' but not the origin, be deleted?
#' @param ignore_dots \code{logical}. Should local directories with filenames
#' begining with dots be ignored? This is useful for 'invisible' folders such as
#' \code{.git} and \code{.Rproj.user} where uploading them is likely to be
#' unexpected.
#' 
#' @details The box.com API does not have direct support for downloading more 
#' than one file. With \code{recursive} set to \code{false}, \code{box_fetch} 
#' will download the files, but not subdirectories of the folder specified by 
#' \code{dir_id}. If \code{recursive == TRUE}, then it will download every file 
#' and folder in the directory tree. Because R has to make recursive API calls 
#' to explore the directory structure, and then iterate through each file it 
#' finds, this option can be rather slow.
#' 
#' @inheritParams dirTreeRecursive 
#' @inheritParams box_dl
#' 
#' @export
#' 
#' @return Nothing. Used for its side-effects.
box_fetch <- function(dir_id, local_dir = getwd(), recursive = TRUE, 
                      overwrite = FALSE, delete = FALSE){
  checkAuth()
  
  t1 <- Sys.time()
  
  # Initialize a variable to log downloads
  fetch_log <- c()
  dir_c  <- c()
  
  # Define a function which outputs the object so far, on exit
  fetchExit <- function(){
    returnDwOp(
      list(
        files      = fetch_log, 
        operation  = "box_fetch",
        local_tld  = local_dir,
        box_tld_id = dir_id,
        t1         = t1,
        local_dc   = dir_c
      )
    )
  }
  
  # Recursively scan the box dir for folders
  d <- dirTreeRecursive(dir_id, local_dir)
  
  # Update the tld
  dl <- downloadDirFiles(dir_id, local_dir = local_dir, overwrite = overwrite)
  fetch_log <- c(list(dl), fetch_log)
  
  if(delete){
    deletions <- deleteLocalObjects(dir_id, local_dir)
    fetch_log <- c(list(deletions), fetch_log)
  }
  
  # If there are no subdirectories (or user's not interested in them), update 
  # and exit
  if(nrow(d) < 1 | !recursive)
    return(fetchExit())
  
  # Loop through the box dirs. If they don't exist, create them.
  # Once they do, fill 'em up!
  for(i in 1:nrow(d)){
    
    catif(
      paste0(
        "Comparing remote dir ", i,"/", nrow(d) ,": ", d$path[i]
      )
    )
    
    dc <- dir.create(
      normalizePath(d$local_dir[i], mustWork = FALSE), 
      showWarnings = FALSE
    )
    
    # If a local dir was created, log it
    if(dc)
      dir_c <- c(dir_c, d$local_dir[i])
    
    dl <-
      downloadDirFiles(
        d$id[i], d$local_dir[i], overwrite = overwrite,
        dir_str = 
          paste0("(", i, "/", nrow(d), "): ", trimDir(d$local_dir[i], 5))
      )
    
    fetch_log <- c(list(dl), fetch_log)
    
    # Delete remote files and folders
    if(delete){
      deletions <- deleteLocalObjects(d$id[i], d$local_dir[i])
      fetch_log <- c(list(deletions), fetch_log)
    }
  }
  
  return(fetchExit())
}


#' @rdname box_fetch
#' @export
box_push <- function(dir_id, local_dir = getwd(), ignore_dots = TRUE,
                     overwrite = FALSE, delete = FALSE){
  
  checkAuth()
  
  t1 <- Sys.time()
  
  # Define a function which outputs the object so far, on exit
  pushExit <- function(){
    returnDwOp(
      list(
        files = push_log, 
        operation  = "box_push",
        local_tld  = local_dir,
        box_tld_id = dir_id,
        t1         = t1,
        box_dc     = dir_c
      )
    )
  }
  
  # Initializing a running total of file operations for the course of the 
  # functions
  push_log <- c()
  dir_c  <- c()
  
  # First update the files in the first level of the directory
  ul <- uploadDirFiles(dir_id, local_dir, 
                       overwrite = overwrite)
  
  # Append new file operations
  push_log <- c(list(ul), push_log)
  
  if(delete){
    deletions <- deleteRemoteObjects(dir_id, local_dir)
    push_log <- c(list(deletions), push_log)
  }

  local_dirs <- list.dirs(normalizePath(local_dir), full.names = FALSE)[-1]
  
  if(ignore_dots)
    local_dirs <- local_dirs[!grepl("\\/\\.", local_dirs)]
  
  dir_depth <- 
    unlist(
      lapply(
        gregexpr("\\/", local_dirs),
        function(x) length(attr(x, "match.length"))
      )
    )
  
  # Remove the confusing dot-slash thing (if it's there)
  local_dirs <- gsub("^\\.\\/", "", local_dirs)
  
  # If tree-depth is 0, end
  if(length(dir_depth) < 1)
    return(pushExit())
  
  # Order the dirs by depth
  local_dirs <- local_dirs[order(dir_depth)]
  
  # A version where the dir names will be replaced with thier box.com ids.
  # Started off by appending the current folder id
  box_dirs <- paste0(dir_id, "/", local_dirs)
  
  for(i in 1:length(box_dirs)){
    catif(
      paste0(
        "Comparing local dir ", i,"/",length(local_dirs),": ", local_dirs[i]
      )
    )
    
    new_dir <-
      box_dir_create(
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
    if(new_dir$status == 201){
      new_dir_id <- httr::content(new_dir)$id
      catif(
        "Created box.com folder (id: ", new_dir_id, ") ", local_dirs[i]
      )
      
      dir_c <- c(
        dir_c, 
        paste0(
          httr::content(new_dir)$name, " (id: ",
          httr::content(new_dir)$id, ")"
        )
      )
    }
    
    # If the folder already exists, take it's id
    if(new_dir$status == 409)
      new_dir_id <- httr::content(new_dir)$context_info$conflicts[[1]]$id
    
    # String where the name of the local dir is replaced by it's
    # box.com folder id
    rep_str <- gsub(basename(box_dirs[i]), new_dir_id, box_dirs[i])
    
    # Where you see the old path, replace it
    box_dirs <- gsub(box_dirs[i], rep_str, box_dirs)
    
    # Upload the files in the directory
    ul <- 
      uploadDirFiles(
        new_dir_id, 
        paste0(local_dir, "/", local_dirs[i]),
        overwrite = overwrite
      )
    
    # Add the uploads to the running total
    push_log <- c(list(ul), push_log)
    
    # Delete remote files and folders
    if(delete){
      deletions <- 
        deleteRemoteObjects(
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

#' @rdname box_fetch
#' @export
box_merge <- function(dir_id, local_dir = getwd(), ignore_dots = TRUE){
  
  checkAuth()
  
  bp <- box_push(dir_id, local_dir, ignore_dots = ignore_dots)
  bf <- box_fetch(dir_id, local_dir)
  
  # You need to figure out a way to combine the outputs here
  bm <- bp
  
  bm$file_list[grepl("downloads", names(bm$file_list))] <-
    bf$file_list[grepl("downloads", names(bf$file_list))]
  
  bm$operation <- "box_merge"
  bm
}

