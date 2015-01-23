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
#' @param ignore_dots \code{logical}. Should local directories with filenames
#' begining with dots be ignored? This is useful for 'invisible' folders such as
#' \code{.git} and \code{.Rproj.user} where uploading them is likely to be
#' unexpected.
#' 
#' @details The box.com API does not have direct support for downloading more 
#' than one file. With \code{recursive} set to \code{false}, \code{box_pull} 
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
box_fetch <- 
  function(
    dir_id, local_dir = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  ){
    
    checkAuth()
    
    if(!recursive){
      return(downloadDirFiles(dir_id, local_dir))
    }
    # 1. Recursively scan the box dir for folders
    d <- dirTreeRecursive(dir_id, local_dir)
    
    # 2. Update the files in the tld
    downloadDirFiles(dir_id, local_dir = local_dir, overwrite = overwrite)
    
    # Loop through the box dirs. If they don't exist, create them.
    # Once they do, fill 'em up!
    for(i in 1:nrow(d)){
      dir.create(d$local_dir[i], showWarnings = FALSE)
      downloadDirFiles(
        d$id[i], d$local_dir[i], overwrite = overwrite,
        dir_str = 
          paste0("(", i, "/", nrow(d), "): ", trimDir(d$local_dir[i], 5))
        )
    }
  }


#' @rdname box_fetch
#' @export
box_push <- 
  function(dir_id, local_dir = getwd(), ignore_dots = TRUE){
    
    checkAuth()
    
    # First update the files in the first level of the directory
    boxr:::uploadDirFiles(
      dir_id, 
      normalizePath(paste0(local_dir))
    )

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
    
    # If there is no 'depth', end
    if(length(dir_depth) < 1)
      return(TRUE)
    
    # Order the dirs by depth
    local_dirs <- local_dirs[order(dir_depth)]
    
    # A version where the dir names will be replaced with thier box.com ids.
    # Started off by appending the current folder id
    box_dirs <- paste0(dir_id, "/", local_dirs)
    
    for(i in 1:length(box_dirs)){
      catif(
        paste0(
          "\rComparing local dir ", i,"/",length(local_dirs),": ", local_dirs[i],
          "\r"
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
          "\rCreated box.com folder (id: ", new_dir_id, ") ", local_dirs[i], 
          "\r"
        )
      }
      # If the folder already exists, take it's id
      if(new_dir$status == 409)
        new_dir_id <- httr::content(new_dir)$context_info$conflicts[[1]]$id
      
      # Create a string, where the name of the local dir is replaced by it's
      # box.com folder id
      rep_str <- gsub(basename(box_dirs[i]), new_dir_id, box_dirs[i])
      
      # Where you see the old path, replace it
      box_dirs <- gsub(box_dirs[i], rep_str, box_dirs)
      
      # Upload the files in the directory
      boxr:::uploadDirFiles(
        new_dir_id, 
        normalizePath(paste0(local_dir, "/", local_dirs[i]))
      )
    }
  }

#' @rdname box_fetch
#' @export
box_merge <- function(dir_id, local_dir = getwd(), ignore_dots = TRUE){
  
  checkAuth()
  
  box_push(local_dir, dir_id, ignore_dots = ignore_dots)
  box_fetch(local_dir, dir_id)
}
