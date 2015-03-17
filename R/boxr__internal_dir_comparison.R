

#' Create a data.frame of metadata of the contents of a local directory
#' 
#' @inheritParams dirTreeRecursive
#' @return A data.frame of metadata.
#' @keywords internal
create_loc_dir_df <- function(local_dir = getwd()){
  fs <- list.files(local_dir, full.names = TRUE)
  if(length(fs) < 1L)
    return(data.frame())
  
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
  
  dplyr::bind_rows(d)
}


# You should create a seperate dir diff function, shared between
# uploadDirFiles and downloadDirFiles
box_dir_diff <- function(dir_id, local_dir, load = "up", folders = FALSE){
  
  if(!load %in% c("up", "down"))
    stop('load must be either "up" or "down"')
  
  loc_dir_df <- create_loc_dir_df(local_dir)
  box_dir_df <- box_ls(dir_id)
  
  b <- box_dir_df[box_dir_df$type == "file",]
  l <- loc_dir_df[loc_dir_df$type == "file",]
  
  if(folders){
    b_folders <- box_dir_df[box_dir_df$type == "folder",]
    l_folders <- loc_dir_df[loc_dir_df$type == "folder",]
    
    l_folders$name <- gsub(".*\\/", "", l_folders$name)
    
  } else {
    b_folders <- data.frame()
    l_folders <- data.frame()
  }
  
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
    origin_folders <- l_folders
    destin_folders <- b_folders
  }
  
  if(load == "down"){
    origin <- b
    destin <- l
    origin_folders <- b_folders
    destin_folders <- l_folders
  }
  
  absent      <- origin[!origin$name %in% destin$name,]
  present     <- origin[ origin$name %in% destin$name,]
  superfluous <- destin[!destin$name %in% origin$name,]
  
  superfluous_folders <- 
    destin_folders[!destin_folders$name %in% origin_folders$name,]
  
  # Same content?
  if(length(present) > 0 && nrow(present) > 0L){
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
  if(length(changed) > 0 && nrow(changed) > 0L){
    changed <- merge(changed, destin, by = "name", all.x = TRUE, all.y = FALSE)
    to_update <- changed[changed$mod.x > changed$mod.y,]
    behind    <- changed[changed$mod.x < changed$mod.y,]
  } else {
    to_update <- behind <- data.frame()
  }
  
  if(class(absent) != "data.frame")
    absent <- data.frame()
  
  list(
    new                 = data.frame(absent),
    superfluous         = data.frame(superfluous),
    to_update           = data.frame(to_update),
    up_to_date          = data.frame(nchange),
    behind              = data.frame(behind),
    superfluous_folders = data.frame(superfluous_folders)
  )
}
