#' Compare the contents of Remote and Local Directories
#' 
#' `box_dir_diff` is the internal function used by 
#' [box_fetch()] and [box_push()] to determine how to
#' which files and folders should be uploaded/downloaded, updated,
#' or deleted, to synchronize remote and local directories.
#' 
#'     
#' `box_dir_diff` works by comparing files in the **'origin'** to 
#' those in the **'destination'**.
#'   
#' For downloading files (e.g. with [box_fetch()]), the origin is 
#' the remote folder on box.com specified with `dir_id`, and the 
#' destination would be the local directory specified by `local_dir`. 
#'   
#' The reverse is true for uploads (e.g. via [box_fetch()]).
#'   
#' `box_dir_diff` decides what should happen to a file based on three
#'   
#' \describe{
#'  \item{**Presence**}{
#'  Is the file present in both the origin and destination? The filename
#'  (within the directory structure) is used to determine this.
#'  }
#'  \item{**Content**}{
#'  If a file is present in both the origin and the destination, does it
#'  have the same content? The definition comes from the file's `sha1`
#'  hash, which for local files is determined using the
#'  [digest()] function from the package of the same name. For
#'  remote files, it is queried from the box.com API.
#'  }
#'  \item{**Modification Date**}{
#'  If a file is present in both the origin and destination, and the
#'  content is different in each, boxr will prefer the file which was most
#'  recently modified.
#'  
#'  For local files, the 'content modified time' is used; the `mtime`
#'  variable returned by [file.info()].
#'  
#'  For remote files, the `modified_at` date returned by the box.com API.
#'  This is the time that the file was modified on the box.com servers, as
#'  opposed to the time that the content itself was modified.
#'  }
#' }
#'   
#' **Why not use the content modified time for both?** 
#'      
#' With regards to the box.com API, `modified_at` is preferred to 
#' `content_modified_at`, as it includes changes to the file outside of
#' just it's content. This means that, for example, a collaborator could
#' roll back to a previous version of a file, or upload a preferred but 
#' older version. These actions count as modifications on the box.com 
#' servers, but not to the content of the file itself (they are reflected
#' in `modified_at`, but not `content_modified_at`).
#'     
#' Implementing similar functionality for local files is not possible in a 
#' platform-independent manner; content modified time is the only file-based
#' timestamp which has a consistent definition for UNIX and Windows
#' systems.
#' 
#' @param dir_id The id of the box.com folder which you'd like to use for the
#'   comparison
#' @param local_dir The path of the local folder which you'd like to use for the
#'   comparison
#' @param load `logical`. Should the results be in the context of an upload
#'   or a download operation? Permitted values are `"up"` or `"down"`
#' @param folders `logical`. Should folders/directories be included in the
#'   result?  
#'    
#' @return An object of class `boxr_dir_comparison`, describing the 
#'   differences between the files.
#'   
#'   It is a named list, it's entries containing data frames, 
#'   describing the files in each of the following categories:
#'   
#'   \describe{
#'     \item{**`new`**}{
#'       Files which are present in the origin, but not the destination. These 
#'       will be downloaded by [box_fetch()]/uploaded by 
#'       [box_push()].
#'     }
#'     \item{**`superfluous`**}{
#'       These are files which are present in the destination, but not the 
#'       origin. If `delete` is set to `TRUE` in 
#'       [box_fetch()]/[box_push()], they will be deleted.
#'     }
#'     \item{**`to_update`**}{
#'       Files which are present in both the origin and the destination, but
#'       which have more recently modified copies in the origin. If downloading
#'       with [box_fetch()], and `overwrite` set to `TRUE`,
#'       new files will overwrite existing local copies. If uploading with 
#'       [box_push()] (and `overwrite` set to `TRUE`), the 
#'       new version will be uploaded to box.com, with a new version number, and
#'       the old version still being available.
#'     }
#'     \item{**`up_to_date`**}{
#'       Files present in both origin and destination, with the same content.
#'       Note: A file may be modified at later date, but if it has identical 
#'       contents according to it's `sha1` hash, it will be considered
#'       up-to-date. [box_fetch()]/[box_push()] do nothing for these 
#'       files.
#'     }
#'     \item{**`behind`**}{
#'       Files which are present in both origin and destination, but where the
#'       content differs, and the version in the destination has been more
#'       recently updated. [box_fetch()]/[box_push()] do nothing for these 
#'       files.
#'     }
#'     \item{**`new_folders`**}{
#'       Analogous to the file operation, but for directories/folders.
#'     }
#'     \item{**`superfluous_folders`**}{
#'       Analogous to the file operation, but for directories/folders.
#'     }    
#'   }
#' 
#' @references
#'   <https://developers.box.com/docs>
#' 
#' @seealso [box_fetch()] and [box_push()], which depend on
#'   this internal function, [file.info()] for timestamps describing
#'   local files, [digest()] for details of the `sha1` algorithm
#'   implementation.
#'
#' @keywords internal
box_dir_diff <- function(dir_id = box_getwd(), local_dir = getwd(), load = "up",
                         folders = FALSE) {
  
  # Assertions
  if (!load %in% c("up", "down"))
    stop('load must be either "up" or "down"')
  
  if (!folders %in% c(TRUE, FALSE))
    stop('folders must be either TRUE or FALSE')
  
  checkAuth()
  assertthat::is.readable(local_dir)
  
  
  loc_dir_df <- create_loc_dir_df(local_dir)
  box_dir_df <- as.data.frame(box_ls(dir_id))
  
  b <- box_dir_df[box_dir_df$type == "file",]
  l <- loc_dir_df[loc_dir_df$type == "file",]
  
  if (folders) {
    b_folders <- box_dir_df[box_dir_df$type == "folder",]
    l_folders <- loc_dir_df[loc_dir_df$type == "folder",]    
  } else {
    b_folders <- data.frame()
    l_folders <- data.frame()
  }
  
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
  
  if (load == "up") {
    origin <- l
    destin <- b
    origin_folders <- l_folders
    destin_folders <- b_folders
  }
  
  if (load == "down") {
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
  
  # This isn't actually used, but is something a user might want to know
  absent_folders <-
    origin_folders[!origin_folders$name %in% destin_folders$name,]
  
  # Same content?
  if (length(present) > 0 && nrow(present) > 0L) {
    o_sha1 <- stats::setNames(origin$sha1, origin$name)
    d_sha1 <- stats::setNames(destin$sha1, destin$name)
    
    # Files in the origin which have changed, or not
    changed <- present[!d_sha1[present$name] == o_sha1[present$name],]
    nchange <- present[ d_sha1[present$name] == o_sha1[present$name],]
  } else {
    changed <- nchange <- data.frame()
  }
  
  # to_update: changed files, where the mod date at the origin is later than
  # the destination
  if (length(changed) > 0 && nrow(changed) > 0L) {
    changed <- merge(changed, destin, by = "name", all.x = TRUE, all.y = FALSE)
    to_update <- changed[changed$mod.x > changed$mod.y,]
    behind    <- changed[changed$mod.x < changed$mod.y,]
  } else {
    to_update <- behind <- data.frame()
  }
  
  if (class(absent) != "data.frame")
    absent <- data.frame()
  
  # The final list to output
  out <- 
    list(
      new                 = data.frame(absent),
      superfluous         = data.frame(superfluous),
      to_update           = data.frame(to_update),
      up_to_date          = data.frame(nchange),
      behind              = data.frame(behind),
      new_folders         = data.frame(absent_folders),
      superfluous_folders = data.frame(superfluous_folders)
    )
  
  # Make sure that every entry has a full_path variable
  out <- 
    lapply(
      out, 
      function(x) {
        if (!is.null(x) && nrow(x) > 0)
          x$full_path <- paste0(local_dir, "/", x$name)
        x
      }                
    )
  
  # Printable messages which describe the differences
  # At the moment there's nothing here for new folders (I suppose you don't need
  # that). Worth adding for the sake of completeness?
  diff_msg <- 
    c("new files", "superfluous files", "files to update", 
      "files already up-to-date", "files with newer versions in the destination", 
      "new folders", "superfluous folders")
  
  
  # Add some 'metadata' from the call itself
  out[["call_info"]] <- 
    list(load = load, local_dir = local_dir, dir_id = dir_id, msg = diff_msg)
  
  class(out) <- "boxr_dir_comparison"
  out
}


#' Create a data.frame of metadata of the contents of a local directory
#' 
#' @inheritParams dirTreeRecursive
#' @return A data.frame of metadata.
#' @keywords internal
create_loc_dir_df <- function(local_dir = getwd()) {
  
  fs <- list.files(local_dir, full.names = TRUE)
  if (length(fs) < 1L)
    return(data.frame())
  
  # Create a data.frame of metadata on the contents
  # of a local directory
  # You need to make this fail gracefully if the home directory is
  # empty (e.g.) return null
  fi <- file.info(fs)
  
  fi$mode <- as.character(fi$mode)
  
  df <- 
    data.frame(
      name = fs,
      fi,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  
  df$type <- ifelse(df$isdir, "folder", "file")
  df$sha1 <- NA
  
  # Add sha1 hashes for the files
  if (sum(!df$isdir) > 0L) {
    sha1 <-
      sapply(
        df$name[!df$isdir], 
        function(f)
          digest::digest(f, "sha1", file = TRUE)
      )
    
    df$sha1 <- sha1[df$name]
  }
  
  # Remove the filepath from the local name, add it to a full_path col
  # (You'll want to keep the full path for later)
  # df$full_path <- df$name
  df$name <- gsub(".*\\/", "", df$name)
  
  return(df)
}


#' Obtain a data.frame of the sub-directories in a box.com folder
#' 
#' Takes the `id` of a box folder and returns a data.frame of it's 
#' subdirectories, including their equivalent paths in the local directory.
#' 
#' @param dir_id The box.com id for the folder that you'd like to query
#' @param local_dir The local directory which you'd like `dir_id` to 
#' correspond to. If you're not interested in mapping local to hosted 
#' directories, this isn't terribly important.
#' @return A data.frame describing the contents directory structure of the 
#' box.com folder corresponding to `dir_id`.
#' @keywords internal
dirTreeRecursive <- function(dir_id, local_dir = getwd()) {
  
  dir_row <- 
    data.frame(
      type = "folder", id = dir_id, sequence_id = 0,
      etag = 0, name = "",
      local_dir = local_dir,
      path = "~",
      stringsAsFactors = FALSE
    )
  
  dirDirs <- function(dir_id) {
    df <- as.data.frame(box_ls(dir_id))
    return(df[df$type == "folder",])
  }
  
  dirTreeList <- function(dir_row) {
    sub_dirs <- dirDirs(dir_row[,"id"])
    
    if (!is.null(sub_dirs))
      if (nrow(sub_dirs) > 0) {
        sub_dirs$local_dir <- paste0(dir_row[,"local_dir"], "/", sub_dirs$name)
        sub_dirs$path      <- paste0(dir_row[,"path"], "/", sub_dirs$name)
      }
    
    tmp <- list(sub_dirs)
    
    if (!is.null(sub_dirs))
      if (nrow(sub_dirs) > 0)
        for (i in 1:nrow(sub_dirs))
          tmp <- c(tmp, Recall(sub_dirs[i,]))
    tmp
  }
  
  d <- dirTreeList(dir_row)
  
  dplyr::bind_rows(d)
}
