#' Authenticate box.com account
#' 
#' @param client_id The client id for the account you'd like to use. 
#' \code{character}.
#' @param client_sc The client secret for the account you'd like to use. \code{character}.
#' @param interactive \code{logical}. Should the authorization process happen interactively (requiring user input to the R console, and/or a visit to box.com)?
#' @param use_oob Passed to \code{oob} in \code{\link{httr}}.
#' @param as_header Passed to \code{as_header} in \code{\link{httr}}.
#' @param cache Passed to \code{cache} in \code{\link{httr}}.
#' @param write.Renv \code{logical}. If they were missing, and an OAuth2.0 token was obtained, should \code{client_id} and \code{client_sc} be written to \code{.Renvirons} in  your \code{HOME} directory? (Note: The \code{HOME} dir is not neccesarily that returned by \code{geetwd()}.)
#' @param reset.Renv \code{logical}. Should existing values for \code{client_id} and \code{client_sc} in \code{.Renvirons} be ignored?
#' @return Involked for it's side effect; OAuth2.0 connection to the box.com API.
box_auth <- function(
  client_id = "",
  client_sc = "",
  interactive = TRUE,
  use_oob = getOption("httr_oob_default"), 
  as_header = TRUE,
  cache = getOption("httr_oauth_cache"),
  write.Renv = TRUE,
  reset.Renv = FALSE
  ){

  # If the user hasn't input any, look to .Renviron for the
  # id and secret
  if(client_id == "")
    if(Sys.getenv("BOX_CLIENT_ID") != ""){
      message("Reading client id from .Renviron")
      client_id <- Sys.getenv("BOX_CLIENT_ID")
    }
  
  if(client_sc == "")
    if(Sys.getenv("BOX_CLIENT_SECRET") != ""){
      message("Reading client secret from .Renviron")
      client_sc <- Sys.getenv("BOX_CLIENT_SECRET")
    }
    
  # UI for interactively entering ids and secrets
  if(client_id == "" & interactive){
    message("Please enter your box client id. If you don't have one")
    message("see the documentation at ?box_auth, and hit ENTER to exit.")
    client_id <- readline()
    
    # Tidy up any invalid characters
    client_id <- gsub("[[:space:]]|[[:punct:]]", "", client_id)
    if(nchar(client_id) == 0L) return()
  }
  
  if(client_sc == "" & interactive){
    message("Please enter your box client secret")
    message("(Hit ENTER to exit.)")
    client_sc <- readline()
    
    # Tidy up any invalid characters
    client_sc <- gsub("[[:space:]]|[[:punct:]]", "", client_sc)
    if(nchar(client_sc) == 0L) return()
  }
  
  box_app <- 
    httr::oauth_app(
      appname = "box",
      key     = client_id,
      secret  = client_sc
    )
  
  box_endpoint <- 
    httr::oauth_endpoint(
      authorize = "authorize",
      access    = "token",
      base_url  = "https://app.box.com/api/oauth2/"
    )
  
  box_token <- 
    httr::oauth2.0_token(
      box_endpoint,
      box_app, 
      use_oob   = use_oob,
      as_header = as_header,
      cache     = cache
    )
  
  if(!exists("box_token"))
    stop("Login at box.com failed; unable to connect to API.")
  
  # Write the details to the Sys.env
  app_details <- 
    setNames(list(client_id, client_sc), c("BOX_CLIENT_ID", "BOX_CLIENT_SECRET"))
  do.call(Sys.setenv, app_details)

  # Write the details to .Renviron
  if(write.Renv){
    re <- readLines(paste0(Sys.getenv("HOME"), "/.Renviron"))
    # Remove any where they details were previously set, and write the new ones
    # to the end of the file
    writeLines(
      c(
        re[!grepl("BOX_CLIENT_ID=|BOX_CLIENT_SECRET=", re)],
        paste0('BOX_CLIENT_ID="',     client_id, '"'),
        paste0('BOX_CLIENT_SECRET="', client_sc, '"')
      ),
      con = paste0(Sys.getenv("HOME"), "/.Renviron")
    )
    message("Writing client_id and client_scret to")
    message(normalizePath("~/"), ".Renviron")
  }
  
  # Write to options
  options(boxr.token = box_token)
  # Set box_wd - Should this look in the .RProfile or.Renvirons?
  options(box_wd = "0")
  
}


#' Obtain a data.frame describing the contents of a directory
#' 
#' @param dir_id The box.com id for the folder that you'd like to query
#' @return A data.frame describing the contents of the the folder specified by \code{dir_id}. Non recursive.
box_ls <- function(dir_id){
  req <- 
    httr::GET(
      paste0(
        "https://api.box.com/2.0/folders/",
        dir_id, "/items"
      ),
      httr::config(token = getOption("boxr.token"))
    )
  
  # A data.frame of the metadata of the files in the folder
  do.call(
    plyr::rbind.fill,
    lapply(
      httr::content(req)$entries, 
      function(x) data.frame(x, stringsAsFactors = FALSE)
    )
  )
}





#' Download and upload individual files from box.com
#' 
#' \code{box_dl} takes the \code{id} of a file hosted on box.com, downloads 
#' it and writes it to disk.
#' 
#' \code{box_read} does the same, but reads it into memory as an \code{R}
#' object. This can be useful, for example, to read a \code{.csv} file into
#' memory as a \code{\link{data.frame}}.
#' 
#' \code{box_ul} uploads a file stored locally to a specified box.com folder.
#' If a file with the same name already exists, it will store a new version of 
#' the file.
#' 
#' @aliases box_read
#' @param file_id The box.com id for the file that you'd like to download
#' @param overwrite \code{logical}. Should existing files with the same name be 
#' overwritten?
#' @param local_dir A file path to a local directory which you'd like the file
#' to be downloaded to.
#' @param filename The filename for the local version of the file. The default,
#'  \code{NULL}, uses the name from box.com.
#' @param file the path to the local file that you'd like to upload (if there is
#'  one)
#' @param dir_id If uploading, the box.com folder id that you'd like to upload
#' to.
#' 
#' @inheritParams dirTreeRecursive
#' 
#' @return \code{TRUE}. Used for it's side-effect (a downloaded file)
box_dl <- 
  function(
    file_id, overwrite = FALSE, local_dir = getwd(),
    filename = NULL
  ){
  
  if(is.null(filename))
    filename <- "TEMP"
  
    req <- 
      httr::GET(
        paste0(
          "https://api.box.com/2.0/files/",
          file_id, "/content"
        ),
        httr::config(token = getOption("boxr.token")),
        httr::write_disk(paste0(local_dir, "/", filename), overwrite)
      )

  if(filename != "TEMP")
    return(paste0(local_dir, "/", filename))
  
  # If not supplied, extract filename from request headers
  # Extract filename
    filename <- 
      gsub(
        'filename=\"|\"', '',
        stringr::str_extract(
          req$headers["content-disposition"][[1]],
          'filename=\"(.*?)\"'
        )
      )
    
  # Rename the file if it's got a temporary name
  if(overwrite | !file.exists(filename))
    file.rename(
      paste0(local_dir, "/TEMP"),
      paste0(local_dir, "/", filename)
    )

  paste0(local_dir, "/", filename)
}


#' @rdname box_dl
box_ul <- function(file, dir_id){
  
  # First try and upload it
  ul_req <- box_upload_new(file, dir_id)
  
  # If that worked, end it here
  if(httr::http_status(ul_req)$cat == "success")
    # You should add an s3 class first
    return(httr::content(ul_req))
  
  # If it didn't work, because there's already a file with that name (http
  # error code 409), use the 'update' api
  if(httr::content(ul_req)$status == 409){
    message(
      "File '", basename(file),"' aleady exists. Attempting to upload new ",
      "version",
      " (V", as.numeric(httr::content(ul_req)$context_info$conflicts$sequence_id) + 2,
      ")."
    )
    
    ud_req <- 
      box_update_file(
        file, 
        httr::content(ul_req)$context_info$conflicts$id,
        dir_id
      )
    
    # If this works, end it here
    if(httr::http_status(ud_req)$cat == "success")
      return(httr::content(ud_req))
    
    # If it doesn't, try to end as informatively as possible!
    ud_error_msg <- httr::content(ud_req)$context_info$errors[[1]]$message
    
    if(!is.null(ud_error_msg))
      stop(ud_error_msg)
    
    httr::stop_for_status(ud_req)
  }
  
  # If it doesn't, try to end as informatively as possible!
  ul_error_msg <- httr::content(ul_req)$context_info$errors[[1]]$message
  
  if(!is.null(ul_error_msg))
    stop(ul_error_msg)
  
  httr::stop_for_status(ul_req)
  
}


#' @rdname box_dl
box_read <- function(file_id){
    
  req <- 
    httr::GET(
      paste0(
        "https://api.box.com/2.0/files/",
        file_id, "/content"
      ),
      httr::config(token = getOption("boxr.token"))
    )
  
  filename <- 
    gsub(
      'filename=\"|\"', '',
      stringr::str_extract(
        req$headers["content-disposition"][[1]],
        'filename=\"(.*?)\"'
      )
    )
  
  
  
  cont <- httr::content(req)
  if(is.raw(cont))
    warning(filename, " appears to be a binary file.")
   
  message(filename, " read into memory.\n")
  
  return(cont)
    
}



#' set directory
#' 
#' set directory
#'  
#' @aliases box_getwd
#' 
#' @inheritParams dirTreeRecursive 
#' 
#' @return Nothing. Used for its side-effects.
box_setwd <- function(dir_id){
  req <- 
    httr::GET(
      paste0(
        "https://api.box.com/2.0/folders/",
        dir_id
      ),
      httr::config(token = getOption("boxr.token"))
    )
  
  cont <- httr::content(req)
  
  if(cont$type != "folder")
    stop("box.com API error message:\n", cont$message)
  
  path_str <- 
    do.call(
      function(...) paste(..., sep="/"), 
      lapply(
        cont$path_collection$entries,
        function(x) x$name
      )
    )
  
  item_types <- lapply(cont$item_collection$entries, function(x) x$type)
  
  options(
    box_wd = cont,
    box_wd_path_str = path_str
  )
  
  message(
    "box.com working directory changed to ",
    "'", cont$name, "'",
    if(is.null(getOption("box_wd_path_str")) & cont$name == "All Files")
      " (top level box.com folder)",    
    "\n\n",
    "      id: ", cont$id, "\n",
    "    tree: ", path_str, "\n",
    "   owner: ", cont$owned_by$login, "\n",
    "contents: ", sum(item_types == "file"), " files, ",
    sum(item_types == "folder"), " folders\n",
    if(cont$description != "")
      paste0("\ndescription: \n    ", cont$description, "\n\n"),
    if(!is.null(cont$shared_link))
      paste0("shared link: ", cont$shared_link$url)
  )
  
}



#' @rdname box_setwd
box_getwd <- function(){
  
  if(is.null(getOption("box_wd"))){
    message("No box.com working directory set")
    return(invisible())
  }
  
  cont <- getOption("box_wd")
  
  if(cont$id != getOption("box_wd")$id)
    stop("Something has gone horribly wrong. Please try to send a 
          reproducible example to the pakcage maintainer. The problem
          may be fixed by re-setting the working directory with
          box_setwd().
         ")
  
  message(
    "'", cont$name, "'",
    if(cont$id == "0")
      " (top level box.com folder)",    
    "\n\n",
    if(getOption("box_wd")$id != "0")
      paste0(" tree: ", getOption("box_wd_path_str"), "\n"),
    "owner: ", cont$owned_by$login, "\n"
  )
  
  return(getOption("box_wd")$id)
}


#' Display global options for boxr
#' 
#' Run \code{boxr_options()} to see what it's possible to set globally.
#' 
#' @details You should really write out all the options here, and explain them!
#' 
#' @return A \code{list} of the options available
boxr_options <- function(){
  avail <- 
    c(
      "boxr.token",
      "boxr.wd",
      "boxr.wd.path",
      "boxr.verbose",
      "boxr.progress",
      "boxr.interactive"
    )
  
  o <- options()
  
  message("To set an option, use options()")
  message("e.g.: options(boxr.verbose = FALSE)")
  
  return(o[names(o) %in% avail])
  
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
downloadDirFiles <- function(dir_id, local_dir = getwd(), overwrite = TRUE){
  
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
    
    to_update <- present[!l_sha1[present] == b_sha1[present]]
  }
  
  # If overwrite is false, ignore to_update
  if(!overwrite)
    to_update <- ""
  
  # The ids of files to download
  dl_ids <- setNames(box_dir_df$id, box_dir_df$name)[c(absent, to_update)]
  
  # Note, specifies filenames from the names of the dl_ids vector
  # to write straight to disk
  lapply(
    dl_ids,
    function(x)
      box_dl(x, filename = names(x), overwrite = TRUE, local_dir)
  )
  return(TRUE)
}

#' @rdname downloadDirFiles
uploadDirFiles <- function(dir_id, local_dir = getwd()){
  
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
  
  if(length(lf) > 0L){
    b_sha1 <- setNames(box_dir_df$sha1[box_dir_df$type == "file"], bf)
    l_sha1 <- setNames(loc_dir_df$sha1[loc_dir_df$type == "file"], lf)
    
    to_update <- present[!l_sha1[present] == b_sha1[present]]
  }
  
  
  update_names <- box_dir_df$name[box_dir_df$name %in% to_update]
  update_ids   <- box_dir_df$id[box_dir_df$name %in% to_update]
  
  # Run through the files to update, and upload up dates
  # NOTE: insert messages/progress bars here
  ud_l <- 
    mapply(
      function(file_name, file_id)
        box_update_file(
          file.path(local_dir, file_name),
          file_id,
          dir_id
        ),
        update_names,
        update_ids 
    )
  
  # Run through the files to upload, and upload up dates
  # NOTE: insert messages/progress bars here
  ul_l <- 
    lapply(
      absent,
      function(x)
        box_upload_new(file.path(local_dir, x), dir_id)
    )
  
  
  return(TRUE)
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
  
  do.call(rbind, d[!unlist(lapply(d, is.null))])
}









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
box_upload_new <- function(file, dir_id){
  
  httr::POST(
    "https://upload.box.com/api/2.0/files/content",
    httr::config(token = getOption("boxr.token")),
    encode = "multipart",
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
box_update_file <- function(file, file_id, dir_id){
  httr::POST(
    paste0("https://upload.box.com/api/2.0/files/", file_id, "/content"),
    httr::config(token = getOption("boxr.token")),
    encode = "multipart",
    body = 
      list(
        attributes = 
          paste0(
            '{"name": "', basename(file), '", "parent": {"id":"', dir_id,'"}}'
          ),
        file = upload_file(file)
      )
  )
}




#' Create a new box.com folder
#' 
#' Create a new box.com folder
#' 
#' @param dir_name The name for the directory you'd like to create.
#' @param parent_dir_id The box.com folder id of the folder you'd like your new
#' folder to be within.
#' 
#' @return The \code{\link{httr}} object returned by the api call
#' @keywords internal
box_dir_create <- function(dir_name, parent_dir_id){
  httr::POST(
    "https://api.box.com/2.0/folders/",
    httr::config(token = getOption("boxr.token")),
    encode = "multipart",
    body = 
      paste0(
        '{"name":"', dir_name, '", "parent": {"id": "', parent_dir_id, '"}}'
      )
  )
}  






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
#' @return Nothing. Used for its side-effects.
box_fetch <- 
  function(
    dir_id, local_dir = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  ){
    if(!recursive){
      downloadDirFiles(dir_id, local_dir)
      # You need to insert a message here
      return(invisible(TRUE))
    }
    # 1. Recursively scan the box dir for folders
    d <- dirTreeRecursive(dir_id)
    
    # 2. Update the files in the tld
    downloadDirFiles(dir_id, local_dir, overwrite = overwrite)
    
    # Loop through the box dirs. If they don't exist, create them.
    # Once they do, fill 'em up!
    for(i in 1:nrow(d)){
      dir.create(d$local_dir[i], showWarnings = FALSE)
      downloadDirFiles(d$id[i], d$local_dir[i], overwrite = overwrite)
    }
  }


#' @rdname box_fetch
box_push <- 
  function(dir_id, local_dir = getwd(), ignore_dots = TRUE){
    
    local_dirs <- list.dirs()[-1]
    if(ignore_dots)
      local_dirs <- local_dirs[!grepl("^\\.\\/\\.", local_dirs)]
    
    dir_depth <- 
      unlist(
        lapply(
          gregexpr("\\/", local_dirs),
          function(x) length(attr(x, "match.length"))
        )
      )
    
    # Order the dirs by depth
    local_dirs <- local_dirs[order(dir_depth)]
    
    # A version where the dir names will be replaced with thier box.com ids
    box_dirs <- gsub("^\\.\\/", paste0(dir_id, "/"), local_dirs)
    
    for(i in 1:length(box_dirs)){
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
        message(
          "Created box.com folder (id: ", new_dir_id, ") ", local_dirs[i]
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
      uploadDirFiles(
        new_dir_id, 
        paste0(local_dir, gsub("^\\.", "", local_dirs[i]))
      )
    }
  }

#' @rdname box_fetch
box_merge <- function(dir_id, local_dir = getwd(), ignore_dots = TRUE){
  box_push(local_dir, dir_id, ignore_dots = ignore_dots)
  box_fetch(local_dir, dir_id)
}




#' Save and load \code{R} workspaces via box.com
#' 
#' These convenience functions aim to provide analagous functionality to 
#' \code{\link[base]{load}} and \code{\link[base]{save.image}} (or 
#' \code{\link[base]{save}}), but for \code{.RData} files stored on box.com, as 
#' opposed to locally.
#' 
#' @aliases box_load
#' 
#' @param file_name The name you'd like your \code{.Rdata} file saved as. For
#' example, "myworkspace.RData"
#' @param objects Optional. A \code{\link[base]{list}} of \code{R} objects to be 
#' saved. If ommitted, all the objects in the current workspace will be saved.
#' @param dir_id The box.com folder id where the objects will be stored as a
#' \code{.RData} file.
#' @param file_id For \code{box_load}, the box.com id of the \code{.RData} or
#' \code{.rda} file you'd like to load into your workspace.
#' 
#' @details \code{box_save} saves an .RData file using \code{\link[base]{save.image}} if 
#' \code{objects} is not supplied or \code{\link[base]{save}} if it is. The file is 
#' then uploaded to box.com via \code{\link{box_ul}}.
#' 
#' \code{box_load} downloads a file from box.com using \code{\link{box_dl}},
#' and then \code{\link[base]{load}}s it into the current workspace.
#' 
#' @return \code{box_load} returns a character vector of the names of objects 
#' created, invisibly. \code{box_load} doesn't return anything.
box_save <- function(file_name = ".RData", objects = character(), dir_id){
  
  temp_file <- file.path(tempdir(), file_name)
  
  if(length(objects) == 0L){
    save.image(temp_file)
  } else {
    save(objects, file = temp_file)
  }
  
  box_ul(temp_file, dir_id)
  
}

#' @rdname box_save
box_load <- function(file_id){
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  load(temp_file)
}

