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
  options(box_token = box_token)
  # Set box_wd
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
      httr::config(token = getOption("box_token"))
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





#' Download a file from box.com
#' 
#' Takes the \code{id} of a file hosted on box.com, downloads it, either 
#' writing it to disk (\code{box_dl}), or loading it as an \code{R} object 
#' (\code{box_read}).
#' 
#' @aliases box_read
#' @param file_id The box.com id for the file that you'd like to download
#' @param overwrite \code{logical}. Should existing files with the same name be 
#' overwritten?
#' @param filename The filename for the local version of the file. The default,
#'  \code{NULL}, uses the name from box.com.
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
        httr::config(token = getOption("box_token")),
        httr::write_disk(paste0(local_dir, "/", filename), overwrite)
      )

  if(filename != "TEMP")
    return(TRUE)
  
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

}



#' @rdname box_dl
box_read <- function(file_id){
    
  req <- 
    httr::GET(
      paste0(
        "https://api.box.com/2.0/files/",
        file_id, "/content"
      ),
      httr::config(token = getOption("box_token"))
    )
  
  filename <- 
    gsub(
      'filename=\"|\"', '',
      stringr::str_extract(
        req$headers["content-disposition"][[1]],
        'filename=\"(.*?)\"'
      )
    )
  
  
  
  cont <- content(req)
  if(is.raw(cont))
    warning(filename, " appears to be a binary file.")
   
  message(filename, " read into memory.\n")
  
  return(cont)
    
}



#' Synchronize the directory structure and file contents of a box.com folder
#' 
#' Download the contents of a box.com directory
#' 
#' @param recursive \code{logical}. Should the call include subdirectories and 
#' thier contents?
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
    updateDirFiles(dir_id, local_dir)
    # You need to insert a message here
    return(invisible(TRUE))
  }
  # 1. Recursively scan the box dir for folders
  d <- dirTreeRecursive(dir_id)
  
  # 2. Update the files in the tld
  updateDirFiles(dir_id, local_dir, overwrite = overwrite)
  
  # Loop through the box dirs. If they don't exist, create them.
  # Once they do, fill 'em up!
  for(i in 1:nrow(d)){
    dir.create(d$local_dir[i], showWarnings = FALSE)
    updateDirFiles(d$id[i], d$local_dir[i], overwrite = overwrite)
  }
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
      httr::config(token = getOption("box_token"))
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



#' Synchronize the contents of a box.com direcory loc
#' ally
#' 
#' Download the contents of a box.com directory, not including subdirectories
#' 
#' @inheritParams dirTreeRecursive
#' 
#' @return \code{TRUE} for a successful sync, \code{NULL} if the box.com folder 
#' is empty.
#' @keywords internal
updateDirFiles <- function(dir_id, local_dir = getwd(), overwrite = TRUE){
  
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















