#' Authenticate box.com account
#' 
#' You'll need to set up an 'app' at https://www.box.com/developers/services
#' 
#' 
#' @param client_id The client id for the account you'd like to use. 
#' \code{character}.
#' @param client_secret The client secret for the account you'd like to use. 
#' \code{character}.
#' @param interactive \code{logical}. Should the authorization process happen 
#' interactively (requiring user input to the R console, and/or a visit to 
#' box.com)?
#' @param use_oob Passed to \code{oob} in \code{\link{httr}}.
#' @param as_header Passed to \code{as_header} in \code{\link{httr}}.
#' @param cache Passed to \code{cache} in \code{\link{httr}}.
#' @param write.Renv \code{logical}. If they were missing, and an OAuth2.0 token
#' was obtained, should \code{client_id} and \code{client_secret} be written to 
#' \code{.Renvirons} in  your \code{HOME} directory? (Note: The \code{HOME} dir
#' is not neccesarily that returned by \code{geetwd()}.)
#' @param reset.Renv \code{logical}. Should existing values for \code{client_id}
#' and \code{client_secret} in \code{.Renvirons} be ignored?
#' @return Invoked for it's side effect; OAuth2.0 connection to the box.com 
#' API.
#' @export
box_auth <- function(
  client_id = "",
  client_secret = "",
  interactive = TRUE,
  use_oob = getOption("httr_oob_default"), 
  as_header = TRUE,
  cache = "~/.boxr-oath",
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
  
  if(client_secret == "")
    if(Sys.getenv("BOX_CLIENT_SECRET") != ""){
      message("Reading client secret from .Renviron")
      client_secret <- Sys.getenv("BOX_CLIENT_SECRET")
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
  
  if(client_secret == "" & interactive){
    message("Please enter your box client secret")
    message("(Hit ENTER to exit.)")
    client_secret <- readline()
    
    # Tidy up any invalid characters
    client_secret <- gsub("[[:space:]]|[[:punct:]]", "", client_secret)
    if(nchar(client_secret) == 0L) return()
  }
  
  # At this point, a non-interactive call may still have no id & secret
  # If there's
  if(client_id == "" | client_secret == "")
    stop(
      "box.com authorization unsuccessful; client id and/or secret not found.
       See ?box_auth for help!"
    )
  
  box_app <- 
    httr::oauth_app(
      appname = "box",
      key     = client_id,
      secret  = client_secret
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
    setNames(list(client_id, client_secret), c("BOX_CLIENT_ID", "BOX_CLIENT_SECRET"))
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
        paste0('BOX_CLIENT_SECRET="', client_secret, '"')
      ),
      con = paste0(Sys.getenv("HOME"), "/.Renviron")
    )
    message("Writing client_id and client_secret to")
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
#' @return A data.frame describing the contents of the the folder specified by 
#' \code{dir_id}. Non recursive.
#' @export
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
  plyr::rbind.fill(
    lapply(
      httr::content(req)$entries, 
      function(x) data.frame(x, stringsAsFactors = FALSE)
    )
  )
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
#' @export
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
#' @export
box_getwd <- function(){
  
  if(is.null(getOption("box_wd"))){
    message("No box.com working directory set")
    return(invisible())
  }
  
  cont <- getOption("box_wd")
  
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
#' @export
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
#' @export
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
