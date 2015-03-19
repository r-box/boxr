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
        dir_id, 
        "/items?fields=modified_at,content_modified_at,name,id,type,sha1"
      ),
      httr::config(token = getOption("boxr.token"))
    )
    
  d <- data.frame(dplyr::bind_rows(
    lapply(
      httr::content(req)$entries, 
      function(x) data.frame(x, stringsAsFactors = FALSE)
    )
  ))



  d$modified_at         <- box_datetime(d$modified_at)
  d$content_modified_at <- box_datetime(d$content_modified_at)
  
  return(d)
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

