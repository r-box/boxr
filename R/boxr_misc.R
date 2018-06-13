#' Obtain a data.frame describing the contents of a box.com folder
#' 
#' @param dir_id The box.com id for the folder that you'd like to query
#' @param limit  Maximum number of entries to retrieve per query-page
#' @param max    Maximum number of entries to retrieve in total
#' 
#' @return A data.frame describing the contents of the the folder specified by 
#'   \code{dir_id}. Non recursive.
#'   
#' @author Brendan Rocks \email{foss@@brendanrocks.com} and Ian Lyttle 
#'   \email{ian.lyttle@@schneider-electric.com}
#'   
#' @seealso \code{\link{box_fetch}} and \code{\link{box_push}} for synchronizing
#'   the contents of local and remote directories. \code{\link{list.files}} for
#'   examining the contents of local directories.
#'   
#' @export
box_ls <- function(dir_id = box_getwd(), limit = 100, max = Inf) {
  
  # maybe some logic here to check that limit <= 1000
  
  checkAuth()
  
  url_root <- "https://api.box.com/2.0"
  
  url <- httr::parse_url(
    paste(url_root, "folders", box_id(dir_id), "items", sep = "/")
  )

  fields <- c("modified_at" ,"content_modified_at", "name", "id", "type",
              "sha1" ,"size", "owned_by", "path_collection", "description")
  
  url$query <- list(
    fields = paste(fields, collapse = ","),
    limit = limit
  )
  
  out <- box_pagination(url, max = max)
  
  class(out) <- "boxr_object_list"
  return(out)
}

#' Shorter version of box_ls.
#' 
#' @param dir_id The box.com id for the folder that you'd like to query
#' @param limit  Maximum number of entries to retrieve per query-page
#' @param max    Maximum number of entries to retrieve in total
#' 
#' @return A data.frame describing the contents of the the folder specified by 
#'   \code{dir_id}. Non recursive.
#'   
#' @author Alec Wong \email{aw685@cornell.edu}
#'   
#' @seealso \code{\link{box_fetch}} and \code{\link{box_push}} for synchronizing
#'   the contents of local and remote directories. \code{\link{list.files}} for
#'   examining the contents of local directories.
#'   
#' @export
box_ls_short <- function(dir_id = box_getwd(), limit = 1000, max = Inf) {
  
  # maybe some logic here to check that limit <= 1000
  
  checkAuth()
  
  url_root <- "https://api.box.com/2.0"
  
  url <- httr::parse_url(
    paste(url_root, "folders", box_id(dir_id), "items", sep = "/")
  )
  
  fields <- c("name")
  
  url$query <- list(
    fields = paste(fields, collapse = ","),
    limit = limit
  )
  
  out <- box_pagination(url, max = max)
  
  class(out) <- "boxr_object_list"
  return(out)
}


#' @keywords internal
box_pagination <- function(url, max = 200) {
  
  out       <- list()
  next_page <- TRUE
  page      <- 1
  n_so_far  <- 0
  
  while (next_page) {
    
    limit <- url$query$limit
    
    url$query$offset <- as.integer((page - 1) * limit)
    
    req      <- httr::GET(
      url, 
      httr::config(token = getOption("boxr.token"))
    )
    
    if (req$status_code == 404) {
      message("box.com indicates that no results were found")
      return()
    }
    
    httr::stop_for_status(req)
    
    resp     <- httr::content(req)
    n_req    <- length(resp$entries)
    n_so_far <- n_so_far + n_req
    total    <- resp$total_count
    
    if (!n_so_far < total | n_so_far >= max)
      next_page <- FALSE
    
    out     <- c(out, resp$entries)
    page    <- page + 1
  }
  
  return(out)
}


#' Get/Set Default box.com directory/folder
#' 
#' @description Providing analgous functionality for the jbase \bold{\code{R}}
#'   functions \code{\link{getwd}} and \code{\link{setwd}}, these functions set 
#'   and retrieve a default box.com dir_id, stored in 
#'   \code{\link{boxr_options}}.
#'  
#' @aliases box_getwd
#' 
#' @param dir_id The id of the folder you'd like to set as your default
#' 
#' @return \code{box_getwd} returns the id of the default folder. 
#'   \code{box_setwd} does nothing and is ussed for its side-effects.
#'   
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
#'   
#' @seealso \code{\link{box_ls}} to examine the contents of a remote directory, 
#'   and \code{\link{box_fetch}}/\code{\link{box_push}} for synchorizing them.
#'  
#' @export
box_setwd <- function(dir_id) {
  
  checkAuth()
  
  req <- httr::GET(
    paste0(
      "https://api.box.com/2.0/folders/",
      box_id(dir_id)
    ),
    httr::config(token = getOption("boxr.token"))
  )
  
  cont <- httr::content(req)
  
  if (cont$type != "folder")
    stop("box.com API error message:\n", cont$message)
  
  path_str <- do.call(
    function(...) paste(..., sep="/"), 
    lapply(
      cont$path_collection$entries,
      function(x) x$name
    )
  )
  
  path_str <- paste0(path_str, "/", cont$name)
  
  item_types <- lapply(cont$item_collection$entries, function(x) x$type)
  
  options(
    boxr.wd      = cont,
    boxr.wd.path = path_str
  )
  
  message(
    "box.com working directory changed to ",
    "'", cont$name, "'",
    if (is.null(getOption("boxr.wd.path")) & cont$name == "All Files")
      " (top level box.com folder)",    
    "\n\n",
    "      id: ", cont$id, "\n",
    "    tree: ", path_str, "\n",
    "   owner: ", cont$owned_by$login, "\n",
    "contents: ", sum(item_types == "file"), " files, ",
    sum(item_types == "folder"), " folders\n",
    if (cont$description != "")
      paste0("\ndescription: \n    ", cont$description, "\n\n"),
    if (!is.null(cont$shared_link))
      paste0("shared link: ", cont$shared_link$url)
  )
}


#' @rdname box_setwd
#' @export
box_getwd <- function() {
  
  if (is.null(getOption("boxr.wd"))) {
    stop("No box.com working directory set")
    return(invisible())
  }
  
  return(getOption("boxr.wd")$id)
}


#' Display global options for boxr
#' 
#' Run \code{boxr_options()} to see what it's possible to set globally.
#' 
#' @details
#' Options can be set in the usual way using \code{\link{options}}, and include:
#' 
#' \describe{
#'   \item{\code{box.verbose}}{
#'     Should boxr print to the console using \code{\link{cat}}? This is
#'     slightly 'rude' package behaviour, and may cause problems if using the 
#'     \code{knitr} package.
#'   }
#'   \item{\code{box.wd}}{
#'     A list containg the name and id of the default box.com directory
#'   }
#'   \item{\code{box.token}}{
#'     The token object used for authentication
#'   }
#' }   
#'
#' @return A \code{list} of the options available
#' 
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
#' 
#' @seealso \code{\link{box_setwd}} for another way to set the default box.com 
#'   directory
#' 
#' @export
boxr_options <- function() {
  avail <- c(
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
#'   folder to be within.
#' 
#' @return An object of class 
#'   \code{\link[=boxr_S3_classes]{boxr_folder_reference}}.
#' 
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
#' 
#' @seealso \code{\link{box_delete_folder}} to delete remote 
#'   folders/directories, \code{\link{box_ls}} to examine their conetents.
#' 
#' @export
box_dir_create <- function(dir_name, parent_dir_id = box_getwd()) {
  
  checkAuth()
  
  add_folder_ref_class(httr::content(
    boxDirCreate(dir_name, box_id(parent_dir_id))
  ))
}

#' @keywords internal
boxDirCreate <- function(dir_name, parent_dir_id = box_getwd()) {
  httr::POST(
    "https://api.box.com/2.0/folders/",
    httr::config(token = getOption("boxr.token")),
    encode = "multipart",
    body = 
      paste0(
        '{"name":"', dir_name, '", "parent": {"id": "', box_id(parent_dir_id),
        '"}}'
      )
  )
}
