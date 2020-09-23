#' List files in a Box directory
#'
#' Non-recursive
#'
#' @inheritParams box_setwd
#' @param limit  `integer`, maximum number of entries to retrieve per query-page.
#' @param max    `integer`, maximum number of entries to retrieve in total.
#' @param fields `character`, fields to return; the default
#'   value, `NULL`, will return all possible columns: `modified_at`,
#'   `content_modified_at`, `name`, `id`, `type`, `sha1` ,`size`,
#'   `owned_by`, `path_collection`, `description`.
#'
#' @return Object with S3 class [`boxr_object_list`][boxr_S3_classes].
#'
#' @seealso [box_fetch()] and [box_push()] for synchronizing the contents of
#'   local and remote directories.
#'
#' @export
box_ls <- function(dir_id = box_getwd(), limit = 100, max = Inf, fields = NULL) {
  
  if (limit > 1000) {
    warning("The maximum limit is 1000; box_ls is using 1000.")
    limit <- 1000
  }
  
  checkAuth()
    
  url_root <- "https://api.box.com/2.0"
  
  url <- httr::parse_url(
    paste(url_root, "folders", box_id(dir_id), "items", sep = "/")
  )
  
  fields_all <- 
    c("modified_at" ,"content_modified_at", "name", "id", "type",
      "sha1" ,"size", "owned_by", "path_collection", "description")
  
  if (is.null(fields)) {
    fields <- fields_all
  } else {
    assertthat::assert_that(
      all(fields %in% fields_all),
      msg = paste("all fields must be in", paste(fields_all, collapse = ", "))
    )
  }
  
  url$query <- list(
    fields = paste(fields, collapse = ","),
    limit = limit
  )
  
  out <- box_pagination(url = url, max = max)
  
  class(out) <- "boxr_object_list"
  
  return(out)
}


#' @keywords internal
box_pagination <- function(url, max){
    
  marker <- character(0)
  n_so_far <- 0
  out <- list()
  url$query$usemarker <- TRUE
  next_page <- TRUE
  
  while (next_page) {

    req <- httr::RETRY(
      "GET",
      url,
      get_token(),
      terminate_on = box_terminal_http_codes()
    )    

    if (req$status_code == 404) {
      message("Error 404: box.com indicates that no results were found, or the folder specified does not exist in your account.")
      return()
    }    

    httr::stop_for_status(req)
    resp <- httr::content(req)
    n_req    <- length(resp$entries)
    n_so_far <- n_so_far + n_req
    out <- c(out, resp$entries)
    marker <- resp$next_marker

    if (is.null(marker)) {
      next_page <- FALSE
    } else {
        url$query$marker <- marker
    }
    
    if (n_so_far >= max) {
      return(out)
    }
    
  }
  
  return(out)
  
}

#' Get/set Box default working-directory
#' 
#' @description
#' Similar to [getwd()] and [setwd()], 
#' these functions get and set the folder ID of the working directory 
#' at [box.com](https://box.com). 
#' 
#' This folder ID is also stored in [boxr_options()].
#'  
#' @aliases box_getwd
#' 
#' @param dir_id `numeric` or `character`, folder ID at Box.
#' 
#' @return \describe{
#'   \item{`box_getwd()`}{`numeric`, ID for working folder at Box.}
#'   \item{`box_setwd()`}{`invisible(NULL)`, called for side-effects.}
#' } 
#' 
#' @seealso [box_ls()] to list files in a Box directory, 
#'   [box_fetch()]/[box_push()] to download/upload directories from/to Box
#'  
#' @export
#' 
box_setwd <- function(dir_id) {
  
  checkAuth()
  
  req <- httr::RETRY(
    "GET",
    paste0(
      "https://api.box.com/2.0/folders/",
      box_id(dir_id)
    ),
    get_token(),
    terminate_on = box_terminal_http_codes()
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
  
  invisible(NULL)
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


#' Get boxr options
#' 
#' This function gets the values of boxr's global options.
#' 
#' Options can be set in the usual way, using [options()].
#'
#' @return `list`, current values of boxr options, with elements: 
#' 
#' \describe{
#'   \item{`boxr.interactive`}{`logical`, indicates if boxr is running in interactive mode.}
#'   \item{`boxr.progress`}{`logical`, indicates to use progress-bars, if available.}
#'   \item{`boxr.verbose`}{`logical`, indicates if boxr will use [cat()] to print to the console.
#'     Setting to `TRUE` may cause problems with `knitr`.}
#'   \item{`boxr.wd`}{`list`, containing information on the Box working-directory:
#'     `id` `(numeric)`, and `name` `(character)`.}
#'   \item{`boxr.wd.path`}{`character`, path to the Box working-directory.}
#'   \item{`boxr.token`}{Object with S3 class `Token2.0` (`httr::Token2.0`).}
#' }
#' 
#' @export
boxr_options <- function() {
  avail <- c(
    "boxr.token",
    "boxr.wd",
    "boxr.wd.path",
    "boxr.verbose",
    "boxr.progress",
    "boxr.interactive",
    "boxr_token_jwt"
  )
  
  o <- options()
  
  message("To set an option, use options()")
  message("e.g.: options(boxr.verbose = FALSE)")
  
  return(o[names(o) %in% avail])
}


#' Create a Box directory
#' 
#' This will create a new folder at Box, with name `dir_name`,
#' in the Box folder with ID `parent_dir_id`.
#' 
#' @param dir_name `character`, name for new folder at Box.
#' @param parent_dir_id `character` or `numeric`, 
#'   ID for the parent folder at Box.
#' 
#' @return Object with S3 class [`boxr_folder_reference`][boxr_S3_classes].
#' 
#' @seealso [box_delete_folder()] to move Box folders to trash,
#'   [box_ls()] to list files in a Box folder.
#' 
#' @export
#' 
box_dir_create <- function(dir_name, parent_dir_id = box_getwd()) {
  
  checkAuth()
  
  add_folder_ref_class(httr::content(
    boxDirCreate(dir_name, box_id(parent_dir_id))
  ))
}

#' @keywords internal
boxDirCreate <- function(dir_name, parent_dir_id = box_getwd()) {
  httr::RETRY(
    "POST",
    "https://api.box.com/2.0/folders/",
    get_token(),
    encode = "multipart",
    body = 
      paste0(
        '{"name":"', dir_name, '", "parent": {"id": "', box_id(parent_dir_id),
        '"}}'
      ),
    terminate_on = box_terminal_http_codes()
  )
}


#' Open a Box directory or file in browser
#' 
#' Thin wrapper of `utils::browseURL()` to make bouncing between R and Box a breeze.
#' 
#' @inheritParams box_collab_create
#' 
#' @return Invisible `NULL`, called for side effects.
#' 
#' @examples 
#' \dontrun{
#'   box_browse(0) # root folder on Box
#'   box_browse(file_id = 12345)
#' }
#' 
#' @export
#' 
box_browse <- function(dir_id = NULL, file_id = NULL) {
  item <- collab_item_helper(dir_id, file_id)
  utils::browseURL(glue::glue("https://app.box.com/{item$type}/{item$id}"))
}
