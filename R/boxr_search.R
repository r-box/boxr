#' Search the files in a box.com account
#' 
#' @details {
#'  The box.com API supports a maximum of 200 results per request. If
#'  \code{max > 200}, then multiple requests will be sent to retrieve and
#'  combine 'paginated' results for you, behind the scenes.
#' 
#'  See the \href{https://community.box.com/t5/Managing-Your-Content/Search-Overview-and-FAQs/ta-p/354}{box.com search description}
#'  for details of the features of the service.
#'  Some notable details:

#'   \describe{
#'     \item{\bold{Full Text Searching}}{
#'       Is available for many source code file types, though not including R at
#'       the time of writing.
#'     }
#'     \item{\bold{Boolean Operators Are Supported}}{
#'       Such as \code{and}, \code{or}, and \code{not} (upper or lower case).
#'     }
#'     \item{\bold{Phrases Can Be Searched}}{
#'       By putting them in "quotation marks".
#'     }
#'     \item{\bold{Search Availability}}{
#'       It takes around 10 minutes for a newly uploaded file to enter the 
#'       search index.
#'     }
#'   }
#' }
#' 
#' @param query The search term that you'd like to use
#' @param content_types The content types you'd like to search. More than one 
#'   can be supplied with a vector
#' @param type The type of object you'd like returned. The default of 
#'   \code{NULL} return all possible types. Other acceptable values are "file",
#'   "folder", or "weblink"
#' @param file_extensions A vector of strings containing the file extensions 
#'   (without dots) by which to narrow your search.
#' @param ancestor_folder_ids If supplied, results are limited to one or more 
#'   parent folders
#' @param created_at_range Optional. Limit search to a range of created at 
#'   times. A vector of two dates (coercible via \code{\link{as.POSIXct}}).
#' @param updated_at_range Optional. Limit search to a range of updated at 
#'   times. A vector of two dates (coercible via \code{\link{as.POSIXct}}).
#' @param size_range Optional. Limit search to a range of file sizes, in bytes.
#'   A vector of two whole numbers (coerible via (coercible via
#'   \code{\link{as.numeric}} and \code{\link{round}})).
#' @param trash Should the search be limited to the trash folder? 
#'   \code{\link{logical}}.
#' @param owner_user_ids Optional. Limit search to a files owned by a set of 
#'   users. A vector if IDs, coercible with \code{\link{as.integer64}}.
#' @param auto_paginate \code{logical}. By default, the box.com will return a 
#'   fixed number of search results per request. By setting auto_paginate to 
#'   \code{TRUE}, boxr will keep making new requests untill all search results 
#'   are in.
#' @param max \code{numeric}. Upper limit on the number of search results 
#'   returned (protective measure for users with large numbers of files).
#' @param ... Parameters passed to \code{box_search}
#'
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' 
#' @return An object of class \code{boxr_object_list}. See 
#'   \code{\link{boxr_S3_classes}} for details.
#'   
#' @export
#' 
box_search <- function(
  query = "", 
  content_types = c("name", "description", "file_content", "comments", "tags"),
  type = NULL, file_extensions = NULL, 
  ancestor_folder_ids = NULL, created_at_range = NULL, updated_at_range = NULL,
  size_range = NULL, trash = FALSE, owner_user_ids = NULL, auto_paginate = TRUE,
  max = 200
) {
  
  # Validation & Coercion ---------------------------------------------------  
  checkAuth()
  # For converting dates to box's preferred format
  to_rfc3339 <- function(x) {
    as.character(x, format = "%Y-%m-%dT%H:%M:%SZ")
  } 
  
  # To validate and coorce dates for created_at_range / updated_at_range
  coerce_dates <- function(x) {
    # NULL is meaningful, and should be preserved. If NULL, exit.
    if (all(is.null(x)))
      return(x[1])
    
    x <- try(as.POSIXct(x), silent = TRUE)
    if ("try-error" %in% class(x) | length(x) != 2)
      stop("'created_at_range' and/or 'updated_at_range' must be a vector of ",
           "length 2, containing data coercible via as.POSIXct().")
    return(to_rfc3339(x))
  }
  
  # To validate and coerce bytes for size_range
  coerce_bytes <- function(x) {
    # NULL is meaningful, and should be preserved. If NULL, exit.
    if (all(is.null(x)))
      return(x[1])
    
    # Note, you validate with as.numeric to check that they're numbers, but you
    # encode as character, to prevent scientific notation ending up in the URL.
    # as.integer freaks out with large numbers, so avoiding here.
    x <- try(round(as.numeric(x)), silent = TRUE)
    if ("try-error" %in% class(x) | length(x) != 2)
      stop("'size_range' must be a vector of length 2, containing data",
           "coercible to whole numbers via round(as.numeric(size_range)).")
    return(format(x, scientific = FALSE))
  }
  
  # Validate 'type'. Slightly more specific for the user than using
  # match.args(), which is desirable given all these confusing options
  valid_types <- c("file", "folder", "weblink")
  
  if (!is.null(type) && !type %in% valid_types)
    stop("'type' must be one of : ", paste(valid_types, collapse = ", "))
  
  # Validate 'content_types'
  valid_content_types <- 
    c("name", "description", "file_content", "comments", "tags")
  
  if (!is.null(content_types) && !all(content_types %in% valid_content_types))
    stop("'content_types' may contain the following values only: ",
         paste(valid_content_types, collapse = ", "))
  
  # Validate ids 
  ancestor_folder_ids <- box_id(ancestor_folder_ids)
  owner_user_ids      <- box_id(owner_user_ids)
  
  # Validate trash
  if (!trash %in% c(TRUE, FALSE))
    stop("'trash' must be either TRUE or FALSE.")
  
  # Validate date and size ranges
  created_at_range <- coerce_dates(created_at_range)
  updated_at_range <- coerce_dates(updated_at_range)
  size_range       <- coerce_bytes(size_range)
  
  # Encode the search query
  query <- utils::URLencode(query)
  
  # Assemble the URL --------------------------------------------------------
  
  extra_params <- c(
    "content_types",
    "type",
    "file_extensions",
    "ancestor_folder_ids",
    "created_at_range",
    "updated_at_range",
    "size_range",
    "owner_user_ids"
  )
  
  # Pluck a param from the function environment, and turn it's values (if it has
  # any) into an appropriate string
  encode_params <- function(x) {
    if (!is.null(get(x)))
      return(paste0(x, "=", paste(get(x), collapse = ","), "&"))
  }
  
  params <- unlist(lapply(extra_params, encode_params))

  url <- paste0(c(
    "https://api.box.com/2.0/search?",
    params,
    if (trash)
      "&trash_content=trashed_only&",
    # Note: The box.com API will handle a maximum of 200 responses per request
    "limit=200&",
    "query=", query
  ), collapse = "")
  
  # return(url)
  
  # Make the request --------------------------------------------------------
    
  out <- box_search_pagination(url, max = max)
  
  # In the case of a 404 (nothing found), out == NULL
  if (is.null(out))
    return()

  class(out) <- "boxr_object_list"
  return(out)
}

#' @name box_search
#' @export
box_search_files <- function(query, ...) {
  box_search(query, type = "files", ...)
}

#' @name box_search
#' @export
box_search_folders <- function(query, ...) {
  box_search(query, type = "folders", ...)
}

#' @name box_search
#' @export
box_search_trash <- function(query, ...) {
  box_search(query, trash = TRUE, ...)
}

#' @keywords internal
box_search_pagination <- function(url, max = 200) {
  out       <- list()
  next_page <- TRUE
  page      <- 1
  n_so_far  <- 0
  
  while (next_page) {
    page_url <- paste0(url, "&offset=", (page - 1) * 200)
    req      <- httr::GET(url, httr::config(token = getOption("boxr.token")))
    
    if (req$status_code == 404) {
      message("box.com indicates that no search results were found")
      return()
    }
    
    httr::stop_for_status(req)
    
    resp     <- httr::content(req)
    n_req    <- length(resp$entries)
    n_so_far <- n_so_far + n_req
    total    <- resp$total_count
    
    if (!n_so_far < total | n_so_far >= max) 
      next_page <- FALSE
    
    out <- c(out, resp$entries)
  }
  
  return(out)
}
