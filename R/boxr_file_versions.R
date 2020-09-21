#' Get version information
#' 
#' Box uses file versioning, but the API does not explicitly provide version 
#' numbers. These functions use `modified_date` as a proxy to determine a 
#' version number (`version_no`), which you can use with [box_dl()] and 
#' [box_read()].
#' 
#' - `box_version_history()`, previously called `box_previous_versions()`,
#' gets information on all previous versions of a file. If there are no
#' previous versions, this function returns `NULL`.
#' 
#' - `box_version_number()` gets the version number of the most-recent version.
#' 
#' - To access the Box version API itself, you can use [box_version_api()].
#' 
#' @inheritParams box_dl
#' 
#' @return \describe{
#'   \item{`box_previous_versions()`}{
#'     `data.frame` describing previous versions of file}
#'   \item{`box_version()`}{
#'     `integer` version number of most-recent version of file}
#' }
#'   
#' @references
#'   This function is a light wrapper of the 
#'   [box.com](https://developer.box.com/docs) API `versions` method.
#'   
#'   <https://developers.box.com/docs/#files-view-versions-of-a-file>
#' 
#' @seealso [box_version_api()], [box_dl()], [box_read()]
#' 
#' @export
#' 
box_version_history <- function(file_id) {
  
  content <- box_version_api(file_id)
  
  if (is_void(content)) {
    message(
      glue::glue("No previous versions for file {file_id} found.")
    )
    return(invisible(NULL))
  }
  
  as.data.frame(content)
}

#' Get version information
#' 
#' \lifecycle{superseded} Superseded by [box_version_history()].
#' 
#' @inheritParams box_dl
#' 
#' @return `data.frame` describing previous versions of file
#' 
#' @export
#' 
box_previous_versions <- function(file_id) {
  lifecycle::deprecate_soft(
    "3.6.0", 
    what = "box_previous_versions()", 
    with = "box_version_history()"
  )
  prev_versions(file_id)
}

# internal function to support superseding
prev_versions <- function(file_id) {
  
  entries <- box_version_api(file_id)
  
  # The box API isn't very helpful if there are no previous versions. If this
  # is the case, let the user know and exit.
  if (is_void(entries)) {
    message(
      glue::glue("No previous versions for file {file_id} found.")
    )
    return(invisible(NULL))
  }
  
  # Munge it into a data.frame
  d <- suppressWarnings(
    purrr::map_df(
      entries,
      function(x) data.frame(
        t(unlist(x)),
        stringsAsFactors = FALSE
      )
    )
  )
  
  # Fix the dates
  d$created_at  <- box_datetime(d$created_at)
  d$modified_at <- box_datetime(d$modified_at)
  
  # Make it clear that the ids are for file versions, not files themselves
  colnames(d)[colnames(d) == "id"] <- "file_version_id"
  
  # The box API has started returning these in arbitrary order, and there's
  # no specific order information in the response
  # The best you can do is probably modified at
  message("Version ordering inferred from file modification dates. The box.com API ",
          "does not provide explicit version information.")
  
  d <- d[order(d$modified_at),]
  # row.names are confusing after re-ordering
  rownames(d) <- NULL
  
  # Add explicit version numbers (the data is returned in reverse order by the
  # API)
  d <- cbind(version = paste0("V", 1:nrow(d)), d)
  
  # Superfluous
  d$type <- NULL
  
  d
  
}

#' @rdname box_version_history
#' @export
#' 
box_version_number <- function(file_id) {
  
  entries <- box_version_api(file_id)
  
  # use `entries` to protect against paginiation
  ver <- as.integer(length(entries) + 1)

  message("Box file ", file_id, " has current version ", ver, ".")
  
  ver
}

#' Access Box version API
#' 
#' Use this function to access the response-content for the 
#' versions API endpoint.
#' 
#' @inheritParams box_dl
#' 
#' @return Object with S3 class `"boxr_version_list"`
#' 
#' @keywords internal
#' @export
#' 
box_version_api <- function(file_id) {
  
  # TODO: consider pagination
  
  checkAuth()
  
  response <- httr::RETRY(
    "GET",
    glue::glue("https://api.box.com/2.0/files/{file_id}/versions"),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  content <- httr::content(response)
  
  result <- content$entries
  
  class(result) <- "boxr_version_list"
  
  result
}

