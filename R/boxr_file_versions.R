#' Get details about versions of a Box file
#' 
#' Box uses file versioning, but the API does not explicitly provide version numbers.
#' These functions use `modified_date` as a proxy and allow programatic version access
#' in `box_dl()` and `box_read()`
#' 
#' * `box_previous_version()` returns a
#' `data.frame` containing information on a file's previous 
#' versions on Box, the column `file_version_id` can be passed to `version_id`
#' in `box_dl()` and `box_read()`.
#' If the version of a file is one, then NULL is returned invisibly
#' along with a helpful message.
#' 
#' * `box_current_version()` returns a `integer`, starting from 1, which can be passed
#' to `version_no = ` in `box_dl()` and `box_read()`.
#' 
#' @inheritParams box_dl
#' 
#' @return `data.frame` containing information about previous 
#'   versions of the file (if available). 
#'   
#' @references
#'   This function is a light wrapper of the 
#'   [box.com](https://developer.box.com/docs) API `versions` method.
#'   
#'   <https://developers.box.com/docs/#files-view-versions-of-a-file>
#' 
#' @seealso [box_dl()], [box_read()]
#' 
#' @export
#' 
box_previous_versions <- function(file_id) {

  req <- box_version_api(file_id)

  # The box API isn't very helpful if there are no previous versions. If this
  # is the case, let the user know and exit.
  if (is_void(req[["entries"]])) {
    message("No previous versions for this file found.")
    return(invisible(NULL))
  }

  # Munge it into a data.frame
  d <- suppressWarnings(
    purrr::map_df(
      req$entries,
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
  # no specific order information in the response:
  # loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooool
  # 
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

#'
#' @rdname box_previous_versions
#' @export
box_current_version <- function(file_id) {
  
  # file_id <- 682127082014 # ver 3
  # file_id <- 682162782067 # ver1
  
  req <- box_version_api(file_id)
  
  ver <- req[["total_count"]] + 1

  message("Box file ", file_id, " is version ", ver)
  
  ver
}
#' Wrap and resuse
#' at_internal
#' @inheritParams box_dl
box_version_api <- function(file_id) {
  checkAuth()
  
  req <- httr::RETRY(
    "GET",
    paste0(
      "https://api.box.com/2.0/files/",
      file_id, "/versions"
    ),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  httr::content(req)
}

