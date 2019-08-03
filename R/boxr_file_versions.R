#' Get details of previous versions of a Box file
#' 
#' Box explicitly versions files; this function returns a
#' `data.frame` containing information on a file's previous 
#' versions on Box. No information about the current version of the file is
#' returned.
#' 
#' The returned `data.frame` contains a variable, `file_version_id`, 
#' which you can use with [box_dl()].
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
#' @seealso [box_dl()]
#' 
#' @export
#' 
box_previous_versions <- function(file_id) {
  checkAuth()
  
  req <- httr::GET(
    paste0(
      "https://api.box.com/2.0/files/",
      file_id, "/versions"
    ),
    httr::config(token = getOption("boxr.token"))
  )
  
  # The box API isn't very helpful if there are no previous versions. If this
  # is the case, let the user know and exit.
  if (is.null(httr::content(req)[["entries"]])) {
    message("No previous versions for this file found.")
    return(NULL)
  }
  
  # Munge it into a data.frame
  d <- suppressWarnings(
    data.frame(
      dplyr::rbind_all(lapply(
        httr::content(req)$entries,
        function(x) data.frame(t(unlist(x)))
      ))
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
  message("Versions inferred from file modification dates. The box.com API ",
          "does not provide explicit version information.")
  
  d <- d[order(d$modified_at),]
  
  # Add explicit version numbers (the data is returned in reverse order by the
  # API)
  d <- cbind(version = paste0("V", 1:nrow(d)), d)
  
  # Superfluous
  d$type <- NULL
  
  d
}

