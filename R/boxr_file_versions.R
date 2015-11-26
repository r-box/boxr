#' Retrieve details of previous versions of a remote file
#' 
#' box.com explicitly versions files. \code{box_previous_versions} returns a
#' \code{\link{data.frame}} containing information on a file's previous 
#' versions on box.com. No information about the current version of the file is
#' returned.
#' 
#' @param file_id the id of the file you'd like version information about
#' 
#' @return A \code{\link{data.frame}} containing information about previous 
#'   versions of the file (if available). Importantly, it contains the 
#'   \code{file_version_id}, which can be passed to \code{\link{box_dl}}.
#'
#' @references
#'   This function is a light wrapper for box.com API's \code{versions} method.
#'   
#'   \url{https://developers.box.com/docs/#files-view-versions-of-a-file}
#'   
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' 
#' @seealso \code{\link{box_dl}}
#' 
#' @export
box_previous_versions <- function(file_id) {
  checkAuth()
  
  req <- httr::GET(
    paste0(
      "https://api.box.com/2.0/files/",
      file_id, "/versions"
    ),
    httr::config(token = getOption("boxr.token"))
  )
  
  # Munge it into a data.frame
  d <- suppressWarnings(
    data.frame(
      dplyr::rbind_all(lapply(httr::content(req)$entries, data.frame))
    )
  )
  
  # Fix the dates
  d$created_at  <- box_datetime(d$created_at)
  d$modified_at <- box_datetime(d$modified_at)
  
  # Make it clear that the ids are for file versions, not files themselves
  colnames(d)[colnames(d) == "id"] <- "file_version_id"
  
  # Add explicit version numebers (the data is returned in reverse order by the
  # API)
  d <- cbind(version = paste0("V", nrow(d):1), d)
  
  d <- d[order(d$version),]
  
  # Superfluous
  d$type <- NULL
  
  d
}

