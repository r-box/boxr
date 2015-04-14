#' Read files from box.com into memory as R objects
#' 
#' @description {
#'   \code{box_read} will download a file specified by \code{file_id}, and
#'   attempt to read it into memory as an R object. This can be useful, for 
#'   example, to read in a \code{.csv} file as a \code{\link{data.frame}}.
#'   
#'   Converting a file to an R object is largely handled by \code{\link{httr}}'s
#'   \code{\link[httr]{content}} function, which can guess how best to convert
#'   the file. A specific MIME type can be set with the \code{type} parameter,
#'   although convenience functions are provided for common types:
#'   
#'   \describe{
#'     \item{\bold{\code{box_read_csv}}}{ Reads remote \code{.csv} files as 
#'       \code{\link{data.frame}}s (via \code{\link{read.csv}})
#'     }
#'     \item{\bold{\code{box_read_tsv}}}{ Reads remote \code{.tsv} files as 
#'       \code{\link{data.frame}}s (via \code{\link{read.delim}})
#'     }
#'     \item{\bold{\code{box_read_json}}}{ Reads remote \code{.json} files as 
#'       \code{\link{list}}s (via \code{\link[jsonlite]{toJSON}})
#'     }
#'   }
#' }
#' 
#' @param type Passed to \code{\link[httr]{content}}. MIME type (aka internet
#'   media type) used to override the content type returned by the server. See 
#'   http://en.wikipedia.org/wiki/Internet_media_type for a list of common types
#' 
#' @details
#'   \code{box_read} will attempt to coerce the remote file to an 
#'   \bold{\code{R}} object using httr's \code{\link[httr]{content}} function,
#'   which in general does a good job, especially converting \code{csv} files to
#'   a \code{\link{data.frame}}.
#'   
#'   However, at the time of writing, this isn't always successful with 
#'   JSON files, so \code{box_read} will try and convert any files with a 
#'   \code{.json} extension using \code{\link[jsonlite]{toJSON}}.
#'   
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#' 
#' @seealso \code{\link{box_dl}} for saving files to disk, 
#'   \code{\link{box_save}} for working with R workspaces, and 
#'   \code{\link{box_source}} for working with R code.
#'   
#' @inheritParams box_dl
#' 
#' @export
box_read <- function(file_id, type = NULL){
  checkAuth()
  
  req <- 
    httr::GET(
      paste0(
        "https://api.box.com/2.0/files/",
        file_id, "/content"
      ),
      httr::config(token = getOption("boxr.token"))
    )
  
  filename <- 
    gsub(
      'filename=\"|\"', '',
      stringr::str_extract(
        req$headers["content-disposition"][[1]],
        'filename=\"(.*?)\"'
      )
    )
  
  # Currently, httr works well with .csv files, but doesn't to a great job with
  # json.
  probably_json <- grepl("\\.json$", filename)
  
  if(is.null(type) & probably_json){
    cont <- jsonlite::fromJSON(httr::content(req, as = "text"))
  } else {
    cont <- httr::content(req, type = type)
  }
  
  if(is.raw(cont))
    warning(filename, " appears to be a binary file.")
  
  message(filename, " read into memory.\n")
  
  return(cont)
}

#' @rdname box_read
#' @export
box_read_csv <- function(file_id){
  box_read(file_id, type = "text/csv")
}

#' @rdname box_read
#' @export
box_read_tsv <- function(file_id){
  box_read(file_id, type = "text/tab-separated-values")
}

#' @rdname box_read
#' @export
box_read_json <- function(file_id){
  box_read(file_id, type = "application/json")
}
