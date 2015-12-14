#' Read files from box.com into memory as R objects
#' 
#' @description {
#'   \code{box_read} will download a file specified by \code{file_id}, and
#'   attempt to read it into memory as an R object. This can be useful, for 
#'   example, to read in a \code{.csv} file as a \code{\link{data.frame}}.
#'   
#'   Converting a file to an R object is by default handled by 
#'   \code{\link{rio}}'s \code{\link[rio]{import}} function. The only 
#'   modification of it's behavior is that json files are not neccesarily
#'   coerced to \code{data.frame}s, but can used to store \code{list} data, too.
#'   In addtion, more specific read functions are provided:
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
#'     \item{\bold{\code{box_read_excel}}}{ Reads remote Microsoft Excel files
#'     as \code{\link{data.frame}}s (via \code{\link[readxl]{read_excel}})
#'     }
#'   }
#' }
#' 
#' @param type MIME type (aka internet media type) used to override the content
#'   type returned by the server. See 
#'   http://en.wikipedia.org/wiki/Internet_media_type for a list of common types
#' @param read_fun The function used to read the data into R. Defaults to 
#'   \code{\link{rio}}::\code{\link{import}}
#' @param fread Should the function \code{data.table::fread} be used to read 
#'   \code{.csv} files? Passed to \code{\link{rio}}::\code{\link{import}} (if 
#'   used).
#' @param ... Passed to as additional parameters to read_fun
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
box_read <- function(file_id, type = NULL, version_id = NULL, 
                     version_no = NULL, read_fun = rio::import, fread = FALSE,
                     ...) {
  checkAuth()
  
  temp_file <- tempfile()
  
  # Make the request
  req <- boxGet(file_id, local_file = temp_file, version_id = version_id, 
                version_no = version_no, download = TRUE)

  # Extract the filename
  filename <- gsub(
    'filename=\"|\"', '',
    stringr::str_extract(
      req$headers["content-disposition"][[1]],
      'filename=\"(.*?)\"'
    )
  )
  
  # Give the file it's original name back, so that you can preserve the file
  # extension
  new_name <- paste0(tempdir(), "/", filename)
  file.rename(temp_file, new_name)
  
  # If the file doesn't have an obvious file extension, try and do the right
  # thing by considering the mime-type from the request
  if (!grepl("\\.[[:alnum:]]+$", new_name)) {
    message("Cannot read file extension from name.\n",
            "Inferring from mime-type...\n")
    mime <- req$headers$`content-type`
    ext  <- stats::setNames(names(mime::mimemap), mime::mimemap)[mime]
    if (is.na(ext)) {
      stop("File has no extension, and is of unknown mime-type:\n",
           "    ", mime, "\n")
    }
    # Supply the file format to read_fun, if it seems to accept them (the 
    # default, rio::import, does)
    if ("format" %in% names(formals(read_fun))) {
      cont <- read_fun(new_name, format = ext, ...)
    } else {
      # Otherwise, just try and read it with a user-supplied function
      cont <- read_fun(new_name, ...)
    }
  } else {
    cont <- read_fun(new_name, ...)
  }
  
  # rio is imposing the data.frame class on .json files, which isn't lolz.
  # So, if it's classed as a data.frame but doesn't have the 'row.names'
  # attribute, unclass it
  if ("data.frame" %in% class(cont) & is.null(attr(cont, "row.names"))) {
    cont <- unclass(cont)
  }
  
  # Delete the tempfile
  unlink(temp_file, force = TRUE)
  
  message(
    "Remote file '", new_name, "' read into memory as an object of class ", 
    paste(class(cont), collapse = ", "),
    "\n"
  )
  
  return(cont)
}


#' @rdname box_read
#' @export
box_read_csv <- function(file_id, ...) {
  box_read(file_id, format = "csv", ...)
}


#' @rdname box_read
#' @export
box_read_tsv <- function(file_id, ...) {
  box_read(file_id, format = "tsv", ...)
}


#' @rdname box_read
#' @export
box_read_json <- function(file_id, ...) {
  box_read(file_id, format = "json", ...)
}


#' @rdname box_read
#' @export
box_read_excel <- function(file_id, ...) {
  box_read(file_id, format = "excel", ...)
}
