#' Read an R object from a Box file
#' 
#' These functions are used to download a Box file, specified by `file_id`, then
#' attempt to parse its contents into memory as an R object. For 
#' example, you may wish to read a Box CSV file as a `data.frame`.
#' 
#' This is a two-step process. The first is to download the contents
#' of the file, the second is to parse those contents into an R object.
#' The default parsing-function is [rio::import()].
#' 
#' In addition to `box_read()`, some specific helpers are
#' provided:
#' 
#' \describe{
#'   \item{`box_read_csv()`}{parse a remote CSV file into a `data.frame` using [read.csv()]}
#'   \item{`box_read_tsv()`}{parse a remote TSV file into a `data.frame` using [read.delim()]}
#'   \item{`box_read_json()`}{parse a remote JSON file into a `list` using [jsonlite::fromJSON()]}
#'   \item{`box_read_excel()`}{parse a remote Microsoft Excel file into a `data.frame` using [readxl::read_excel()]}
#' }
#' 
#' @inheritParams box_dl
#' @param type `character`, 
#'   [MIME type](http://en.wikipedia.org/wiki/Internet_media_type)  
#'   used to override the content type returned by the server. 
#' @param read_fun `function`, used to read (parse) the content into R; default 
#'   function is [rio::import()].
#' @param fread `logical`, indicates to use function [data.table::fread()] 
#'   to read CSV files.
#' @param ... Other arguments passed to `read_fun`.
#'   
#' @return Object returned by function `read_fun`.   
#' 
#' @seealso [box_dl()], [box_save()], [box_source()]
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
