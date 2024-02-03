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
#'   \item{`box_read_csv()`}{parse a remote CSV file into a `data.frame`. Default
#'   read-function is [rio::import()] with `format = "csv"`, which uses [data.table::fread()].}
#'   \item{`box_read_tsv()`}{parse a remote TSV file into a `data.frame`. Default
#'   read-function is [rio::import()] with `format = "tsv"`, which uses [data.table::fread()].}
#'   \item{`box_read_json()`}{parse a remote JSON file into a R object. Default
#'   read-function is [jsonlite::fromJSON()].}
#'   \item{`box_read_excel()`}{parse a remote Microsoft Excel file into a `data.frame`. Default
#'   read-function is [rio::import()] with `format = "excel"`, which uses [readxl::read_excel()].}
#'   \item{`box_read_rds()`}{parse an RDS file into a R object. Uses [readRDS()].}
#' }
#' 
#' @section rio's import() and JSON files:
#' In rio (0.5.18) there was a change in how JSON files are processed by
#' [rio::import()], a non-`data.frame` object stored in JSON is no longer coerced
#' into a `data.frame`. The old behavior would produce unexpected results or fatal errors
#' if the stored object was not a `data.frame`. The new behavior is closer to that
#' of the underlying function [jsonlite::fromJSON()] and similar to the behavior for RDS files.
#' 
#' In keeping with the spirit of `jsonlite`, `box_read_json()` has been
#' modified to call `jsonlite::fromJSON()` directly, which by-passes the old
#' "undesirable" behavior of `rio` (< 0.5.18). If you are using the current CRAN
#' release of `rio` (0.5.16) you should use [jsonlite::read_json()] to avoid these issues.
#' 
#' @inheritParams box_dl
#' @param type `character`, 
#'   [MIME type](https://en.wikipedia.org/wiki/Internet_media_type)  
#'   used to override the content type returned by the server. 
#' @param read_fun `function`, used to read (parse) the content into R; for `box_read()`
#'   the default function is [rio::import()]; the specific helpers
#'   each use a different function directly.
#' @param ... Other arguments passed to `read_fun`.
#'   
#' @return Object returned by function `read_fun`.   
#' 
#' @seealso [box_dl()], [box_save()], [box_source()]
#'   
#' @export
box_read <- function(file_id, type = NULL, version_id = NULL, 
                     version_no = NULL, read_fun = rio::import,
                     ...) {
  checkAuth()
  
  temp_file <- withr::local_tempfile() 
  
  # Make the request
  req <- boxGet(file_id, local_file = temp_file, version_id = version_id, 
                version_no = version_no, download = TRUE)

  # Extract the filename and extension
  filename <- gsub(
    'filename=\"|\"', '',
    stringr::str_extract(
      req$headers["content-disposition"][[1]],
      'filename=\"(.*?)\"'
    )
  )
  file_ext <- glue::glue(".{fs::path_ext(filename)}")
  
  # Give the file its original file-extension back
  temp_file_new <- withr::local_tempfile(fileext = file_ext)
  file.rename(temp_file, temp_file_new)
  
  # If the file doesn't have an obvious file extension, try and do the right
  # thing by considering the mime-type from the request
  if (!grepl("\\.[[:alnum:]]+$", temp_file_new)) {
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
      cont <- read_fun(temp_file_new, format = ext, ...)
    } else {
      # Otherwise, just try and read it with a user-supplied function
      cont <- read_fun(temp_file_new, ...)
    }
  } else {
    cont <- read_fun(temp_file_new, ...)
  }
  
  message(
    "Remote file '", filename, "' read into memory as an object of class ", 
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
  box_read(file_id, read_fun = jsonlite::fromJSON, ...)
}


#' @rdname box_read
#' @export
box_read_excel <- function(file_id, ...) {
  box_read(file_id, format = "excel", ...)
}

#' @rdname box_read
#' @export
box_read_rds <- function(file_id, ...) {
  box_read(file_id, read_fun = readRDS, ...)
}

