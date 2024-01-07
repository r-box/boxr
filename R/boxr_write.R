#' Write an R object to a Box file
#' 
#' Use these functions to serialize an R object and write it
#' to a Box file. To write an object using RDS serialization, 
#' use `box_save_rds()`; for other types of serialization, 
#' use `box_write()` and provide a serialization function.
#' 
#' Using `box_save_rds()` is relatively straightforward, your
#' object will be written to Box as an RDS file.
#' 
#' If you want to specify the serialization, use `box_write()`.
#' For example, you may wish to write a `data.frame`
#' to Box as a CSV file. Within `box_write()`, this is a 
#' two-step process:
#' 
#'   - serialize the contents of the R object using `write_fun`
#'   - upload that serialization to a Box file
#'   
#' The default serialization-function is [rio::export()].
#' 
#' The [rio::export()] function currently supports only `data.frame`; 
#' to serialize lists, you may wish to use `jsonlite::toJSON()`.
#' 
#' Please note that `box_write()` is used to write R objects to Box files 
#' using standard formats. To write R objects as `.RData` files, 
#' you can use [box_save()].
#'
#' @inheritParams box_ul
#' @param object Object to be written.
#' @param file_name `character`, name of the new Box file.
#' @param write_fun `function`, used to write (serialize) the content from R; 
#'  default function is [rio::export()].
#' @param ... Other arguments passed to `write_fun`.
#' @param x Object to be written, **deprecated**: use `object` instead.
#' @param filename `character`, **deprecated**: use `file_name` instead.
#' 
#' @return Object with S3 class [`boxr_file_reference`][boxr_S3_classes].
#' 
#' @seealso [saveRDS()], [box_save()]
#' 
#' @export
#' 
box_write <- function(object, file_name, dir_id = box_getwd(), description = NULL,
                    write_fun = rio::export, x, filename, ...) {
  
  dir_id <- as_box_id(dir_id)
  
  # TODO: in future version, remove argument
  if (!missing(filename)) {
    
    warning(
      "argument `filename` is deprecated; please use `file_name` instead.", 
      call. = FALSE
    )
    
    if (missing(file_name)) {
      file_name <- filename
    }
  }
  
  if (!missing(x)) {
    
    warning(
      "argument `x` is deprecated; please use `object` instead.", 
      call. = FALSE
    )
    
    if (missing(object)) {
      object <- x
    }
  }
  
  temp_file <- paste0(tempdir(), "/", file_name)
  write_fun(object, temp_file, ...)
  box_ul(dir_id = dir_id, file = temp_file, description = description)
}

#' @rdname box_write
#' @export
box_save_rds <- function(object, dir_id = box_getwd(), file_name = ".RDS",
                         description = NULL) {

  dir_id <- as_box_id(dir_id)
  
  temp_file <- fs::path_temp(file_name)
  on.exit(fs::file_delete(temp_file))
  
  saveRDS(object, temp_file)
  
  box_ul(dir_id, temp_file, description = description)
}

