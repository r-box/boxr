#' Download/upload an R workspace from/to a Box file
#' 
#' These convenience functions aim to provide analagous functionality to 
#' [base::load()] and [base::save.image()] (or 
#' [base::save()]), but for `.RData` files stored on box.com, as 
#' opposed to locally.
#' 
#' @aliases box_load
#' 
#' @inheritParams box_dl
#' @param ... The objects to be saved. Quoted or unquoted. Passed to 
#'   [save()].
#' @param dir_id The box.com folder id where the objects will be stored as a
#'   `.RData` file.
#' @param file_name The name you'd like your `.Rdata` file saved as. For
#'   example, "myworkspace.RData"
#' @param file_id For `box_load`, the box.com id of the `.RData` or
#'   `.rda` file you'd like to load into your workspace.
#'
#' @details `box_save` saves an .RData file using 
#'   [base::save.image()] if `objects` is not supplied or 
#'   [base::save()] if it is. The file is then uploaded to box.com via 
#'   [box_ul()].
#' 
#'   `box_load` downloads a file from box.com using [box_dl()],
#'   and then [base::load()]s it into the current workspace.
#' 
#' @return `box_load` returns a character vector of the names of objects 
#'   created, invisibly. `box_save` and `box_save_image` are used for 
#'   their side effects, and doen't return anything.
#'   
#' @author Brendan Rocks \email{foss@@brendanrocks.com}
#' 
#' @seealso The base R functions which these wrap; [save()],
#'   [save.image()] and [load()].
#'   
#' @export
box_save <- function(..., dir_id = box_getwd(), file_name = ".RData", 
                     description = NULL) {
  temp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  save(..., file = temp_file)
  box_ul(dir_id, temp_file, description = description)
}

#' @rdname box_save
#' @export
box_save_image <- function(dir_id = box_getwd(), file_name = ".RData", 
                           description = NULL) {
  temp_file <- normalizePath(file.path(tempdir(), file_name), mustWork = FALSE)
  save.image(file = temp_file)
  
  box_ul(dir_id, temp_file, description = description)
}

#' @rdname box_save
#' @export
box_load <- function(file_id) {  
  temp_dir  <- tempdir()
  temp_file <- box_dl(file_id, overwrite = TRUE, local_dir = temp_dir)
  load(temp_file, envir = globalenv())
}
