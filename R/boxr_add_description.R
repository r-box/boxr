#' Add description to a Box file
#' 
#' These functions will attach a description or comment to a Box file. A new
#' description will overwrite an existing one.
#' 
#' Files hosted at Box can have small text-descriptions that 
#' you can be use to annotate files, or even to
#' 
#' @inheritParams box_dl
#' 
#' @return Object with S3 class [`boxr_file_reference`][boxr_S3_classes].
#' 
#' @export
box_add_description <- function(file_id, description) {
  file_id <- handle_file_id(file_id)
  
  req <- httr::RETRY(
    "PUT",
    paste0("https://api.box.com/2.0/files/", file_id),
    body = paste0('{"description":"', description, '"}'),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  httr::stop_for_status(req)
  add_file_ref_class(httr::content(req))
}

#' Create/delete Box comments
#' 
#' Comments can be associated with files or other comments. If a `comment_id` 
#' is used it will be posted as a reply. Retreiving comments en bulk is only
#' possible on a `file_id`. 
#' 
#' @md
#' @inheritParams box_dl
#' @param msg `character` the comment Note: tagging people with the
#' \@user pattern is *not* supported at this time.
#' @param comment_id `numeric` or `character`, comment ID at Box. 
#' @return Invisible, the `comment_id` of the newly created Box comment.
#' @examples 
#' \dontrun{
#' box_comment(12345, "Rmd report is ready for review")
#' box_comment(comment_id = 98756, msg = "Responsed to your change requests!")
#' }
#' @export
box_comment_create <- function(file_id = NULL, msg, comment_id = NULL) {
  # Could support tagging users with @name, but requires tricky formatting with userID
  # https://developer.box.com/reference/post-comments/
  
  checkAuth()
  
  item <- comment_item_helper(file_id, comment_id)
  
  req <- httr::RETRY(
    "POST",
    "https://api.box.com/2.0/comments/",
    encode = "multipart",
    body = jsonlite::toJSON(
      list(
        "item" = item,
        "message" = msg
      ),
      auto_unbox = TRUE
    ),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  message("Comment left on ", item$type, " ", item$id, ".")
  invisible(httr::content(req))
}
#' @rdname box_comment_create
#' @return A `data.frame` with a row for each comment.
#' @examples 
#' \dontrun{
#' box_comment(12345, "Rmd report is ready for review")
#' }
#' @export
box_comment_get <- function(file_id) {
  req <- httr::RETRY(
    "GET",
    glue::glue("https://api.box.com/2.0/files/{file_id}/comments"),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  httr::stop_for_status(req)
  
  httr::content(req) %>% 
    # should this be lifted in a helper, stack_list_element_into_dataframe()
    .[["entries"]] %>%
    purrr::map_df(as.data.frame) %>% 
    dplyr::mutate(file_id = file_id)
}
