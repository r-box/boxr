#' Add description or comment to a Box file
#' 
#' These functions will attach a description or comment to a Box file. A new
#' description will overwrite an existing one. Comments are associated by id, which
#' can be a fileID or commentID, if commentID it will be posted as a reply.
#' 
#' Files hosted at Box can have small text-descriptions that 
#' you can be use to annotate files, or even to
#' leave 'git commit' style messages.
#' 
#' @inheritParams box_dl
#' @param comment `character` the comment to be make. Tagging people with the
#' \@user pattern is supported
#' @return Object with S3 class [`boxr_file_reference`][boxr_S3_classes].
#' 
#' @export
#' 
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

#' @rdname box_add_description
#' @examples 
#' \dontrun{
#' box_comment(12345, "Rmd report is ready for review")
#' }
#' @export
#' 
box_comment <- function(id, msg, id_type = "file") {
  # Could support tagging users with @name, but requires tricky formatting with userID
  # https://developer.box.com/reference/post-comments/
  
  checkAuth()
  
  req <- httr::RETRY(
    "POST",
    "https://api.box.com/2.0/comments/",
    encode = "multipart",
    body = jsonlite::toJSON(
      list(
        "item" = list(
          "id" = id,
          "type" = id_type
        ),
        "message" = msg
      ),
      auto_unbox = TRUE
    ),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  message("Comment left on ", id_type, " ", id, ".")
  invisible(httr::content(req))
}
#' @rdname box_add_description
#' @examples 
#' \dontrun{
#' box_comment(12345, "Rmd report is ready for review")
#' }
#' @export
#' 
box_get_comments <- function(file_id) {
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
