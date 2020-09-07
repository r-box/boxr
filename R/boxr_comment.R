
#' Create/delete Box comments
#' 
#' Comments can be associated with files or other comments. If a `comment_id` 
#' is used it will be posted as a reply. Retrieving comments in bulk is only
#' possible on a `file_id`. 
#' 
#' @md
#' @inheritParams box_dl
#' @param message `character` the comment Note: tagging people with the
#' \@user pattern is *not* supported at this time.
#' @param comment_id `numeric` or `character`, comment ID at Box. 
#' @return Invisible, a `list` object of class [`boxr_comment_create`][boxr_S3_classes] with the API's
#' response values. 
#' @examples 
#' \dontrun{
#' file_id <- 12345
#' x <- box_comment_create(file_id, "Report is ready.")
#' box_comment_create(comment_id = x$id, message = "Response to a comment")
#' }
#' @export
box_comment_create <- function(file_id = NULL, message, comment_id = NULL) {
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
        "message" = message
      ),
      auto_unbox = TRUE
    ),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  httr::stop_for_status(req)
  
  message("Comment left on ", item$type, " ", item$id, ".")
  
  x <- httr::content(req)
  
  # class it up
  class(x) <- c("boxr_comment", class(x))
  
  invisible(x)
}
#' @rdname box_comment_create
#' @return A `list` object of class [`boxr_comment_get`][boxr_S3_classes] with the API's
#' response values. . Use `as.data.frame()` to convert to a frame. 
#' @examples 
#' \dontrun{
#' x <- box_comment_get(file_id)
#' x
#' as.data.frame(x)
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
  
  x <- httr::content(req)
  
  # class it up
  class(x) <- c("boxr_comment_list", class(x))
  
  invisible(x)
}
