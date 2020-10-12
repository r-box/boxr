#' Create/get Box comments
#' 
#' Use these functions to create and get comments for Box files.
#' 
#' When you create a comment using `box_comment_create()`, you have to specify 
#' a `file_id` or a `comment_id`. If you specify a `comment_id`, the comment 
#' will be posted as a reply to that comment.
#' 
#' Use `box_comment_get()` to retrieve comments in bulk. This gets all the 
#' comments associated with a file, thus you can specify only a  `file_id`.
#' 
#' @inheritParams box_dl
#' @param message `character` contents of comment. 
#'   Note: tagging people with the \@user pattern is *not* yet supported.
#' @param comment_id `numeric` or `character`, comment ID at Box.
#'  
#' @return \describe{
#'   \item{`box_comment_create()`}{
#'     Object with S3 class [`boxr_comment`][boxr_S3_classes].}
#'   \item{`box_comment_get()`}{
#'     Object with S3 class [`boxr_comment_list`][boxr_S3_classes].}
#' }
#' 
#' @examples 
#' \dontrun{
#'   file_id <- 12345
#'   
#'   # create comments
#'   x <- box_comment_create(file_id, "Report is ready.")
#'   box_comment_create(comment_id = x$id, message = "Response to a comment")
#'   
#'   # get comments
#'   box_comment_get(file_id)
#' }
#' @export
#' 
box_comment_create <- function(file_id = NULL, message, comment_id = NULL) {
  
  # TODO: consider support tagging users with @name, 
  #   but requires tricky formatting with userID:
  #   https://developer.box.com/reference/post-comments/
  
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
#' @export
#' 
box_comment_get <- function(file_id) {
  
  resp <- httr::RETRY(
    "GET",
    glue::glue("https://api.box.com/2.0/files/{file_id}/comments"),
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  httr::stop_for_status(resp)
  
  content <- httr::content(resp)
  
  result <- content$entries
  
  # class it up
  class(result) <- c("boxr_comment_list", class(result))
  
  invisible(result)
}
