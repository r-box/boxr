#' Create Box collaboration 
#' 
#' @description 
#' Although this function can be used in all sorts of situations, it can be 
#' particularly useful in setting up a workflow with a service-account:
#' 
#' - If you are authenticated as a user, using [box_auth()], you can invite
#' the service account to collaborate on a folder in your *user* filespace.
#' In this case, the shared folder will appear in the service-account 
#' filespace.
#' 
#' - If you are authenticated as the service-account using 
#' [box_auth_service()], you can invite your *user-account* to collaborate. 
#' In this case, the shared folder will appear in your user file-space.
#' 
#' Once you issue an invitation to create a collaboration, you cannot change it,
#' e.g. you cannot change the `role` from `"viewer"` to `"co-owner"`. 
#' However, you can delete the collaboration, then issue a *new* invitation.
#' To delete a collaboration, use [`box_collab_delete()`]. To check a Box folder ID or file ID 
#' for existing collaborations, use [`box_collab_get()`].
#' You can also use the Box web-portal to manage collaborations.
#' 
#' The default `role`, i.e. permission level, for an invitation
#' is `"editor"`. Legal values for `role` are `"editor"`, `"viewer"`, 
#' `"previewer"`, `"uploader"`, `"previewer uploader"`, `"viewer uploader"`, 
#' `"co-owner"`, `"owner"`.
#' 
#' @details
#' To use this function, you must provide exactly one of: `dir_id` or `file_id`,
#' to specify what you want to share, and exactly one of: `user_id`, 
#' `group_id`, or `login` (email address), to specify the account you want to 
#' share it with.
#' 
#' While authenticated from the host account, the one that will issue the 
#' invitation, you can use `box_ls()` and `box_setwd()` to get the `dir_id`
#' or `file_id` for the item you want to share. If the host-account is the 
#' user-account, you can also use the web-portal to find the `dir_id` or 
#' `file-id`. If the host account is the service-account, you can use the Box 
#' [content-portal](https://app.box.com/master/content) to find this.
#' 
#' A user can find their `user_id` using the Box web-portal. As well, when 
#' you authenticate using boxr, the `user_id` is included in the login 
#' message. Thus, you can use `box_auth_service()` to find out the `user_id`
#' for a given service-account.
#' 
#' This returns an object with S3 class [`boxr_collab`][boxr_S3_classes];
#' this is a list containing the response from the API. You can use 
#' `as_tibble()` or `as.data.frame()` on this return-object to convert to 
#' a tibble or data frame.
#' 
#' @seealso [box_auth()], [box_auth_service()]
#' @inheritParams box_dl
#' @inheritParams box_fetch
#' @param user_id `character` ID for Box user-account to invite.
#' @param group_id `character` ID for Box group-account to invite.
#' @param login `character` email address of account to invite, if specified 
#'   will be used instead of `user_id`.
#' @param role `character` role of the collaborator; default is `"viewer"`.
#' @param can_view_path `logical` indicates to allow the collaborator to 
#'   navigate parent-folders at Box.
#' 
#' @return Object with S3 class [`boxr_collab`][boxr_S3_classes].
#' @export
#' 
box_collab_create <- function(dir_id = NULL, user_id = NULL, 
                              file_id = NULL, group_id = NULL, login = NULL,
                              role = "editor", can_view_path = FALSE) {

  # if login is provided, ignore user_id
  if (!is_void(login)) {
    user_id <- NULL
  }

  # detect item details for API call
  item <- collab_item_helper(dir_id, file_id)
  
  accessible_by <- collab_access_helper(user_id, group_id, login)
  
  resp <- box_collab_create_internal(item, accessible_by, role, can_view_path)
  resp <- structure(resp, class = "boxr_collab")
  
  resp
} 

#' Collaboration creation station
#' @noRd
#' @keywords internal
box_collab_create_internal <- function(item, accessible_by, role, can_view_path = FALSE) {

  # ref: https://developer.box.com/reference#collaboration-object
  
  # validate
  item_type_legal <- c("file", "folder")
  
  acc_type_legal <- c("user", "group")
  
  role_legal <- c(
    "editor", 
    "viewer", 
    "previewer", 
    "uploader", 
    "previewer uploader", 
    "viewer uploader",
    "co-owner", 
    "owner"
  )
  
  assertthat::assert_that(
    # item
    is.list(item),
    rlang::is_string(item$type),
    item$type %in% item_type_legal,
    rlang::is_string(item$id),
    # accessible_by
    is.list(accessible_by),
    rlang::is_string(accessible_by$type),
    accessible_by$type %in% acc_type_legal,
    rlang::is_string(accessible_by$id) || rlang::is_string(accessible_by$login),
    # role
    rlang::is_string(role),
    role %in% role_legal,   
    # can_view_path
    rlang::is_bool(can_view_path)
  )
  
  # call the API
  resp <- httr::RETRY(
    "POST",
    "https://api.box.com/2.0/collaborations",
    get_token(),
    encode = "multipart",
    body = 
      jsonlite::toJSON(
        list(
          item = item, 
          accessible_by = accessible_by, 
          role = role, 
          can_view_path = can_view_path
        ),
        auto_unbox = TRUE
      ),
    terminate_on = box_terminal_http_codes()
  ) 
  
  httr::stop_for_status(resp, task = "invite collaborator")

  resp <- httr::content(resp)

  # feedback
  message(
    glue::glue(
      "{resp$created_by$name} ({resp$created_by$login}) has invited",
      "{resp$accessible_by$name} ({resp$accessible_by$login})",
      "to collaborate on {resp$item$type} `{resp$item$name}`",
      "as {resp$role}.",
      .sep = " "
    )
  )
  
  invisible(resp)
}

#' Invite collaboration
#' 
#' `box_dir_invite()` is deprecated in favor of `box_collab_create()`.
#'
#' @inheritParams box_collab_create
#' @return Invisible `list()`.
#'
#' @keywords deprecated
#' \lifecycle{deprecated}
#' @export
#' 
box_dir_invite <- function(dir_id, user_id, login = NULL, role = "viewer", 
                           can_view_path = FALSE) {
  .Deprecated("box_collab_create")
  
  # if login is provided, ignore user_id
  if (!is_void(login)) {
    user_id <- NULL
  }
  
  item <-
    list(
      type = "folder", # imagine box_file_invite()
      id = as.character(dir_id)
    )
  
  accessible_by <-
    list(
      type = "user", #  imagine inviting a group
      id = as.character(user_id),
      login = login
    )
  
  box_collab_create_internal(item, accessible_by, role, can_view_path)
} 

#' Get Box collaborations
#' 
#' Retrieve information on all collaborations on a file or folder.
#' 
#' You must specify exactly one of `dir_id` or `file_id`.
#' 
#' This returns an object with S3 class [`boxr_collab_list`][boxr_S3_classes];
#' this is a list containing the response from the API. You can use 
#' `as_tibble()` or `as.data.frame()` on this return-object to convert to 
#' a tibble or data frame.
#' 
#' 
#' @inheritParams box_dl
#' @inheritParams box_fetch
#' 
#' @return Object with S3 class 
#'   [`boxr_collab_list`][boxr_S3_classes].
#' 
#' @export
#' 
box_collab_get <- function(dir_id = NULL, file_id = NULL) {
  
  # detect item type for API call
  item_id <- dir_id %|0|% file_id
  
  if (is.null(item_id)) {
    stop("You must specify dir_id or file_id.")
  }
  
  item_type <- ifelse(!is.null(dir_id), "folder", "file")
  
  url <- glue::glue(
    "https://api.box.com/2.0/{item_type}s/{item_id}/collaborations"
  )
  
  response <- httr::RETRY(
    "GET",
    url = url,
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
    
  content <- httr::content(response)
  
  if (identical(content$type, "error")) {
    stop(
      glue::glue(
        "Error getting collaborators for {item_type} {item_id}: {content$message}"
      )
    )
  }  
  
  # result should be only the entries member
  #   - the rest of the content has only pagination information.
  #   - this is what we did for box_ls().
  #   - this approach it would permit pagination.
  result <- content$entries
  result <- structure(result, class = "boxr_collab_list")

  message(
    glue::glue(
      "Box {item_type} {item_id} has {length(result)} collaborator(s)."
    )
  )
  
  invisible(result)
}

#' Delete Box collaboration
#' 
#' @param collab_id `character` ID for Box collaboration
#' 
#' @return Invisible `NULL`.
#'
#' @export
#'
box_collab_delete <- function(collab_id) {
  
  url <- glue::glue("https://api.box.com/2.0/collaborations/{collab_id}")
  
  resp <- httr::RETRY(
    "DELETE",
    url,
    get_token(),
    terminate_on = box_terminal_http_codes()
  )
  
  httr::stop_for_status(
    resp,
    glue::glue("deleting Box collaboration_id: {collab_id}")
  )
  
  message(glue::glue("Box collaboration id: {collab_id} deleted."))
  
  invisible(NULL)
}
