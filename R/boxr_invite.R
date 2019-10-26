#' Invite a collaboration on a Box folder
#'
#' @description 
#' Although this function can be used in all sorts of situations, it can be 
#' particularly useful in setting up a workflow with a service-account:
#' 
#' - If you are authenticated as a user, using [box_auth()], you can invite
#' the service account to collaborate on a folder in your *user* filespace.
#' In this case, the shared folder will appear in the service-account filespace.
#' 
#' - If you are authenticated as as the service-account using 
#' [box_auth_service()], you can invite your *user-account* to collaborate. 
#' In this case, the shared folder will appear in your user filespace.
#' 
#' Once you issue an invitation to create collaboration, you cannot change it,
#' e.g. you cannot change the `role` from `"viewer"` to `"co-owner"`. 
#' However, you can delete the collaboration, then issue a *new* invitation.
#' To delete a collaboration, you can use the Box web-portal.
#' 
#' @details
#' Regardless of the scenario, to use this function, you need the `dir_id` of
#' folder you want to share and the `account_id` of the account with which you 
#' want to share it.
#' 
#' While authenticated from the host account, the one that will issue the 
#' invitation, you can use `box_ls()` and `box_setwd()` to get the `dir_id`
#' for the folder you want to share. If the host-account is the user-account, 
#' you can also use the web-portal to find the `dir_id`. If the host account 
#' is the service-account, you can use the Box 
#' [content-portal](https://app.box.com/master/content) to find the `dir_id`.
#' 
#' A user can find their `account_id` using the Box web-portal. As well, when 
#' you authenticate using boxr, the `account_id` is included in the login 
#' message. Thus, you can use `box_auth_service()` to find out the `account_id`
#' for a given service-account.
#' 
#' Please note that the default `role`, i.e. permission level, for an invitation
#' is `"viewer"`. Legal values for `role` are `"editor"`, `"viewer"`, 
#' `"previewer"`, `"uploader"`, `"previewer uploader"`, `"viewer uploader"`, 
#' `"co-owner"`, `"owner"`.
#' 
#' @inheritParams box_setwd
#' @param account_id `character` ID for Box account to invite, e-mail address (login) 
#' will also  work.
#' @param login `character` email address of account to invite, can be used instead of 
#'   `account_id`.
#' @param role `character` role of the collaborator; default is `"viewer"`.
#' @param can_view_path `logical` indicates to allow the collaborator to navigate 
#'   parent-folders at Box.
#' 
#' @seealso [box_user_id()], [box_auth()], [box_auth_service()]
#' 
#' @return Invisible `list()` containing collaboration-information.
#' @export
#' 
box_dir_invite <- function(dir_id, account_id, login = NULL,
                           role = "viewer", can_view_path = FALSE) {

  # if login is provided, ignore account_id
  if (!is_void(login)) {
    account_id <- NULL
  }
  
  item <-
    list(
      type = "folder", # imagine box_file_invite()
      id = as.character(dir_id)
    )
  
  accessible_by <-
    list(
      type = "user", #  imagine inviting a group
      id = as.character(account_id),
      login = login
    )
  
  box_invite(item, accessible_by, role, can_view_path)
} 


box_invite <- function(item, accessible_by, role, can_view_path = FALSE) {

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
  resp <- httr::POST(
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
      )
  ) 
  
  httr::stop_for_status(resp, task = "invite collaborator")

  # TODO: create an S3 class
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