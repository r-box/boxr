

# Prelim pacakge code -----------------------------------------------------

box_dir_collaborators <- function(dir_id) {
  url <- glue::glue("https://api.box.com/2.0/folders/{dir_id}/collaborations")
  
  resp <-httr::content(httr::GET(url, get_token()))
  
  message(glue::glue("box_dir {dir_id} has {resp$total_count} collaborator(s)."))
  
  resp[["entries"]]
}

box_file_collaborators <- function(file_id) {
  url <- glue::glue("https://api.box.com/2.0/files/{file_id}/collaborations")
  
  resp <-httr::content(httr::GET(url, get_token()))
  
  message(glue::glue("box_dir {file_id} has {resp$total_count} collaborator(s)."))
  
  invisible(resp)
}

box_delete_collaboration <- function(collaboration_id) {
  url <- glue::glue("https://api.box.com/2.0/collaborations/{collaboration_id}")
  
  resp <- httr::DELETE(url, get_token())
  
  httr::stop_for_status(
    resp,
    glue::glue("find collaboration_id: {collaboration_id}"))
  
  message(glue::glue("Box collaboration id: {collaboration_id} deleted."))
} 


# Human Testing Workflow --------------------------------------------------


user <- 9459307839
service <- 11368714415

# O-Auth as user
box_auth()

existing_file <- 670079476711
existing_dir <- 111213377084


box_dir_invite(existing_dir, service, role = "co-owner") # errors is already collaborator

box_get_collab_dir(existing_dir)

box_(existing_file, service, role = "editor")

r <- box_dir_collaborators(existing_dir)

i <- box_get_collab_dir(existing_dir)

box_delete_collaboration(r['id'])

box_dir_collaborators(existing_dir)

box_file_collaborators(existing_file)

box_create_collab_file(existing_file, service)

r <- box_get_collab_file(existing_file)


# JWT-Auth as service

box_auth_service()

existing_service_dir <- 111224647004

box_dir_collaborators(existing_service_dir)

box_dir_invite(existing_service_dir, user)

r <- box_dir_collaborators(existing_service_dir)

box_delete_collaboration(r['id'])

box_dir_collaborators(existing_service_dir)

