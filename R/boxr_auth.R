#' Authenticate box.com account
#' 
#' You'll need to set up an 'app' at https://www.box.com/developers/services
#' 
#' 
#' @param client_id The client id for the account you'd like to use. 
#' \code{character}.
#' @param client_secret The client secret for the account you'd like to use. 
#' \code{character}.
#' @param interactive \code{logical}. Should the authorization process happen 
#' interactively (requiring user input to the R console, and/or a visit to 
#' box.com)?
#' @param use_oob Passed to \code{oob} in \code{\link{httr}}.
#' @param as_header Passed to \code{as_header} in \code{\link{httr}}.
#' @param cache Passed to \code{cache} in \code{\link{httr}}.
#' @param write.Renv \code{logical}. If they were missing, and an OAuth2.0 token
#' was obtained, should \code{client_id} and \code{client_secret} be written to 
#' \code{.Renvirons} in  your \code{HOME} directory? (Note: The \code{HOME} dir
#' is not neccesarily that returned by \code{geetwd()}.)
#' @return Invoked for it's side effect; OAuth2.0 connection to the box.com 
#' API.
#' @export
box_auth <- function(client_id = "", client_secret = "", interactive = TRUE,
                     use_oob = getOption("httr_oob_default"), as_header = TRUE,
                     cache = "~/.boxr-oath", write.Renv = TRUE){
  # If the user hasn't input any, look to .Renviron for the
  # id and secret
  if(client_id == "")
    if(Sys.getenv("BOX_CLIENT_ID") != ""){
      message("Reading client id from .Renviron")
      client_id <- Sys.getenv("BOX_CLIENT_ID")
    }
  
  if(client_secret == "")
    if(Sys.getenv("BOX_CLIENT_SECRET") != ""){
      message("Reading client secret from .Renviron")
      client_secret <- Sys.getenv("BOX_CLIENT_SECRET")
    }
  
  # UI for interactively entering ids and secrets
  if(client_id == "" & interactive){
    message("Please enter your box client id. If you don't have one")
    message("see the documentation at ?box_auth, and hit ENTER to exit.")
    client_id <- readline()
    
    # Tidy up any invalid characters
    client_id <- gsub("[[:space:]]|[[:punct:]]", "", client_id)
    if(nchar(client_id) == 0L) return()
  }
  
  if(client_secret == "" & interactive){
    message("Please enter your box client secret")
    message("(Hit ENTER to exit.)")
    client_secret <- readline()
    
    # Tidy up any invalid characters
    client_secret <- gsub("[[:space:]]|[[:punct:]]", "", client_secret)
    if(nchar(client_secret) == 0L) return()
  }
  
  # At this point, a non-interactive call may still have no id & secret
  if(client_id == "" | client_secret == "")
    stop(paste0(
      "box.com authorization unsuccessful; client id and/or secret not found.\n",
      "See ?box_auth for help!"
    ))
  
  box_app <- 
    httr::oauth_app(
      appname = "box",
      key     = client_id,
      secret  = client_secret
    )
  
  box_endpoint <- 
    httr::oauth_endpoint(
      authorize = "authorize",
      access    = "token",
      base_url  = "https://app.box.com/api/oauth2/"
    )
  
  box_token <- 
    httr::oauth2.0_token(
      box_endpoint,
      box_app, 
      use_oob   = use_oob,
      as_header = as_header,
      cache     = cache
    )
  
  if(!exists("box_token"))
    stop("Login at box.com failed; unable to connect to API.")
  
  # Test the connection; retrieve the username
  test_req <- 
    httr::GET(
      "https://api.box.com/2.0/folders/0", httr::config(token = box_token)
    )
  
  if(httr::http_status(test_req)$cat != "success")
    stop("Login at box.com failed; unable to connect to API.")
  
  cr <- httr::content(test_req)
  
  # Write the details to the Sys.env
  app_details <- 
    setNames(
      list(client_id, client_secret), c("BOX_CLIENT_ID", "BOX_CLIENT_SECRET")
    )
  
  do.call(Sys.setenv, app_details)
  
  # Write to options
  options(
    boxr.token = box_token,
    boxr.token.cache = cache,
    boxr.username = cr$owned_by$login,
    box_wd = "0"
  )
  
  # Let the user know they're logged in
  message(
    paste0(
      "boxr: Authenticated at box.com as ",
      cr$owned_by$name, " (",
      cr$owned_by$login, ")"
    )
  )
  
  # Write the details to .Renviron
  if(write.Renv){
    
    # Path to the R environment variables file, if it exists
    env_path <- 
      normalizePath(paste0(Sys.getenv("HOME"), "/.Renviron"), mustWork = FALSE)
    
    if(file.exists(env_path)) {
      re <- readLines(env_path)
    } else {
      re <- NULL
    }
    
    # Remove any where they details were previously set, and write the new ones
    # to the end of the file
    writeLines(
      c(
        re[!grepl("BOX_CLIENT_ID=|BOX_CLIENT_SECRET=", re)],
        paste0('BOX_CLIENT_ID="',     client_id, '"'),
        paste0('BOX_CLIENT_SECRET="', client_secret, '"')
      ),
      con = env_path
    )
#     message("Writing client_id and client_secret to")
#     message(env_path)
  }
  return(TRUE)
}

#' Authenticate box.com account automatically
#' 
#' This function saves you the effort of typing \code{\link{box_auth}()} after 
#' the package loads. Executing \code{box_auth_on_attach(TRUE)} will mean that 
#' \code{boxr} will automatically attempt to authorize itself when 
#' 'attached' (e.g. \code{library(boxr)}), using the credentials from the 
#' current session.
#' 
#' @note This is provided for convenience, but it's a bad idea to use, if:
#' \describe{
#'   \item{\strong{You'd like your code to be reporoducible}}{Even if your 
#'   collaborators have access to the same files on box.com, as the default 
#'   behaviour is to require using \code{\link{box_auth}()}, code is likely to 
#'   become irreproducible.}
#'   \item{\strong{You use more than one box.com account}}{Things could get 
#'   rather confusing.}
#' }
#'   
#' @param auth_on_attach Should boxr try and connect to your account when 
#' attached? \code{logical}
#' @return Nothing; invoked for it's side effect.
#' @export
box_auth_on_attach <- function(auth_on_attach = FALSE){
  assertthat::assert_that(!is.na(auth_on_attach))
  assertthat::assert_that(is.logical(auth_on_attach))
  
  checkAuth()
  
  # Path to the R environment variables file, if it exists
  env_path <- 
    normalizePath(paste0(Sys.getenv("HOME"), "/.Renviron"), mustWork = FALSE)
  
  if(file.exists(env_path)) {
    re <- readLines(env_path)
  } else {
    re <- NULL
  }
  
  # Remove any where they details were previously set, and write the new ones
  # to the end of the file
  writeLines(
    c(
      re[!grepl("BOX_AUTH_ON_ATTACH|BOX_TOKEN_CACHE", re)],
      paste0('BOX_AUTH_ON_ATTACH="', auth_on_attach, '"'),
      paste0('BOX_TOKEN_CACHE="', getOption("boxr.token.cache"), '"')
    ),
    con = env_path
  )
  
  user <- getOption("boxr.username")
  
  if(auth_on_attach){
    message(
      "boxr will now attempt to authorize you as\n    ", user, "\nwhen",
      "'attached', e.g.    \nlibrary(boxr)\n"
    )
  } else {
    message(
      "boxr will *NOT* attempt to authorize you when",
      "'attached', e.g.    \nlibrary(boxr)\n"
    )
  }
}
