#' Authenticate to Box (interactive-app)
#'
#' @description
#' There are two common use-cases for `box_auth()`:
#' 
#' 1. Connecting to [box.com](https://developer.box.com/docs) 
#'    accounts from **boxr** for the first time.
#' 2. Connecting to previously-connected 
#'    [box.com](https://developer.box.com/docs) accounts.
#'
#' In the first case, you will need to provide `box_auth()` with 
#' `client_id` and `client_secret`.
#' 
#' In the second case, you can call `box_auth()` with no arguments; 
#' the function will look for these in your R environment.
#' 
#' To run this function the first time, you will need access to the `client_id` 
#' and `client_secret` of a Box interactive-app. If you are using a work account,
#' this information might be provided to you by your Box-admin team. If you are 
#' using a personal account, you will have to set up a Box interactive-app.
#' 
#' For both cases, these procedures are detailed in this boxr 
#' [interactive-app article](https://r-box.github.io/boxr/articles/boxr-app-interactive.html).
#' 
#' @section Side-effects:
#' 
#' This function has some side effects which make subsequent calls to 
#' `box_auth()` easier:
#' 
#' - a browser window may be opened at [box.com](https://developer.box.com/docs), 
#'   for you to authorize to your Box app.
#'  
#' - a token file is written, according to the value of `cache`. The default
#'   behavior is to write this file to `~/.boxr-oauth`. 
#'   For all platforms, `~` resolves to the home directory, i.e. path is
#'   resolved using [fs::path_expand()] rather than [fs::path_expand_r()].
#'
#' - some global [options()] are set for your session to manage the token.
#' 
#' - environment variables `BOX_USER_ID`, `BOX_CLIENT_ID`, 
#'   and `BOX_CLIENT_SECRET` are set.
#' 
#' - if these environment variables have changed, and you have the 
#'   [usethis](https://usethis.r-lib.org) package installed, it will copy 
#'   some text to your clipboard that you can paste into your `.Renviron` file.
#' 
#' - a message is printed to the console.  
#' 
#'
#' @inheritParams httr::oauth2.0_token
#' @param client_id `character`, 
#'   the client id for the account to use.
#' @param client_secret `character`, 
#'   the client secret for the account to use. 
#' @param interactive `logical`, indicates that the authorization process 
#'   will be interactive (requiring user input to the R console, and/or a 
#'   visit to [box.com](https://developer.box.com/docs)).
#' @param developer_token `character`, Optional developer token used for
#'   authentication.
#' @param write.Renv **deprecated**.
#' @param ... Other arguments passed to [httr::oauth2.0_token()].
#'
#' @return `r string_side_effects()`
#'
#' @seealso \describe{
#'   \item{[box_auth_service()]}{for authenticating to service-apps.}
#'   \item{[httr::oauth2.0_token()]}{for details on how tokens are handled.}
#'   \item{[Box Developers: Setup with OAuth 2.0](https://developer.box.com/en/guides/applications/custom-apps/oauth2-setup)}{
#'     documentation for setting up Box (interactive) apps with OAuth 2.0.}
#' }
#' 
#' @export
#' 
box_auth <- function(client_id = NULL, client_secret = NULL, 
                     interactive = TRUE, cache = "~/.boxr-oauth", 
                     write.Renv, developer_token= NULL, ...) {

  # deprecate write.Renv
  if (!missing(write.Renv)) {
    warning(
      "argument `write.Renv` is deprecated; ",
      "information provided instead at console."
    )
  }
  
  # harmonize token-location
  cache <- harmonize_token_location(cache)
  
  # read environment variables
  client_id_env <- Sys.getenv("BOX_CLIENT_ID")
  client_secret_env <- Sys.getenv("BOX_CLIENT_SECRET")
  
  # if no input, look to .Renviron for the id and secret
  if (is_void(client_id) && !is_void(client_id_env)) {
    message("Using `BOX_CLIENT_ID` from environment")
    client_id <- client_id_env
  }

  if (is_void(client_secret) && !is_void(client_secret_env)) {
    message("Using `BOX_CLIENT_SECRET` from environment")
    client_secret <- client_secret_env 
  }

  # UI for interactively entering ids and secrets
  if (is_void(client_id) && interactive()) {
    
    message(
      glue::glue(
        "Please enter your box client id.",
        "If you don't have one, hit ENTER to abort, then",
        "see the documentation at ?box_auth.",           
        .sep = "\n"
      )
    )
    client_id <- readline()
    
    # Tidy up any invalid characters
    client_id <- gsub("[[:space:]]|[[:punct:]]", "", client_id)
    
    if (is_void(client_id)) {
      stop("Aborting")
    }
  }

  if (is.null(client_secret) && interactive()) {
    
    message(
      glue::glue(
        "Please enter your box client secret",
        "(Hit ENTER to abort.)",
        .sep = "\n"
      )
    )
    client_secret <- readline()

    # Tidy up any invalid characters
    client_secret <- gsub("[[:space:]]|[[:punct:]]", "", client_secret)
    
    if (is_void(client_secret)) {
      stop("Aborting")
    }
  }

  # At this point, a non-interactive call may still have no id & secret
  if (is_void(client_id) || is_void(client_secret)) {
    stop(
      "box.com authorization unsuccessful; client id and/or secret not found\n",
      "See ?box_auth for help."
    )
  }

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
      base_url  = "https://app.box.com/api/oauth2"
    )

  if (is.null(developer_token)){
    insistent_token <- purrr::insistently(httr::oauth2.0_token, quiet = FALSE)
  
    box_token <- 
      insistent_token(
        box_endpoint,
        box_app,
        use_oob   = getOption("httr_oob_default"),
        cache     = cache,
        ...
      )
  } else {
    credentials <- list(access_token=developer_token,token_type="bearer")
    box_token <- httr::oauth2.0_token(box_endpoint, box_app, credentials = credentials)
  }

  if (!exists("box_token")) {
    stop("Login at box.com failed; unable to connect to API.")
  }

  # write to options
  options(
    boxr.token = box_token,
    boxr.token.cache = cache,
    boxr_token_jwt = NULL
  )
   
  # Test the connection; retrieve the username
  cr <- test_request()
  
  # using repsonse from test-request, set the username
  options(boxr.username = cr$owned_by$login)
  
  user_id <- cr$owned_by$id
  
  # Write the details to the Sys.env
  app_details <-
    stats::setNames(
      list(client_id, client_secret), 
      c("BOX_CLIENT_ID", "BOX_CLIENT_SECRET")
    )

  do.call(Sys.setenv, app_details)

  # if the authentication is new, and this is an interactive session,
  # provide feedback on the .Renviron file
  is_new_client <- 
    !identical(
      c(client_id, client_secret), 
      c(client_id_env, client_secret_env)
    )

  if (is_new_client && interactive()) {
    auth_message(
      glue::glue(
        "BOX_CLIENT_ID={client_id}",
        "BOX_CLIENT_SECRET={client_secret}",
        .sep = "\n"
      )
    )
  }
  
  invisible(NULL)
}


#' Re-authenticate to Box (interactive-app)
#' 
#' Deletes the cached token-file before trying to re-authenticate. This 
#' is often the solution to authentication problems.
#'
#' @inheritParams box_auth
#' @param ... Other arguments passed to [box_auth()].
#' 
#' @inherit box_auth return
#' 
#' @seealso [box_auth()] for the usual method of authentication.
#'   
#' @export
#' 
box_fresh_auth <- function(cache = "~/.boxr-oauth", ...) {
  
  # harmonize token-location
  cache <- harmonize_token_location(cache)
  
  assertthat::assert_that(
    is.character(cache),
    fs::file_exists(cache),
    msg = "`cache` must be a valid filename"
  )
  
  unlink(cache, force = TRUE)
  box_auth(cache = cache, ...)
}


#' Authenticate to Box (interactive) automatically 
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' **This function is deprecated, and may be removed at the next release.** 
#'  
#' This function saves you the effort of typing [box_auth()] after
#' the package loads. Executing `box_auth_on_attach(TRUE)` will mean that
#' `boxr` will automatically attempt to authorize itself when
#' 'attached' (e.g. `library(boxr)`), using the credentials from the
#' current session.
#' 
#' @note This is provided for convenience, but it's a bad idea to use, if:
#'   * You'd like your code to be reproducible. Even if your
#'   collaborators have access to the same files on box.com, as the default
#'   behaviour is to require using [box_auth()], code is likely to
#'   become irreproducible.
#'   
#'   * You use more than one box.com account. Things could get
#'   rather confusing.
#'
#' @param auth_on_attach `logical`, indicates if boxr should authenticate 
#'   as soon as it's loaded.
#'
#' @inherit box_auth return
#'
#' @seealso [box_auth()]
#'
#' @export
#' 
box_auth_on_attach <- function(auth_on_attach = FALSE) {
  assertthat::assert_that(!is.na(auth_on_attach))
  assertthat::assert_that(is.logical(auth_on_attach))

  .Deprecated(
    msg = "box_auth_on_attach() is deprecated; it will be removed in boxr 3.6.0"
  )
  
  if (!interactive()) {
    stop("box_auth_on_attach() can be called only interactively.")
  }
  
  checkAuth()

  # Path to the R environment variables file, if it exists
  env_path <-
    normalizePath(paste0(Sys.getenv("HOME"), "/.Renviron"), mustWork = FALSE)

  if (file.exists(env_path)) {
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

  if (auth_on_attach) {
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
  
  invisible(NULL)
}

#' Authenticate to Box (service-app)
#' 
#' @description 
#' How you authenticate to Box depends the Box-app through which you
#' connect. A Box service-app can be useful for unattended jobs that need
#' access to only a limited part of Box, e.g. one folder.
#' 
#' Use this function to access Box using a service-app.
#' 
#' To access a service-app, you will need a JSON web-token (JWT),
#' generated by your Box-admin team. If you have a personal Box account, *you*
#' are your Box-admin team. You specify the JWT either as `token_file`, 
#' the path to the JWT file, or as `token_text`, the text of the JWT.
#' 
#' Using JWT-authentication is more convenient than using standard OAuth2
#' authentication, as you do not have to go through the "OAuth Dance". This 
#' convenience brings additional considerations because the JWT file gives 
#' its bearer uninhibited access to anything the Box service-app can access.
#' Accordingly, you are recommended to:
#' 
#' - give the service-account access to as little information as you need it
#' to have, e.g. a single folder.
#' - keep the JWT file secure.
#' 
#' @details 
#' The default behavior of a service-app is to act on behalf of the 
#' service-account associated with the service-app. This is different 
#' from an interactive-app, which acts on behalf of the Box user who 
#' authenticates to it.
#' 
#' To use a service-app on a folder belonging to a Box user, either
#' the Box user has to invite the service-account to collaborate on a
#' folder belonging to the user, or the service-account has to invite the
#' Box user to collaborate on a folder belonging to the service-account.
#' 
#' In either case, you can use `box_collab_create()`.
#' 
#' In mid-2020, there appeared intermittent and unexplained failures of 
#' `box_auth_service()`; the theory is that the clocks at either end
#' of the authentication process can be out-of-sync. The workaround
#' is to watch for this failure, then retry the authentication request
#' with a time-offset. If an offset is used, this function generates a message.
#' 
#' For more details on Box service-apps, including how to create them, and 
#' service-app-based workflows, please read this boxr 
#' [service-app article](https://r-box.github.io/boxr/articles/boxr-app-service.html).
#' 
#' @section Side-effects:
#' This function has some side effects:
#' 
#' - some global [options()] are set for your session to manage the token.
#' 
#' - a message is printed to the console.
#' 
#' @param token_file `character`, path to JSON token-file. If not provided,
#'   the function will look for an environment variable `BOX_TOKEN_FILE`. If 
#'   that is not there, it will try `~/.boxr-auth/token.json`.
#' @param token_text `character`, JSON text. If this is provided, 
#'   `token_file` is ignored.
#'
#' @return `r string_side_effects()`
#' 
#' @seealso \describe{
#'   \item{[box_auth()]}{for authenticating to interactive-apps.}
#'   \item{[box_collab_create()]}{for creating a collaboration with a different account
#'   on a Box file or folder.}
#'   \item{[Box Developers: Setup with JWT](https://developer.box.com/en/guides/applications/custom-apps/jwt-setup)}{
#'     documentation for setting up Box (service) apps with JWT.}
#' }
#' 
#' @export
#' 
box_auth_service <- function(token_file = NULL, token_text = NULL) {
  
  assert_packages("jsonlite", "openssl", "jose")
  
  token_file_env <- Sys.getenv("BOX_TOKEN_FILE")

  if (is.null(token_text)) {

    # %|0|% uses is_void()
    token_file <- token_file %|0|% token_file_env %|0|% "~/.boxr-auth/token.json"

    # no need to harmonize; this never went to the `Documents` folder on Windows
    token_file_path <- fs::path_real(token_file)
    if (!fs::file_exists(token_file_path)) {
      stop(
        "box.com authorization not possible; ",
        glue::glue("token_file `{token_file}`: not found\n"),
        "See ?box_auth_service for help.",
        call. = FALSE
      )
    }
    
    token_text <- 
      glue::glue_collapse(
        readLines(token_file_path, warn = FALSE),
        sep = ""
      )
  }

  config <- jsonlite::fromJSON(token_text) 
  user_id <- config$enterpriseID

  # de-crypt the key
  key <- openssl::read_key(
    config$boxAppSettings$appAuth$privateKey,
    config$boxAppSettings$appAuth$passphrase
  )
  
  # build out a claim/payload as a specific user
  auth_url <- "https://api.box.com/oauth2/token"
  
  # wrap params in function, enable retry with different expiry times
  params_time <- function(time_offset = 0) {

    claim <- jose::jwt_claim(
      iss = config$boxAppSettings$clientID,
      sub = as.character(user_id), # maybe don't need this? (can't hurt)
      box_sub_type = "enterprise", # opinion - too risky to support user auth
      aud = auth_url,
      jti = openssl::base64_encode(openssl::rand_bytes(16)),
      exp = as.numeric(Sys.time()) + time_offset + 30 # set expiry 30s in future
    )
    
    # sign claim with key
    assertion <- jose::jwt_encode_sig(
      claim, 
      key,
      header = list("kid" = config$boxAppSettings$appAuth$publicKeyID)
    )
    
    params <- list(
      "grant_type" = "urn:ietf:params:oauth:grant-type:jwt-bearer",
      "assertion" = assertion,
      "client_id" = config$boxAppSettings$clientID,
      "client_secret" = config$boxAppSettings$clientSecret
    )
    
    params
  }
  
  # try a sequence of time offsets (seconds)
  #   to account for possible differences between
  #   clock on local computer and at Box
  seq_time_offset <- c(0, -15, 15, -30, 30)
  
  for (time_offset in seq_time_offset) {
 
    if (!identical(time_offset, seq_time_offset[1])) {
      message(
        glue::glue(
          "Retrying JWT request: time offset now {time_offset} seconds."
        )
      )
    }
    
    response <- httr::RETRY(
      "POST", 
      auth_url, 
      body = params_time(time_offset), 
      encode = "form",
      terminate_on = box_terminal_http_codes()
    )
    
    # if not bad request, break the loop
    if (!identical(httr::status_code(response), 400L)) {
      break
    }

    message(
      glue::glue(
        "Failed JWT request: time offset was {time_offset} seconds."
      )
    )

  }
  
  box_token <- httr::content(response)$access_token
  box_token_bearer <- httr::add_headers(Authorization = paste("Bearer", box_token))

  # write to options
  options(
    # wipe any token set by box_auth() to prevent
    # auth confusion in POST operations
    boxr.token = NULL, 
    boxr.token.cache = NULL,
    boxr_token_jwt = box_token_bearer
  )
  
  # test request, message
  cr <- test_request()
  
  options(boxr.username = cr$owned_by$login)
  
  invisible(NULL)
}

# Is a token available?
# 
# Helper for TravisCI; modeled after `googledrive::drive_has_token()`.
# 
has_jwt_token <- function() {
  inherits(getOption("boxr_token_jwt"), "request")
}

has_oauth_token <- function() {
  inherits(getOption("boxr.token"), "Token2.0")
}

skip_if_no_token <- function() {
  testthat::skip_if_not(has_jwt_token() || has_oauth_token(), "Box token not available")
}

# make a test request, indicate success, return content
test_request <- function() {
 
  test_response <- 
    httr::RETRY(
      "GET",
      "https://api.box.com/2.0/folders/0",
      get_token(),
      terminate_on = box_terminal_http_codes()
    )
  
  httr::stop_for_status(test_response, task = "connect to box.com API")
  
  cr <- httr::content(test_response)
  
  name <- cr$owned_by$name
  login <- cr$owned_by$login
  id <- cr$owned_by$id
  
  if (has_oauth_token()) {
    method <- "OAuth2"
  }
  
  if (has_jwt_token()) {
    method <- "OAuth2 (JWT)"
  }
  
  message(
    glue::glue("boxr: Authenticated using {method} as {name} ({login}, id: {id})")
  )
  
  cr
}

# we are sending the form-of-message for each method
# @param msg_client_info `glue::glue` object
#
auth_message <- function(msg_client_info) {

  # if usethis installed, encourage to edit .Renviron
  if (requireNamespace("usethis", quietly = TRUE)) {
    
    # usethis message
    usethis::ui_todo(
      "You may wish to add to your {usethis::ui_code('.Renviron')} file:"
    )
    usethis::ui_code_block(msg_client_info)
    usethis::ui_todo(
      c(
        "To edit your {usethis::ui_code('.Renviron')} file:",
        "  - {usethis::ui_code('usethis::edit_r_environ()')}",
        "  - check that {usethis::ui_code('.Renviron')} ends with a newline"
      )
    )
    
  } else {
    
    # standard message
    message(
      glue::glue_collapse(
        c(
          "\nYou may wish to add the following to your `.Renviron` file:",
          "  - check that `.Renviron` ends with a newline" ,
          "",
          msg_client_info,
          ""
        ),
        sep = "\n"
      )
    )
    
  }
  
  invisible(NULL)  
}

get_token <- function() {
  
  # Standard OAuth2
  if (has_oauth_token()) {
    return(httr::config(token = getOption("boxr.token")))
  }
  
  if (has_jwt_token()) {
    return(getOption("boxr_token_jwt"))
  }
  
  stop("No token available", call. = FALSE)
}

#' Harmonize token location
#' 
#' The canonical location for tokens is in the `~` directory. Unfortunately,
#' `~` resolves differently on Windows for R than it does for other languages.
#' This has been a source of friction. The solution implemented by the r-lib
#' team is that `fs::path_expand()` resolves only to the home directory.
#' `fs::path_expand_r()` behaves like `path.expand()`, as `~` resolves to the 
#' `Documents` folder, rather than the home folder.
#' 
#' boxr was built using the `path.expand()` philosophy; we are moving to the 
#' r-lib philosophy.
#' 
#' The purpose of this function is to help manage the transition.
#' 
#' This function will look for the cache file in both locations. If the file is 
#' found in the "wrong" location, it will offer to move it to the "right" 
#' location.
#' 
#' It returns the absolute path to the cache file, if it exists.
#'  
#' @param cache `character` path to cache, unexpanded
#' 
#' @return `character` absolute path to cache
#' 
#' @noRd
#'
harmonize_token_location <- function(cache) {

  path_cache <- fs::path_expand(cache)
  path_r_cache <- fs::path_expand_r(cache)
  
  if (fs::file_exists(path_cache)) {
    # success
    return(path_cache)
  }
  
  if (fs::file_exists(path_r_cache)) {
    
    # file in "bad" location, offer to move it
    if (interactive()) {
      msg <- glue::glue(
        "Token file found at `{path_r_cache}`.",
        "Move token file to new boxr standard location `{path_cache}`? (Y/n): ",
        sep = "\n"
      )
      str_move <- readline(prompt = msg)
      
      # we are moving the file
      if (substr(str_move, 1, 1) %in% c("Y", "y", "")) {
        fs::file_move(path_r_cache, path_cache)
        message(
          glue::glue("Moved token file `{path_r_cache}` to `{path_cache}`.")
        )
        return(path_cache)
      }
      
      # declined, return "bad" location
      return(path_r_cache)
      
    }
    
  }
    
  # neither file exists, return "good" location
  path_cache
}
