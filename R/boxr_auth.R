#' Obtain a Box token
#'
#' @description
#' There are two common contexts for using `box_auth()`:
#' 
#' 1. Connecting to [box.com](https://developer.box.com/docs) 
#'    accounts from **boxr** for the first time.
#' 2. Connecting to previously-connected 
#'    [box.com](https://developer.box.com/docs) accounts.
#'
#' In the first case, you will need to provide `box_auth()` with 
#' `client_id` and `client_secret`.
#' 
#' In the second case, you can call `box_auth()` with no arguments 
#' if these variables are stored in your R environment.
#' 
#' To run this function the first time, you will need access to the `client_id` 
#' and `client_secret` of a Box interactive-app. If you are using a work account,
#' this information might be provided to you by your Box-admin team. If you are 
#' using a personal account, you will have to set up a Box interactive-app.
#' 
#' For both cases, these procedures are detailed in 
#' `vignette("boxr-app-interactive")`.
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
#'   behaviour is to write this file to `~/.boxr-oauth`.
#'
#' - some global [options()] are set for your session to manage the token.
#' 
#' - environment variables `BOX_CLIENT_ID` and `BOX_CLIENT_SECRET` are set.
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
#' @param write.Renv **deprecated**.
#' @param ... Other arguments passed to [httr::oauth2.0_token()].
#'
#' @return `invisible(NULL)`, called for side-effects.
#'
#' See [httr::oauth2.0_token()] for details.
#'
#' @seealso [httr::oauth2.0_token()] for details on how tokens are handled
#'
#' @export
#' 
box_auth <- function(client_id = NULL, client_secret = NULL, 
                     interactive = TRUE, cache = "~/.boxr-oauth", 
                     write.Renv, ...) {

  # deprecate write.Renv
  if (!missing(write.Renv)) {
    warning(
      "argument `write.Renv` is deprecated; ",
      "information provided instead at console."
    )
  }
  
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

  box_token <- 
    httr::oauth2.0_token(
      box_endpoint,
      box_app,
      use_oob   = getOption("httr_oob_default"),
      cache     = cache,
      ...
    )

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
  
  # Write the details to the Sys.env
  app_details <-
    stats::setNames(
      list(client_id, client_secret), 
      c("BOX_CLIENT_ID", "BOX_CLIENT_SECRET")
    )

  do.call(Sys.setenv, app_details)

  # if the authenitcation is new, and this is an interactive session,
  # provide feedback on the .Renviron file
  is_new_client <- 
    !identical(c(client_id, client_secret), c(client_id_env, client_secret_env))

  if (is_new_client && interactive()) {
    auth_message(
      glue::glue("BOX_CLIENT_ID={client_id}\nBOX_CLIENT_SECRET={client_secret}")
    )
  }
  
  invisible(NULL)
}


#' Obtain a fresh Box token
#' 
#' Deletes the cached token-file before trying to re-authorise. This 
#' is often the solution to authorisation problems.
#'
#' @inheritParams box_auth
#' @param ... Other arguments passed to [box_auth()].
#' 
#' @seealso [box_auth()] for the usual method of authorisation
#'   
#' @export
#' 
box_fresh_auth <- function(cache = "~/.boxr-oauth", ...) {
  
  assertthat::assert_that(
    is.character(cache),
    fs::file_exists(cache),
    msg = "`cache` must be a valid filename"
  )
  
  unlink(cache, force = TRUE)
  box_auth(cache = cache, ...)
}


#' Obtain a Box token automatically
#'
#' **This function is deprecated, and will be removed at the next relase.** 
#'  
#' This function saves you the effort of typing [box_auth()] after
#' the package loads. Executing `box_auth_on_attach(TRUE)` will mean that
#' `boxr` will automatically attempt to authorize itself when
#' 'attached' (e.g. `library(boxr)`), using the credentials from the
#' current session.
#' 
#' @note This is provided for convenience, but it's a bad idea to use, if:
#'   * You'd like your code to be reporoducible. Even if your
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
#' @return `invisible(NULL)`, called for side-effects.
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

#' Authenicate a box.com account with a JWT (JSON Web Token)
#' 
#' Alternative option for accessing the Box API. Useful on servers.
#' @param user_id `character`, the user ID for the account to use. 
#' @param config_file `character` path to JSON config file.
#'
#' @export
#' 
box_auth_jwt <- function(user_id = NULL, config_file = NULL) {
  
  assert_packages("jsonlite", "openssl", "jose")
  
  user_id_env <- Sys.getenv("BOX_USER_ID")
  config_file_env <- Sys.getenv("BOX_CONFIG_FILE")
  
  # if no input, look to .Renviron `user_id` and `config_file`
  if (is_void(user_id) && !is_void(user_id_env)) {
    message("Using `BOX_USER_ID` from environment")
    user_id <- user_id_env
  }
  
  if (is_void(config_file) && !is_void(config_file_env)) {
    message("Using `BOX_CONFIG_FILE` from environment")
    config_file <- config_file_env 
  }
  
  if (is_void(user_id) || is_void(config_file)) {
    stop(
      "box.com authorization not possible; ",
      "user_id and/or config_file not found\n",
      "See ?box_auth_jwt for help."
    )
  }
  
  config <- jsonlite::fromJSON(config_file)
  
  # de-crypt the key
  key <- openssl::read_key(
    config$boxAppSettings$appAuth$privateKey,
    config$boxAppSettings$appAuth$passphrase
  )
  
  # build out a claim/payload as a specific user
  auth_url <- "https://api.box.com/oauth2/token"
  claim <- jose::jwt_claim(
    iss = config$boxAppSettings$clientID,
    sub = as.character(user_id), # maybe don't need this?
    box_sub_type = "user",
    aud = auth_url,
    jti = openssl::base64_encode(openssl::rand_bytes(16)),
    exp = unclass(Sys.time()) + 45
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
  
  req <- httr::POST(auth_url, body = params, encode = "form")
  
  box_token <- httr::content(req)$access_token
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
  
  # if the authentication is new, and this is an interactive session,
  # provide feedback on the .Renviron file
  is_new_jwt <-
    !identical(c(user_id, config_file), c(user_id_env, config_file_env))

  # including fs::file_exists() to prevent printing contents of the file 
  if (is_new_jwt && interactive() && fs::file_exists(config_file)) {
    auth_message(
      glue::glue("BOX_USER_ID={user_id}\nBOX_CONFIG_FILE={config_file}")
    )
  }
  
  invisible(NULL)
}

#' Is a token available?
#' 
#' @description
#' 
#' Helper for TravisCI; modeled after \code{\link[googledrive]{drive_has_token}()}.
#' 
has_jwt_token <- function() {
  inherits(getOption("boxr_token_jwt"), "request")
}

has_oauth_token <- function() {
  inherits(getOption("boxr.token"), "Token2.0")
}

skip_if_no_token <- function() {
  testthat::skip_if_not(has_jwt_token() || has_oauth_token(), "No Box token")
}


# make a test request, indicate success, return content
test_request <- function() {
 
  test_response <- httr::GET("https://api.box.com/2.0/folders/0", get_token())
  
  httr::stop_for_status(test_response, task = "connect to box.com API")
  
  cr <- httr::content(test_response)
  
  name <- cr$owned_by$name
  login <- cr$owned_by$login
  
  if (has_oauth_token()) {
    method <- "OAuth2"
  }
  
  if (has_jwt_token()) {
    method <- "OAuth2 (JWT)"
  }
  
  message(
    glue::glue("boxr: Authenticated using {method} as {name} ({login})")
  )
  
  cr
}

# we are sending the form-of-message for each method
# @param msg_client_info `glue::glue` object
#
auth_message <- function(msg_client_info) {

  # if usethis installed, encourage to edit .Renviron
  if (requireNamespace("usethis", quietly = FALSE)) {
    
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


