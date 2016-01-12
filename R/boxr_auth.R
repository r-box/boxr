#' Authenticate a box.com Account
#'
#' @description
#' \code{box_auth} serves two purposes:
#'   \enumerate{
#'     \item Setting up box.com accounts with \code{boxr} for the first time
#'     \item Connecting to previously used box.com accounts
#'   }
#'
#' In either case, it should be sufficient to run \code{box_auth()} with no
#' parameters. If you've authenticated with boxr before, this should be all
#' that is required. However, the first time you use boxr, the process is
#' slightly more involved (see 'Getting Set-Up' below).
#'
#' @section Getting Set-Up:
#' \describe{
#'
#'   A version of this guide is in the pakcage vignette, with some additional
#'   screenshots. To view the vignette, run \code{vignette("boxr")}.
#'
#'   To use boxr for the first time, you need to enable API access for your
#'   box.com account. The process is slightly annoying. You only need to do it
#'   once - it takes around 2 minutes.
#'
#'   The next time you use boxr, you should be able to just run
#'   \code{box_auth()} (without entering anything else) to be authenticated and
#'   ready-to-go.
#'
#'   \item{\bold{1. 'Create an app'}}{
#'     At \url{https://www.box.com/developers}, click on 'My Apps', in the top
#'     right hand corner log in and create a new 'app' for your box.com account.
#'     This won't create an app of any description; you'll simply be granting
#'     yourself programmatic access to your files. You can call it anything you
#'     like.
#'   }
#'   \item{\bold{2. Set OAuth2 Parameters}}{
#'     On the next screen, you'll want to check the box for \bold{Content API
#'     Access Only}, and enter \code{http://localhost} as your
#'     \bold{redirect_uri}.
#'   }
#'   \item{\bold{3. Connect boxr to your account}}{
#'     Run \code{box_auth()} and pass your \code{client_id} and
#'     \code{client_secret} to the console when prompted. These strings are
#'     not' enough for someone to access your account maliciously. However,
#'     it's still a good idea to keep them safe, and out of any files or code
#'     which might be shared with others.
#'   }
#' }
#'
#' A browser window should open, for you to formally grant yourself access to
#' your files at box.com.
#'
#' From this point on, simply running \code{box_auth()} at the start of a
#' session should be all that is required.
#'
#' @param client_id Optional. The client id for the account you'd like to use.
#'   \code{character}.
#' @param client_secret Optional. The client secret for the account you'd like
#'   to use. \code{character}.
#' @param interactive \code{logical}. Should the authorization process happen
#'   interactively (requiring user input to the R console, and/or a visit to
#'   box.com)?
#' @param cache Passed to \code{cache} in \code{\link{httr}}.
#' @param write.Renv \code{logical}. If authentication was successful, should
#'   \code{client_id} and \code{client_secret} be written to \code{.Renvirons}
#'   in your \code{HOME} directory?
#' @param ... Passed to \code{\link{oauth2.0_token}}
#'
#' @return Returns \code{TRUE} if connection successful, throws an error
#'   otherwise. Invoked for it's side effect; OAuth2.0 connection to the
#'   box.com API.
#'
#' Your \code{client_id} and \code{client_secret} will be written to
#' \code{~/.Renviron} for future use, and a token object will be wrttien to
#' the path supplied bu \code{cache} (\code{~/.boxr-oauth} by default).
#'
#' See \code{\link{oauth2.0_token}} for details.
#'
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#'
#' @seealso \code{\link{box_auth_on_attach}}, \code{\link{oauth2.0_token}} for
#'   details of how the tokens are handled
#'
#' @export
box_auth <- function(client_id = "", client_secret = "", interactive = TRUE,
                     cache = "~/.boxr-oauth", write.Renv = TRUE, ...) {

  # If the user hasn't input any, look to .Renviron for the
  # id and secret
  if (client_id == "")
    if (Sys.getenv("BOX_CLIENT_ID") != "") {
      message("Reading client id from .Renviron")
      client_id <- Sys.getenv("BOX_CLIENT_ID")
    }

  if (client_secret == "")
    if (Sys.getenv("BOX_CLIENT_SECRET") != "") {
      message("Reading client secret from .Renviron")
      client_secret <- Sys.getenv("BOX_CLIENT_SECRET")
    }

  # UI for interactively entering ids and secrets
  if (client_id == "" & interactive) {
    message("Please enter your box client id. If you don't have one")
    message("see the documentation at ?box_auth, and hit ENTER to exit.")
    client_id <- readline()

    # Tidy up any invalid characters
    client_id <- gsub("[[:space:]]|[[:punct:]]", "", client_id)
    if (nchar(client_id) == 0L) return()
  }

  if (client_secret == "" & interactive) {
    message("Please enter your box client secret")
    message("(Hit ENTER to exit.)")
    client_secret <- readline()

    # Tidy up any invalid characters
    client_secret <- gsub("[[:space:]]|[[:punct:]]", "", client_secret)
    if (nchar(client_secret) == 0L) return()
  }

  # At this point, a non-interactive call may still have no id & secret
  if (client_id == "" | client_secret == "")
    stop(paste0(
      "box.com authorization unsuccessful; client id and/or secret not found",
      "\nSee ?box_auth for help!"
    ))

  box_app <- httr::oauth_app(
      appname = "box",
      key     = client_id,
      secret  = client_secret
    )

  box_endpoint <- httr::oauth_endpoint(
      authorize = "authorize",
      access    = "token",
      base_url  = "https://app.box.com/api/oauth2/"
    )

  box_token <- httr::oauth2.0_token(
      box_endpoint,
      box_app,
      use_oob   = getOption("httr_oob_default"),
      cache     = cache,
      ...
    )

  if (!exists("box_token"))
    stop("Login at box.com failed; unable to connect to API.")

  # Test the connection; retrieve the username
  test_req <- httr::GET(
    "https://api.box.com/2.0/folders/0", httr::config(token = box_token)
  )

  if (httr::http_status(test_req)$cat != "Success")
    stop("Login at box.com failed; unable to connect to API.")

  cr <- httr::content(test_req)

  # Write the details to the Sys.env
  app_details <-
    stats::setNames(
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
  if (write.Renv) {

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
        re[!grepl("BOX_CLIENT_ID=|BOX_CLIENT_SECRET=", re)],
        paste0('BOX_CLIENT_ID="',     client_id, '"'),
        paste0('BOX_CLIENT_SECRET="', client_secret, '"')
      ),
      con = env_path
    )
#     message("Writing client_id and client_secret to")
#     message(env_path)
  }
  return(invisible(TRUE))
}


#' Reauthorise a Problematic box.com connection
#' 
#' Very simply, deletes the old token file before trying to re-authorise. This 
#' is often the solution to authorisation problems raised by users!
#'
#' @inheritParams box_auth
#' @param ... Passed to \code{\link{box_auth}}
#'
#' 
#' @seealso \code{\link{box_auth}} for the usual method of authorisation, and
#'   \code{\link{box_auth_on_attach}} for a lazy one.
#'   
#' @export
box_fresh_auth <- function(cache = "~/.boxr-oauth", ...) {
  unlink(cache, force = TRUE)
  box_auth(cache = cache, ...)
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
#'
#' @return Nothing; invoked for it's side effect.
#'
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
#'
#' @seealso \code{\link{box_auth}}
#'
#' @export
box_auth_on_attach <- function(auth_on_attach = FALSE) {
  assertthat::assert_that(!is.na(auth_on_attach))
  assertthat::assert_that(is.logical(auth_on_attach))

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
}

