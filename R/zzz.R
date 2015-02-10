.onAttach <- function(libname, pkgname) {
  # box_auth() # This causes an error, because httr::oauth_listener() needs
  # an interactive environment. You're best off writing a specific start-up
  # function
  try(box_auth(interactive = FALSE, write.Renv = FALSE), silent = TRUE)
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.boxr <- list(
    boxr.token       = NULL,
    boxr.wd          = list(id = 0, name = "All Files"),
    boxr.wd.path     = "",
    boxr.verbose     = TRUE,
    boxr.progress    = TRUE,
    boxr.interactive = TRUE
  )
  toset <- !(names(op.boxr) %in% names(op))
  if(any(toset)) options(op.boxr[toset])
  
  invisible()
}