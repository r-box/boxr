.onAttach <- function(libname, pkgname) {
  boxAuthOnAttach()
  boxrStartupMessage()
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
  if (any(toset)) options(op.boxr[toset])
  
  invisible()
}