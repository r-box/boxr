# borrowed: https://github.com/vegawidget/vegawidget/blob/master/R/utils-package.R
# also: https://github.com/hadley/assertthat/pull/56
#
# used to help with suggests
#
assert_packages <- function(packages, ...) {
  
  packages <- c(packages, ...)
  
  is_missing <-
    vapply(packages, function(x) {!requireNamespace(x, quietly = TRUE)}, TRUE)
  
  missing_pkgs <- packages[is_missing]
  
  quote_missing_pkgs <-
    vapply(missing_pkgs, function(x) {paste0('"', x, '"')}, "")
  
  assertthat::assert_that(
    identical(length(missing_pkgs), 0L),
    msg = paste(
      "Package(s):",
      paste(quote_missing_pkgs, collapse = ", "),
      "needed for this function to work. Please install.",
      sep = " "
    )
  )
  
}