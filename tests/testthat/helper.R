skip_if_no_token <- function() {
  testthat::skip_if_not(boxr_has_token(), "No Drive token")
}