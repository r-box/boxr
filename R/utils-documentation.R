# Anywhere you want to insert this string in the roxygen documentation,
# you can just write something like:
#
# @return `r string_side_effects()`
#
string_side_effects <- function() {
  "Invisible `NULL`, called for side effects."
}