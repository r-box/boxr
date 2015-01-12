# This code swiped from the function 'humanReadable' in the gdata pakcage
#
# Reference
#
# Gregory R. Warnes, Ben Bolker, Gregor Gorjanc, Gabor Grothendieck, Ales
# Korosec, Thomas Lumley, Don MacQueen, Arni Magnusson, Jim Rogers and others
# (2014). gdata: Various R programming tools for data manipulation. R package
# version 2.13.3. http://CRAN.R-project.org/package=gdata
#
# Package: gdata
# Title: Various R programming tools for data manipulation
# Description: Various R programming tools for data manipulation
# Depends: R (>= 2.13.0)
# SystemRequirements: perl
# Imports: gtools
# Version: 2.13.3
# Date: 2014-04-04
# Author: Gregory R. Warnes, Ben Bolker, Gregor Gorjanc, Gabor
# Grothendieck, Ales Korosec, Thomas Lumley, Don MacQueen, Arni
# Magnusson, Jim Rogers, and others
# Maintainer: Gregory R. Warnes <greg@warnes.net>
#   License: GPL-2
# NeedsCompilation: no
# Suggests: RUnit
# Packaged: 2014-04-05 21:08:58 UTC; warnes
# Repository: CRAN
# Date/Publication: 2014-04-06 08:00:48
# Built: R 3.0.3; ; 2014-05-28 18:41:21 UTC; windows
format_bytes <- 
  function (x, standard = "SI", digits = 1, width = 3, sep = " ") 
  {
    if (any(x < 0)) 
      stop("'x' must be positive")
    if (standard == "SI") {
      suffix <- c("B", "kB", "MB", "GB", "TB", "PB", "EB", 
                  "ZB", "YB")
      base <- 1000
    }
    else {
      suffix <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", 
                  "ZiB", "YiB")
      base <- 1024
    }
    .applyHuman <- function(x, base, suffix, digits, width, sep) {
      n <- length(suffix)
      for (i in 1:n) {
        if (x >= base) {
          if (i < n) 
            x <- x/base
        }
        else {
          break
        }
      }
      if (is.null(width)) {
        x <- format(round(x = x, digits = digits), nsmall = digits)
      }
      else {
        lenX <- nchar(x)
        if (lenX > width) {
          digitsMy <- width - (lenX - (lenX - (nchar(round(x)) + 
                                                 1)))
          digits <- ifelse(digitsMy > digits, digits, digitsMy)
        }
        if (i == 1) 
          digits <- 0
        x <- round(x, digits = digits)
      }
      paste(x, suffix[i], sep = sep)
    }
    sapply(X = x, FUN = ".applyHuman", base = base, suffix = suffix, 
           digits = digits, width = width, sep = sep)
  }
