# Copied from httr/R/progress.R @14c8d96ccb9fe608501096083f6ea813f29a3a1b
# 
# Fixes bug in httr::progress() which spams user with warnings about harmless
# behaviour.
# 
# Note to self: Remove when fixed in httr!
# 
progress <- function(type = c("down", "up")) {
  type <- match.arg(type)
  
  httr:::request(options = list(
    noprogress = FALSE,
    progressfunction = progress_bar(type)
  ))
}

progress_bar <- function(type) {
  bar <- NULL
  first <- TRUE
  
  show_progress <- function(down, up) {
    if (type == "down") {
      total <- down[[1]]
      now <- down[[2]]
    } else {
      total <- up[[1]]
      now <- up[[2]]
    }
    
    # First progress request on new file
    if (total == 0 && now == 0) {
      bar <<- NULL
      first <<- TRUE
      return(TRUE)
    }
    
    if (total == 0) {
      if (first) {
        first <<- FALSE
      }
      cat("\rDownloading: ", bytes(now, digits = 2), "     ", sep = "")
      if (now == total) cat("\n")
      utils::flush.console()
    } else {
      if (is.null(bar)) {
        bar <<- utils::txtProgressBar(max = total, style = 3)
      }
      utils::setTxtProgressBar(bar, now)
    }
    
    TRUE
  }
  
  show_progress
}


bytes <- function(x, digits = 3, ...) {
  power <- min(floor(log(abs(x), 1000)), 4)
  if (power < 1) {
    unit <- "B"
  } else {
    unit <- c("kB", "MB", "GB", "TB")[[power]]
    x <- x / (1000 ^ power)
  }
  
  formatted <- format(signif(x, digits = digits), big.mark = ",",
                      scientific = FALSE)
  
  paste0(formatted, " ", unit)
}