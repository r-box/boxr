# A simple wrapper for box_auth, with defaul options suitable for running 
# at startup
boxAuthOnAttach <- function(){
  if(Sys.getenv("BOX_AUTH_ON_ATTACH") == "TRUE")
    try(
      box_auth(
        cache = Sys.getenv("BOX_TOKEN_CACHE"),
        interactive = FALSE, 
        write.Renv = FALSE
      ),
      silent = TRUE
    )
}

# A version of cat which only works if the package options are set to verbose,
# and pads out the message with spaces so that it fills/wipes the console.
# It also appends \r to the start of each message, so that you can stick them in
# a loop, for example
catif <- function(...){
  if(getOption("boxr.verbose")){
    txt <- paste(..., collapse = " ")
    width <- max(getOption("width"), nchar(txt))
    
    cat(
      paste0(
        "\r", txt, 
        paste(rep(" ", max(0, width - nchar(txt) - 1)), collapse = "")
      )
    )
  }
}

# A function to convert the datetime strings that the box api uses, to something
# R can understand
box_datetime <- function(x){
  # R has trouble figuring out the time format
  # Split out the date/time part
  dt <- substr(x, 1, nchar(x) - 6)
  # and the timezone offset
  tz <- substr(x, nchar(x) - 5, nchar(x))
  
  tz <- gsub(":", "", tz)
  
  # Note, the timzeone of the datetime boject will be the system default,
  # bit it's value will have been adjusted to account for the timzone of x
  as.POSIXct(paste0(dt, tz), format = "%Y-%m-%dT%H:%M:%S%z")
}


checkAuth <- function(){
  if(is.null(getOption("boxr.token")))
    stop("It doesn't look like you've set up authentication for boxr yet.\n",
         "See ?box_auth")
}

# Something for keeping dir strings a constant length for calls to cat
trimDir <- function(x, limit = 25){
  n <- nchar(x)
  if(n > limit)
    return(paste0("...", substr(x, n - limit + 3, n)))
  
  if(n < limit)
    return(paste0(paste(rep(" ", limit - n), collapse = ""), x)) else x
}
