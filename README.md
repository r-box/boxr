# boxr
A lightweight, high-level `R` interface to the box.com API.


## TODO:
* Flesh out the s3 print method. Move the verbose stuff to summary.
* Get an overwrite option set (with userdefaults). Report settings on pakcage
startup
* Write some proper tests with testthat
* Send to Ranjit, see if he can break it for you

### Publication
* Write a little usage guide
* write some examples
* Fill out the see also sections
* Stick it on github
* Set up travis
* Stick it on cran

### Lofty ideal
* Write something like file.choose which uses the view api to allow users to 
select files using a browser

### Some notes on launching a browser from R
listen <- function(env) {
  if (!identical(env$PATH_INFO, "/")) {
    return(list(
      status = 404L,
      headers = list("Content-Type" = "text/plain"),
      body = "Not found")
    )
  }
  
  query <- env$QUERY_STRING
  if (!is.character(query) || identical(query, "")) {
    info <<- NA
  } else {
    info <<- parse_query(gsub("^\\?", "", query))
  }
  
  list(
    status = 200L,
    headers = list("Content-Type" = "text/plain"),
    body = "Authentication complete. Please close this page and return to R."
  )
}


https://github.com/hadley/httr/blob/397fd0876d12f0b584027c58b350923cc09461af/R/oauth-listener.r

server <- httpuv::startServer("127.0.0.1", 1410, list(call = listen))