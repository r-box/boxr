#' @rdname box_dl
#' @export
box_read <- function(file_id, type = NULL){
  checkAuth()
  
  req <- 
    httr::GET(
      paste0(
        "https://api.box.com/2.0/files/",
        file_id, "/content"
      ),
      httr::config(token = getOption("boxr.token"))
    )
  
  filename <- 
    gsub(
      'filename=\"|\"', '',
      stringr::str_extract(
        req$headers["content-disposition"][[1]],
        'filename=\"(.*?)\"'
      )
    )
  
  # Currently, httr works well with .csv files, but doesn't to a great job with
  # json.
  probably_json <- grepl("\\.json$", filename)
  
  if(is.null(type) & probably_json){
    cont <- jsonlite::fromJSON(httr::content(req, as = "text"))
  } else {
    cont <- httr::content(req, type = type)
  }
  
  if(is.raw(cont))
    warning(filename, " appears to be a binary file.")
  
  message(filename, " read into memory.\n")
  
  return(cont)
}