

# This will only really be shown for uploaded files. I can't think of a great
# reason to explicitly 'map' this to local versions of a file at the moment.
#
# A better version of this would keep the whole httr call, in additon
# to the boxr expression called (e.g. upload call : box_ul(blah))
#' @export
print.boxr_file_reference <- function(x, ...){
  ob <- x$entries[[1]]
  cat("box.com file reference (package: boxr)\n\n")
  cat(" name        :", ob$name, "\n")
  cat(" file id     :", ob$id, "\n")
  cat(" version     :", paste0("V", as.numeric(ob$etag) + 1), "\n")
  cat(" size        :", format_bytes(ob$size), "\n")
  cat(" modified at :", 
      as.character(as.POSIXct(gsub("T", " ", ob$modified_at))), "\n"
  )
  cat(" created at  :", 
      as.character(as.POSIXct(gsub("T", " ", ob$modified_at))), "\n"
  )
  cat(" uploaded by :", ob$modified_by$login, "\n")
  cat(" owned by    :", ob$owned_by$login, "\n")
  shared_link <- ob$shared_link
  if(is.null(shared_link))
    shared_link <- "None"
  cat(" shared link :", shared_link, "\n\n")
  cat(" parent folder name : ", ob$parent$name, "\n")
  cat(" parent folder id   : ", ob$parent$id, "\n")
  
  invisible(x)
}

# print.boxr_folder_reference <- function(x, ...){
#   
# }


