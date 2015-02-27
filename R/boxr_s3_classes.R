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

# A little easier on the eye than the default


# This will only really be shown for uploaded files. I can't think of a great
# reason to explicitly 'map' this to local versions of a file at the moment.
#
# A better version of this would keep the whole httr call, in additon
# to the boxr expression called (e.g. upload call : box_ul(blah))
#' @export
print.boxr_dir_wide_operation_result <- function(x, ...){
  
  boxr_timediff <- function(x)
    paste0("took ", format(unclass(x), digits = 3), " ", attr(x, "units"))
  
  f <- x$file_list
  
  tdif <- boxr_timediff(x$end - x$start)
  
  cat("boxr", x$operation, "operation\n\n")
  
  # General blurb on the op
  cat(paste0(
    "User           : ", getOption("boxr.username"), "\n",
    "Local dir      : ", x$local_tld, "\n",
    "box.com folder : ", x$box_tld_id, "\n",
    "started at     : ", x$start , " (", tdif, ")", "\n",
    "\n"
  ))
  
  # Produce a summary of the changes
  summary_items <- 
    unlist(mapply(
      function(x, msg) if(nrow(x) > 0L) paste(nrow(x), msg), 
      x$file_list, x$msg_list
    ))
  
  cat(paste0(
    paste(summary_items[!is.null(summary_items)], collapse = ", "), 
    ".\n\n"
  ))
  
  cat("Use summary() to see individual file operations.")
  invisible(x)
}



# This will only really be shown for uploaded files. I can't think of a great
# reason to explicitly 'map' this to local versions of a file at the moment.
#
# A better version of this would keep the whole httr call, in additon
# to the boxr expression called (e.g. upload call : box_ul(blah))
#' @export
summary.boxr_dir_wide_operation_result <- function(object, ...){
  
  boxr_timediff <- function(object)
    paste0("took ", format(unclass(object), digits = 3), " ", attr(object, "units"))
  
  f <- object$file_list
  
  tdif <- boxr_timediff(object$end - object$start)
  
  cat("boxr", object$operation, "operation\n\n")
  
  # General blurb on the op
  cat(paste0(
    "User           : ", getOption("boxr.username"), "\n",
    "Local dir      : ", object$local_tld, "\n",
    "box.com folder : ", object$box_tld_id, "\n",
    "started at     : ", object$start , " (", tdif, ")", "\n",
    "\n"
  ))
  
  print_df <- function(object, msg){
    if(nrow(object) > 0){
      cat(nrow(object), msg, ":\n")
      print(
        format(
          setNames(data.frame(object), ""), 
          justify = "left"
        ), 
        row.names = FALSE
      )
      cat("\n\n")
    }
  }
  
  # Run through the file df's in file_list, print out messages for them
  dummy_var <- mapply(print_df, object$file_list, object$msg_list)
  
  invisible(object)
}