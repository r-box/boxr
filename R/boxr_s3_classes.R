
# File & Folder References --------------------------------------------------

#' @export
print.boxr_file_reference <- function(x, ...) {
  cat("box.com remote file reference\n\n")
  cat(" name        :", x$name, "\n")
  cat(" file id     :", x$id, "\n")
  cat(" version     :", paste0("V", as.numeric(x$etag) + 1), "\n")
  cat(" size        :", format_bytes(x$size), "\n")
  cat(" modified at :", 
      as.character(as.POSIXct(gsub("T", " ", x$modified_at))), "\n"
  )
  cat(" created at  :", 
      as.character(as.POSIXct(gsub("T", " ", x$modified_at))), "\n"
  )
  cat(" uploaded by :", x$modified_by$login, "\n")
  cat(" owned by    :", x$owned_by$login, "\n")
  shared_link <- x$shared_link
  if (is.null(shared_link))
    shared_link <- "None"
  cat(" shared link :", shared_link, "\n\n")
  cat(" parent folder name : ", x$parent$name, "\n")
  cat(" parent folder id   : ", x$parent$id, "\n")
  
  invisible(x)
}


#' @export
print.boxr_folder_reference <- function(x, ...) {
  cat("box.com remote folder reference\n\n")
  cat(" name        :", x$name, "\n")
  cat(" dir id      :", x$id, "\n")
  cat(" size        :", format_bytes(x$size), "\n")
  cat(" modified at :", 
      as.character(as.POSIXct(gsub("T", " ", x$modified_at))), "\n"
  )
  cat(" created at  :", 
      as.character(as.POSIXct(gsub("T", " ", x$modified_at))), "\n"
  )
  cat(" uploaded by :", x$modified_by$login, "\n")
  cat(" owned by    :", x$owned_by$login, "\n")
  shared_link <- x$shared_link
  if (is.null(shared_link))
    shared_link <- "None"
  cat(" shared link :", shared_link, "\n\n")
  cat(" parent folder name : ", x$parent$name, "\n")
  cat(" parent folder id   : ", x$parent$id, "\n")
  
  invisible(x)
}


# Object Lists ------------------------------------------------------------

#' @export
as.data.frame.boxr_object_list <- function(x, ...) {

  summarise_row <- function(x) {
    path <- paste0(unlist(
      lapply(x$path_collection$entries, function(x) x$name)
    ), collapse = "/")
    
    data.frame(
      name = x$name,
      type = x$type,
      id = x$id,
      size = x$size,
      description = x$description,
      owner = x$owned_by$login,
      path = path,
      stringsAsFactors = FALSE
    )
  }
  
  out <- data.frame(dplyr::bind_rows(lapply(x, summarise_row)))
  
  # If there's nothing in the description field, kill it off
  if (all(out$description == ""))
    out$description <- NULL
  
  return(out)
}


#' @export
print.boxr_object_list <- function(x, ...) {
  df <- as.data.frame.boxr_object_list(x)
  
  # Lower the width of it a bit
  df$description <- trunc_end(df$description)
  df$path        <- trunc_start(df$path)
  df$size        <- format_bytes(df$size)
  
  cat("box.com remote object list\n\n")
  cat(" Summary of first 20 results:\n\n")
  
  print(head(df))
  
  cat("\n\nUse as.data.frame() to extract full results.\n")
  
  invisible(x)
}


# Directory-Wide Operations -----------------------------------------------

# A better version of this would keep the whole httr call, in additon
# to the boxr expression called (e.g. upload call : box_ul(blah))
#' @export
print.boxr_dir_wide_operation_result <- function(x, ...) {
  
  boxr_timediff <- function(x)
    paste0("took ", format(unclass(x), digits = 3), " ", attr(x, "units"))
  
  f <- x$file_list
  
  tdif <- boxr_timediff(x$end - x$start)
  
  cat("boxr", x$operation, "operation\n\n")
  
  # General blurb on the op
  cat(paste0(
    "  User           : ", getOption("boxr.username"), "\n",
    "  Local dir      : ", x$local_tld,                "\n",
    "  box.com folder : ", x$box_tld_id,               "\n",
    "  started at     : ", x$start , " (", tdif, ")",  "\n",
    "\n"
  ))
  
  # Produce a summary of the changes
  summary_items <- 
    unlist(mapply(
      function(x, msg) if (nrow(x) > 0L) paste(nrow(x), msg), 
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
summary.boxr_dir_wide_operation_result <- function(object, ...) {
  
  boxr_timediff <- function(x)
    paste0("took ", format(unclass(x), digits = 3), " ", attr(x, "units"))
  
  f <- object$file_list
  
  tdif <- boxr_timediff(object$end - object$start)
  
  cat("boxr", object$operation, "operation\n\n")
  
  # General blurb on the op
  cat(paste0(
    "  User           : ", getOption("boxr.username"),     "\n",
    "  Local dir      : ", object$local_tld,               "\n",
    "  box.com folder : ", object$box_tld_id,              "\n",
    "  started at     : ", object$start , " (", tdif, ")", "\n",
    "\n"
  ))
  
  # This just justifies the box.com id's
  if (!is.null(object$file_list[[17]]) && nrow(object$file_list[[17]]) > 0)
    object$file_list[[17]][,1] <- dir_id_tidy(object$file_list[[17]][,1])
  
  print_df <- function(x, msg) {
    if (nrow(x) > 0) {
      cat(nrow(x), msg, ":\n")
      print(
        format(
          data.frame(
            " " = x[,grepl("full_path",colnames(x))],
            check.names = FALSE
          ), 
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


# Directory Comparison ----------------------------------------------------

#' @export
print.boxr_dir_comparison <- function(x, ...) {
  cat("boxr remote:local directory comparison\n\n")
  
  origin <- if (x$call_info$load == "up") "Local directory" else "box.com"
  destin <- if (x$call_info$load != "up") "Local directory" else "box.com" 
  
  # General blurb on the op
  cat(paste0(
    "  User           : ", getOption("boxr.username"),    "\n",
    "  Local dir      : ", x$call_info$local_dir,         "\n",
    "  box.com folder : ", x$call_info$dir_id,            "\n",
    "  Direction      : ", x$call_info$load, "load",      "\n",
    "  Origin         : ", origin,                        "\n",
    "  Destination    : ", destin,                        "\n",
    "\n"
  ))
  
  object_list <- x[names(x) != "call_info"]
  
  # Produce a summary of the differences
  summary_items <- 
    unlist(mapply(
      function(x, msg) if (nrow(x) > 0L) paste(nrow(x), msg), 
      object_list, x$call_info$msg
    ))
  
  cat(paste0(
    paste(summary_items[!is.null(summary_items)], collapse = ", "), 
    ".\n\n"
  ))
  #   
  cat("Use summary() to see individual files.")
  
  invisible(x)
  
}


#' @export
summary.boxr_dir_comparison <- function(object, ...) {
  cat("boxr remote:local directory comparison\n\n")
  
  origin <- if (object$call_info$load == "up") "Local directory" else "box.com"
  destin <- if (object$call_info$load != "up") "Local directory" else "box.com" 
  
  # General blurb on the op
  cat(paste0(
    "  User           : ", getOption("boxr.username"),    "\n",
    "  Local dir      : ", object$call_info$local_dir,    "\n",
    "  box.com folder : ", object$call_info$dir_id,       "\n",
    "  Direction      : ", object$call_info$load, "load", "\n",
    "  Origin         : ", origin,                        "\n",
    "  Destination    : ", destin,                        "\n",
    "\n"
  ))
  
  object_list <- object[names(object) != "call_info"]
  
  # This just justifies the box.com id's
  if (!is.null(object$file_list[[17]]) && nrow(object$file_list[[17]]) > 0)
    object$file_list[[17]][,1] <- dir_id_tidy(object$file_list[[17]][,1])
  
  print_df <- function(x, msg) {
    if (nrow(x) > 0) {
      cat(nrow(x), msg, ":\n")
      print(
        format(
          data.frame(
            " " = x[,grepl("full_path",colnames(x))],
            check.names = FALSE
          ), 
          justify = "left"
        ), 
        row.names = FALSE
      )
      cat("\n\n")
    }
  }
  
  # Run through the file df's in file_list, print out messages for them
  dummy_var <- mapply(print_df, object_list, object$call_info$msg)
  
  invisible(object)
  
}
