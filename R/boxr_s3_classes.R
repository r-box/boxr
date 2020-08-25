# File & Folder References --------------------------------------------------

#' boxr S3 Classes
#' 
#' boxr implements a series of S3 classes to manage the data returned by the 
#' Box API. These classes are built on `list`; if you wish to access the
#' information directly, you can use `unclass(x)`.
#' 
#' **`boxr_file_reference`**
#' 
#'  - describes a file created, modified, or deleted at Box.
#'  - returned by [box_ul()], [box_save()], [box_delete_file()], etc.
#'  - available methods: [print()].
#'     
#' **`boxr_folder_reference`** 
#' 
#'  - describes a folder created or deleted at Box.
#'  - returned by [box_dir_create()], [box_delete_folder()].
#'  - available methods: [print()].
#'   
#' **`boxr_dir_wide_operation_result`** 
#' 
#'  - describes the result of a directory-wide operation.
#'  - returned by [box_fetch()] and [box_push()].
#'  - available methods: [print()], [summary()].
#'     
#' **`boxr_object_list`**
#' 
#'  - describes a collection of files at Box.
#'  - returned by [box_ls()], [box_search()], and related functions.
#'  - available methods: [print()], [as.data.frame()].
#'     
#' **`boxr_dir_comparison`** 
#' 
#'  - describes the difference between directories.
#'  - returned by the internal function [box_dir_diff()].
#'  - available methods: [print()], [summary()].
#'  
#' **`boxr_collab`**
#' 
#'  - describes a collaboration (sharing permission).
#'  - returned by [box_collab_create()].
#'  - no available methods (yet).
#'  
#'  **`boxr_collab_list`**
#' 
#'  - describes a collaboration (sharing permission).
#'  - returned by [box_collab_get()].
#'  - no available methods (yet).
#' 
#' @name boxr_S3_classes
NULL

#' @keywords internal
add_file_ref_class <- function(x) {
class(x) <- "boxr_file_reference"
x
}

#' @keywords internal
add_folder_ref_class <- function(x) {
class(x) <- "boxr_folder_reference"
x
}

#' @export
print.boxr_file_reference <- function(x, ...) {
  # x <- object$entries[[1]]
  cat("\nbox.com remote file reference\n\n")
  cat(" name        :", x$name, "\n")
  if(x$description != "")
    cat(" description :", x$description, "\n")
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
  # x <- object$entries[[1]]
  cat("\nbox.com remote folder reference\n\n")
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
      name                = x$name,
      type                = x$type,
      id                  = x$id,
      size                = x$size %|0|% NA_integer_,
      description         = x$description,
      owner               = x$owned_by$login,
      path                = path,
      modified_at         = box_datetime(x$modified_at),
      content_modified_at = box_datetime(x$content_modified_at) %|0|% as.POSIXct(NA),
      sha1                = x$sha1 %|0|% NA_character_,
      version             = as.numeric(x$etag) + 1,
      stringsAsFactors    = FALSE
    )
  }
  
  out <- data.frame(dplyr::bind_rows(lapply(x, summarise_row)))
  return(out)
}


#' @export
print.boxr_object_list <- function(x, ...) {
  
  # Convert to data.frame
  df <- as.data.frame.boxr_object_list(x)
  # If it's empty, just cat a short message
  if (nrow(df) < 1) {
    cat("\nbox.com remote object list: Empty (no objects returned)")
    return(df)
  }
  
  # For the first 10 objects, show the first 5 cols of the df
  df <- df[1:min(nrow(df), 10),]
  
  # If there's nothing in the description field, kill it off
  if (all(df$description == ""))
    df$description <- NULL
  
  # Lower the width of it a bit
  if(!is.null(df$description))
    df$description <- trunc_end(df$description)
  
  df$path        <- trunc_start(df$path)
  df$size        <- purrr::map_chr(df$size, purrr::possibly(format_bytes, NA))
  
  cat(paste0("\nbox.com remote object list (", length(x), " objects)\n\n"))
  cat(paste0("  Summary of first ", nrow(df), ":\n\n"))
  
  print(df[, 1:5])
  
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
  
  cat("\nboxr", x$operation, "operation\n\n")
  
  # General blurb on the op
  cat(paste0(
    "  User           : ", getOption("boxr.username"), "\n",
    "  Local dir      : ", x$local_tld,                "\n",
    "  box.com folder : ", x$box_tld_id,               "\n",
    "  started at     : ", x$start , " (", tdif, ")",  "\n",
    "\n"
  ))
  
  # Produce a summary of the changes
  summarise_ops(x$file_list, x$msg_list)

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
  
  cat("\nboxr", object$operation, "operation\n\n")
  
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
  
  # Print out a summary of each of the file lists
  print_df_summary(object$file_list, object$msg_list)
  
  invisible(object)
}


# Directory Comparison ----------------------------------------------------

#' @export
print.boxr_dir_comparison <- function(x, ...) {
  cat("\nboxr remote:local directory comparison\n\n")
  
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
  summarise_ops(object_list, x$call_info$msg)
  cat("Use summary() to see individual files.")
  
  invisible(x)
}


#' @export
summary.boxr_dir_comparison <- function(object, ...) {
  cat("\nboxr remote:local directory comparison\n\n")
  
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
  
  # This just justifies the box.com id's
  if (!is.null(object$file_list[[17]]) && nrow(object$file_list[[17]]) > 0)
    object$file_list[[17]][,1] <- dir_id_tidy(object$file_list[[17]][,1])
  
  object_list <- object[names(object) != "call_info"]
  
  # Print out a summary of each of the file lists
  print_df_summary(object_list, object$call_info$msg)
  
  invisible(object)
}

# Collab Functions -----------------------------------------------

#' @importFrom tibble as_tibble
#' @export
as_tibble.boxr_collab <- function(x, ...) {
  stack_row(x)
}

# Collab Functions -----------------------------------------------

#' @export
as_tibble.boxr_collab_list <- function(x, ...) {
  stack_rows(x$entries)
}

# Internal Helper Functions -----------------------------------------------

# A function to make msg_list gramatically sensible where you have only one file
# e.g. "1 files were" -> "1 file was"
grep_tense <- function(msg, n) {
  # Non-vectorized version
  grepTense <- function(msg, n) {
    
    if(is.na(n) || is.null(n) || n > 1) {
      return(msg)
    }
    
    msg <- gsub("files", "file", msg)
    msg <- gsub("directories", "directory", msg)
    msg <- gsub("were", "was", msg)
    
    return(msg)
  }
  
  # Apply grepTense along msg and n, return result as vector
  mapply(grepTense, msg, n)
}


# Combine the file list and the message list, to print out a summary of the
# operations performed
summarise_ops <- function(file_list, msg_list) {
  # Construct the messages by combining the number of rows of the data.frame
  # with the message for the operation, e.g. "X " + "files downloaded"
  op_summaries <- unlist(mapply(
    function(x, msg) {
      if (nrow(x) > 0L) paste(nrow(x), grep_tense(msg, nrow(x)))
    }, 
    file_list, msg_list
  ))
  
  # Print the messages out
  cat(paste0(
    paste(op_summaries[!is.null(op_summaries)], collapse = ", "), 
    ".\n\n"
  ))
}

# For a file_list and a msg_list, run through the two, printing out the name
# of the operation, and the individual files affected by it
print_df_summary <- function(file_list, msg_list) {
  # Non-vectorized print function. Oh, how you wish you'd imported magrittr!
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
  # dummy_var absorbs the result (only the side-effect -- the printing to
  # console/terminal) is desired
  dummy_var <- mapply(print_df, file_list, msg_list)
}
