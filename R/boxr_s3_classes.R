# File & Folder References --------------------------------------------------

#' boxr S3 Classes
#' 
#' @description {
#'   boxr has a few very simple S3 classes for the data returned by the API.
#'   While \code{\link{httr}} returns objects in it's own system of classes,
#'   generally boxr extracts information, and converts the JSON response to an R
#'   list with httr::\code{\link{content}} (perhaps with some information judged
#'   to be extraneous removed). If you'd rather get to the list itself, you can
#'   do this with  \code{unclass(x)}.
#'   
#'   The following classes are used:
#'   
#'   \describe{
#'     \item{\bold{boxr_file_reference}}{
#'       Returned by \code{\link{box_ul}}, and similar functions (e.g. 
#'       \code{\link{box_save}}). A description of a file remotely hosted on 
#'       box.com. Available methods: \code{print}.
#'     }
#'     \item{\bold{boxr_folder_reference}}{
#'       As above, but for folders/direcotries. Available methods: \code{print}
#'     }
#'     \item{\bold{boxr_object_list}}{
#'       Returned by \code{\link{box_search}}, and related functions. A list,
#'       with each entry being a reference to a file or folder hosted on box.com
#'       . Available methods: \code{print}, for a summary of the first few
#'       results, and \code{as.data.frame}, to coerce some of the API response's
#'       information to a \code{\link{data.frame}}.
#'     }
#'     \item{\bold{boxr_dir_comparison}}{
#'       Returned by the internal function \code{\link{box_dir_diff}}. Available
#'       methods: \code{print}, \code{summary}.
#'     }
#'     \item{\bold{boxr_dir_wide_operation_result}}{
#'       Returned by \code{\link{box_fetch}} and \code{\link{box_push}}.
#'       Available methods: \code{print}, \code{summary}
#'     }
#'   }
#' }
#'
#' @author Brendan Rocks \email{rocks.brendan@@gmail.com}
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
      size                = x$size,
      description         = x$description,
      owner               = x$owned_by$login,
      path                = path,
      modified_at         = box_datetime(x$modified_at),
      content_modified_at = box_datetime(x$content_modified_at),
      sha1                = ifelse(is.null(x$sha1), NA, x$sha1),
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
  df$size        <- format_bytes(df$size)
  
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
