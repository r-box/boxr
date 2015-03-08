# A function to exit, returning an object of class
# boxr_dir_wide_operation_result
returnDwOp <- function(op_detail){
  
  # As this is supposed to be a list of lists (not just, lists), put a list in a
  # list - but only if it's just a list.
  if(! "list" %in% class(op_detail[[1]][[1]])){
    op_detail <- list(op_detail)
  }
  
  items_list <- 
    c(
      "successful_downloads",
      "unsuccessful_downloads",
      "successful_updates",
      "unsuccessful_updates",
      "successful_uploads",
      "unsuccessful_uploads",
      "up_to_date",
      "successful_remote_file_deletions",
      "unsuccessful_remote_file_deletions",
      "successful_remote_folder_deletions",
      "unsuccessful_remote_folder_deletions",
      "successful_local_file_deletions",
      "unsuccessful_local_file_deletions",
      "successful_local_folder_deletions",
      "unsuccessful_local_folder_deletions",
      "local_new_dirs",
      "remote_new_dirs"
    )
  
  msg_list <- 
    list(
      "files downloaded from box.com",
      "files were NOT downloaded from box.com",
      "files updated on box.com",
      "files were NOT updated on box.com",
      "new files uploaded to box.com",
      "new files were NOT uploaded to box.com",
      "files were already up-to-date on box.com (nothing done)",
      "files were sucessfully trashed on box.com",
      "files could not be trashed on box.com",
      "directories were sucessfully trashed on box.com",
      "directories could not be trashed on box.com",
      "local files were deleted",
      "local files could not be deleted",
      "local directories were deleted",
      "local directories could not be deleted",
      "new local directories created",
      "new remote directories created"
    )
  
  file_list <- 
    lapply(
      items_list,
      function(item)
        data.frame(dplyr::bind_rows(lapply(
          op_detail$files, function(x) data.frame(x[item])
        )))
    )
  
  out <- 
    structure(
      list(
        operation    = op_detail$operation,
        start        = op_detail$t1,
        end          = Sys.time(),
        local_tld    = op_detail$local_tld,
        # box_tld_name = op_detail$box_tld_name,
        box_tld_id   = op_detail$box_tld_id,
        file_list    = file_list,
        msg_list     = msg_list
      ),
      class = "boxr_dir_wide_operation_result"
    )
  
  return(out)
}
