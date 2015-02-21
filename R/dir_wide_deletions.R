# You need to modify box_dir_diff to output dirs to delete, too

deleteRemoteObjects <- function(dir_id, local_dir = getwd()){
  
  box_dd <- box_dir_diff(dir_id, local_dir, load = "up", folders = TRUE)
  if(is.null(box_dd))
    return(NULL)
  
  # Run through the files to delete
  file_deletions   <- list()
  folder_deletions <- list()
  
  # Run through the files to delete
  if(nrow(box_dd$superfluous) > 0)
    for(i in 1:nrow(box_dd$superfluous)){
      catif(
        paste0(
          "Moving files to trash (", i,"/",nrow(box_dd$superfluous),"): ", 
          box_dd$superfluous$name[i]
        )
      )
      file_deletions[[i]] <- box_delete_file(box_dd$superfluous$id[i])
    }
  
  # An output object for files
  file_deletion_success <- 
    unlist(
      lapply(file_deletions, function(x) httr::http_status(x)$category == "success")
    )
  
  # Run through the folders to delete
  if(nrow(box_dd$superfluous_folders) > 0)
    for(i in 1:nrow(box_dd$superfluous_folders)){
      catif(
        paste0(
          "Moving folders to trash (", i,"/",nrow(box_dd$superfluous_folders),"): ", 
          box_dd$superfluous_folders$name[i]
        )
      )
      folder_deletions[[i]] <- box_delete_folder(box_dd$superfluous_folders$id[i])
    }
  
  # An output object for files
  folder_deletion_success <- 
    unlist(
      lapply(folder_deletions, function(x) httr::http_status(x)$category == "success")
    )
  
  
  # Initialize these, for the sake of error handling
  successful_file_deletions <- unsuccessful_file_deletions <- 
    successful_folder_deletions <- unsuccessful_folder_deletions <- data.frame()
  
  if(length(file_deletion_success) > 0 & length(folder_deletion_success) > 0){
    successful_file_deletions <- 
      box_dd$superfluous[ file_deletion_success]
    
    unsuccessful_file_deletions <- 
      box_dd$superfluous[!file_deletion_success]
    
    successful_folder_deletions <- 
      box_dd$superfluous_folders[ folder_deletion_success]
    
    unsuccessful_folder_deletions <- 
      box_dd$superfluous_folders[!folder_deletion_success]
  }
  
  out <- 
    list(
      successful_file_deletions     = successful_file_deletions,
      unsuccessful_file_deletions   = unsuccessful_file_deletions,
      successful_folder_deletions   = successful_folder_deletions,
      unsuccessful_folder_deletions = unsuccessful_folder_deletions
    )
  
  return(out)
}
