# You need to modify box_dir_diff to output dirs to delete, too

deleteRemoteObjects <- function(dir_id, local_dir = getwd()){
  
  box_dd <- box_dir_diff(dir_id, local_dir, load = "up")
  if(is.null(box_dd))
    return(NULL)
  
  # Run through the files to delete
  deletions <- list()
  
  # Run through the files to delete
  if(nrow(box_dd$superfluous) > 0)
    for(i in 1:nrow(box_dd$superfluous)){
      catif(
        paste0(
          "Moving files to trash (", i,"/",nrow(box_dd$superfluous),"): ", 
          box_dd$superfluous$name[i]
        )
      )
      deletions[[i]] <- box_delete_file(box_dd$superfluous$id[i])
    }
  
  # An output object
  deletion_success <- 
    unlist(
      lapply(uploads, function(x) httr::http_status(x)$category == "success")
    )
    
  # Initialize these, for the sake of error handling
  successful_deletions <- unsuccessful_deletions <- data.frame()
  
  if(length(deletion_success) > 0){
    successful_deletions   <- box_dd$superfluous[ deletion_success]
    unsuccessful_deletions <- box_dd$superfluous[!deletion_success]
  }
  
  out <- 
    list(
      successful_deletions   = successful_deletions,
      unsuccessful_deletions = unsuccessful_deletions,
    )
  
  return(out)
}
