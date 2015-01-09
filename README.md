

# boxr
A lightweight, high-level `R` interface to the box.com API.



## TODO:
You need to make an intial (fast call to the api) when box_auth is called.

You should have a box_on_startup function which calls a less bossy verision of box_auth, and refreshes the security token

* You need to 

Add print messages
  Recursively scanning box directory structure...
  Building local directory tree
  name-of-dir (x files up to date, 3 missing, 4 to update)
  name-of-dir(truncated w/...): Downloading 1 of X files ()

txtProgressBar - seems to do something dynamic - copy it!

Print one line per directory..?
Make messages supressable

for(i in 1:10) {
  Sys.sleep(0.2)
  # Dirk says using cat() like this is naughty ;-)
  #cat(i,"\r")
  # So you can use message() like this, thanks to Sharpie's
  # comment to use appendLF=FALSE.
  message("\r", i, "\r", appendLF=FALSE)
  flush.console()
}
# 
for(i in 1:10) {
  Sys.sleep(0.2)
  # Dirk says using cat() like this is naughty ;-)
  cat("\r", "downloading files (", i,"/10)" , "\r", sep = "")
  # So you can use message() like this, thanks to Sharpie's
  # comment to use appendLF=FALSE.
  # message(i, appendLF=FALSE)
  flush.console()
}

switch(menu(c("List letters", "List LETTERS")) + 1,
              cat("Nothing done\n"), letters, LETTERS, graphics = TRUE)

select.list(sort(.packages(all.available = TRUE)))



You could download the dir strucure from box, and store it in a similar way to extrafont

You could also make a search function, with pagination
Ideally, you'd make something dynamic

Error handling - check out stop_for_status





Function names to consider:
box_dl
box_ul
box_pull, box_push, box_merge, etc. Allow messages, which write to descriptions on the files.
    You could have a value for 'detect' which means use the local database of directory maps
box_version to look at the versions of different files
box_set_version
box_search
box_setwd
box_getwd
box_compare (to compare a local and hosted version of a file)
box_save (workspace)
box_write (to save r objects into a workspace)
box_load (workspace)
box_read (for reading the files into R, as opposed to writing them to disk)
box_auth
You'll need an (internal?) function that runs at start-up, which looks for the credentials/token, and suggests running box_setup() if they're missing





## Notes (to self)
Hadley's guide to oAuth2
Taken from https://github.com/hadley/httr/blob/master/demo/oauth2-google.r
https://github.com/hadley/httr/blob/master/vignettes/api-packages.Rmd
To create an app, go here: https://box.com/developers/services/edit
Get the keys, go here: https://box.com/developers/services

