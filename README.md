# boxr
A lightweight, high-level `R` interface to the box.com API.


## TODO:

### Housekeeping
* Tidy up the auth function. It's messy as hell.
* Get something running at startup which either refreshes the oauth token or 
mentions box_auth()
* Make the catif messages summarise themselves when a functions has completed. 
For example (4 files unchannged, 3 new files downloaded, 1 new directory)
* Get good, informative error messages

### Classes
* Add a basic s3 class for folders
* Get an s3 class for multifile operations, which summarize what happened
* Write some proper tests with testthat
* Send to Ranjit, see if he can break it for you

### Publication
* Write a little usage guide
* write some examples
* Fill out the see also sections
* Stick it on github
* Set up travis
* Stick it on cran

### Lofty ideal
* Write something like file.choose which uses the view api to allow users to 
select files using a browser

## Done
