# boxr
[![Build Status](https://travis-ci.org/brendan-R/boxr.svg)](https://travis-ci.org/brendan-R/boxr)
[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/0.1.0/wip.svg)](http://www.repostatus.org/#wip)

A lightweight, high-level R interface for the box.com API, standing on the shoulders of [`httr`](https://github.com/hadley/httr/).

## Installation
boxr is not currently on CRAN. You can install the development version from github with

```R
# install.packages("devtools")
devtools::install_github("brendan-R/boxr")
```

## Usage
### Basic Operations
boxr provides upload/download, as well as many functions which mirror the base R file operations.

* `box_dl(file_id)` and `box_ul(file = 'path/to/file')` to download and upload files
* `box_load()`/`box_save()` to load/save remote R workspaces, and `box_source()` for remote code
* `box_read()` to read files straight into R (e.g. .csv files as data.frames)
* `box_setwd(folder_id)`/`box_getwd()` to get/set a default box folder.


### 'Git Style' Directory wide Operations
Cloud storage services like box can complement version control systems for code, that aren't well suited to large binary files (e.g. databases, .RData, or 200 pdfs). box explicitly versions binary files, keeping old ones, and making it easy fall back to an older copy.

boxr provides *git style* facilities to upload, download, and synchronize the contents of entire local and remote directories. At the time of writing, the box.com API does not support this directly, and so boxr recursively loops through directory structures.

![Synch a whole directory!](http://www.brendanrocks.com/boxr_screengrab.png)

* `box_push(folder_id, 'path/to/dir')` will update the remote directory with any new files stored locally, creating new remote directories if it needs to. Files which already exist will be updated with a new version.

* `box_fetch` works in a similar way, but for downloads
* `box_merge` is effectively `box_push` followed by `box_fetch`

These functions all have `overwrite` and `delete` parameters, which are set to `FALSE` by default, unlike git.

**Disclaimer:** box.com is no replacement for a VCS/remote-database, and familiar function names are no guarantee of expected behavior! Do check the function documentation before jumping in.

#### Working with file ids
box.com refers to files and folders using ids. You can find the id for a file from it's URL:

![Finding file and folder ids](http://www.brendanrocks.com/file_url.png)

## Getting set up
To use boxr, you need to enable API access for your box.com account. You only need to do it once - it takes around 2 minutes.

#### 1. 'Create an app'
At [https://www.box.com/developers/services](https://www.box.com/developers/services), log in and create a new 'app' for your box.com account. You can call it anything you like. This won't do anything remotely like creating an app, but it does allow you to access your account via the API.

#### 2. Set OAuth2 Parameters
On the next screen, you'll want to set **Content API Access Only**, and `http://localhost` as your **redirect_uri** as in the screenshot below.
____
![Setting OAuth2.0 parameters](http://www.brendanrocks.com/screenshot.png)

#### 3. Connect boxr to your account
This means passing your client_id and client_secret to the `box_auth` function. The box.com API and boxr use OAuth2.0; these strings aren't enough for someone to access your account maliciously (they'd still need your username and password). However, it's still a good idea to keep them safe, and out of any files or code which might be shared with others.

Run:
```R
library(boxr)
box_auth()
```

And paste/type the `client_id` and `client_secret` when prompted. If these are valid, a browser window should open, for you to formally grant yourself access to your files at box.com. An OAuth2.0 token will then reside at `~/.boxr-oauth`.

To save you having to enter the id and secret all the time, as per [best practices](https://github.com/hadley/httr/blob/master/vignettes/api-packages.Rmd#user-content-appendix-api-key-best-practices), boxr will store a copy of these for you at `~/.Renviron`, so you needn't keep having to enter them.

If you'd rather create/edit `~/.Renviron` yourself, you can do so with a text editor (make sure the last line is blank).

```
BOX_CLIENT_ID="youridhere"
BOX_CLIENT_SECRET="yoursecrethere"
 
```
Save the file, and run the R code above.

#### 4. And you're done
If `box_auth()` worked successfully, you won't need to do any of this again, and thanks to the magic of `httr` everything should *just work*. Your client_id and client_secret will be securely stored in your R environment variables, your hashed OAuth2.0 token will stored at `~/.boxr-oauth`, .gitignore'd if necessary, and automatically refreshed when needed.

# Verbosity
As moving files around might seem a bit spooky at first, boxr is by default rather verbose, printing status to the console with `cat`. This is slightly 'rude' package behaviour, and may cause unwanted output if used in conjunction with the excellent [`knitr`](https://github.com/yihui/knitr) package.

To supress messages produced using `cat`, set boxr's verbose option with:

```R
options(boxr.verbose = FALSE)
```

## Development
Happens on the `dev` branch. Pull requests welcome!

## The box Desktop App
Is an alternative if you'd rather use a GUI. I wouldn't recommend having both it and boxr operating on the same directory.

## License
MIT
