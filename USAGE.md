# boxr

A lightweight, high-level `R` interface for the box.com API.

# Installation
boxr is not currently on CRAN. If you have `devtools`, you can install the development version from github with

```R
devtools::install_github("brendan-R/boxr")
```

# Usage
The most basic operations are `box_dl(file_id)` and `box_ul('path/to/file')` to download and upload files, respectively. In addition, boxr provides functions which mirror many base R file operations such as `box_load()` `box_save()` to load and save R workspaces, `box_read()` to read files straight into R (e.g. .csv files as data.frames), `box_ls(folder_id)` to see the contents of a box folder, and `box_setwd(folder_id)` and `box_getwd()` to get and set a default box folder.


## Directory wide operations
Cloud storage services like box can complement version control systems for code (e.g. git), that aren't well suited to tracking large binary files, such as databases, .RData, or large pdfs. box explicitly versions binary files, keeping old versions, and making it easy to roll back to a previous one.

boxr provides *git-style* facilities to upload, download, and synchronize the contents of local and remote directories. At the time of writing, the box.com API does not support this directly, and so boxr recursively loops through directory structures.

`box_push(folder_id, path/to/files)` will update the remote directory with any new files stored locally, creating new remote directories if it needs to. Files which already exist will be updated with a new version.

`box_fetch(folder_id, path/to/files)` works in a similar way, but for downloads, and `box_merge` is effectively `box_fetch();box_push()`. All of these functions have an `overwrite` parameter, which by default is `FALSE`.


### Working with file ids
box.com refers to files and folders using ids.

![Finding file and folder ids](http://www.brendanrocks.com/file_url.png)

# The box.com desktop app
box.com provide a desktop app which can do similar things, with a GUI. If this works for you, you probably don't need these functions (it's best not to use )

![Finding file and folder ids](http://www.brendanrocks.com/file_url.png)

# Getting set up
To use boxr, you need to enable API access for your box.com account. You only need to do it once - it takes take around 2 minutes.

### 1. Create an 'app'
At https://www.box.com/developers/services, create a new 'app' for your box.com account. You can call it anything you like. This won't actually create an app, but it allows you to access your account via the API.

### 2. Set OAuth2 Parameters
On the next screen, you'll want to set 'Content API Access Only', and `http://localhost` as your `redirect_uri` as in the screenshot below.

On the next screen, you'll want to set **Content API Access Only**, and `http://localhost` as your **redirect_uri** as in the screenshot below.
![Setting OAuth2.0 parameters](http://www.brendanrocks.com/screenshot.png)

### 3. Connect boxr to your account
This means passing your client_id client_secret to the `box_auth` function. The box.com API and boxr use OAuth2.0; these strings aren't enough for someone to access your account. However, it's still a good idea to keep them safe, and out of any files or code which might be shared with others

**The safest way**
Best practice is *not* to type your id or secret into the terminal/console, as they might be logged to your `.Rhistory`, which could be accessible to others. To avoid this, use a text editor to add them to `~/.Renviron` (your `R` environment variables), like so:

```
BOX_CLIENT_ID="youridhere"
BOX_CLIENT_SECRET="yoursecrethere"
```

(Make sure there's a blank line at the end of the file.)

Save the file, and in R, simply run

```R
libary(boxr)
box_auth()
```

**The easiest way**
If your .Rhistory file is secure (or you don't use one), it's easiest to run `box_auth()`, and copy and paste the `client_id` and `client_secret` when prompted. boxr will handle the rest for you.

## And you're done
If `box_auth()` worked successfully, you won't need to do any of this again, and thanks to the magic of `httr` *everything should just work*.

# How Authentication Works

your `client_id` and `client_secret` will be securely stored in your R environment variables, and `httr` will have stored an OAuth2.0 token at `~/.boxr-oauth`.



# Development
Happens on the `dev` branch. Pull requests welcome!

# License
GPL 3 (see LICENCE).