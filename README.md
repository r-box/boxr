# boxr
[![Build Status](https://travis-ci.org/brendan-r/boxr.svg)](https://travis-ci.org/brendan-r/boxr)
[![Win Build Status](https://ci.appveyor.com/api/projects/status/github/brendan-r/boxr?branch=master&svg=true)](https://ci.appveyor.com/project/brendan-r/boxr)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://img.shields.io/badge/repo%20status-active-brightgreen.svg)](http://www.repostatus.org/#active)
[![cran version](http://www.r-pkg.org/badges/version/boxr)](http://cran.rstudio.com/web/packages/boxr)
![monthly_downloads](http://cranlogs.r-pkg.org/badges/boxr)

A lightweight, high-level R interface for the box.com API, standing on the shoulders of [`httr`](https://github.com/hadley/httr/).

## Installation
You can download boxr from [CRAN](http://cran.r-project.org/web/packages/boxr/), with

```R
install.packages("boxr")
```

If you'd like to download the development version from GitHub, use

```R
# install.packages("devtools")
devtools::install_github("brendan-r/boxr")
```

## Usage
### Basic Operations
Aside from file upload/download, boxr provides functions which mirror base R operations for local files.

* `box_dl(file_id)` and `box_ul(file = 'path/to/file')` download and upload files, respectively
* `box_search()` to query files & folders stored on box.com
* `box_read()` read files straight into R (e.g. .csv or .xlsx files as `data.frames`)
* `box_write()` write R objects to remotely hosted files
* `box_setwd()`/`box_getwd()` get/set a default box folder
* `box_source()` read and execute remote code
* `box_load()`/`box_save()` for remote R workspaces
* `box_add_description()` add text descriptions to your files on box.com


### Directory wide Operations
Cloud storage services can complement version control systems for code, which aren't well suited to large binary files (e.g. databases, .RData, or heaps of pdfs). box explicitly versions binary files, keeping old ones, and making it easy fall back to an older copy.

boxr provides *git style* facilities to upload, download, and synchronize the contents of entire local and remote directories. At the time of writing, the box.com API does not support this directly, and so boxr recursively loops through directory structures.

![Synch a whole directory!](https://s3-us-west-2.amazonaws.com/brendan-misc/boxr_console.png)

* `box_push` will update the remote directory with new/changed local files
* `box_fetch` will update the local directory with new/changed remote files

These functions all have `overwrite` and `delete` parameters, which are set to `FALSE` by default.

**Disclaimer:** box.com is no replacement for a VCS/remote-database, and familiar verbs are no guarantee of expected behavior! Do check the function documentation before jumping in.

### Piping
boxr's functions have been designed to be 'pipable'. Here's a little example:

```r
library(boxr)
library(dplyr)
library(magrittr)

# 'nycflights13.json' is the same as nycflights13::flights, if you want to
# follow along at home

box_auth()

box_search("nycflights13.json") %>%                # Find a remote file
  box_read() %>%                                   # Download it as a data.frame
    group_by(origin, dest, month) %>%              #   Do some, er, cutting edge
    summarise(mu = mean(arr_delay), n = n()) %>%   #   analysis with dplyr!
  box_write("delay_summary.xlsx") %>%              # Convert to .xlsx, upload
  box_add_description("Check out these averages!") # Add a description to your file!

```

### File/Folder IDs
Are how box.com identifies things. You can find them in an item's URL:

![Finding file and folder ids](https://s3-us-west-2.amazonaws.com/brendan-misc/file_ids.png)

## Getting set up
To use boxr, you need to enable API access for your box.com account. The process is slightly annoying. You only need to do it once - it takes around 2 minutes.

#### 1. 'Create New App'
Go to [https://app.box.com/developers/console](https://app.box.com/developers/console), (when you are logged in) and click on the button 'Create New App', which will guide you through four screens to create your new app.

* On the first, select **Custom App** and click 'Next'.
* On the second, select **Standard OAuth 2.0 (User Authentication)** and click 'Next'
* On the third, choose a unique name for your app, this can be anything and click 'Next'
* The fourth screen should be a confirmation of successful creation, click 'View Your App'

![Four steps](images/four_steps.png)


#### 2. Set OAuth2 Parameters

'View Your App' will take you to the **Box Developers Console** and where you will be in the **Configuration** sub-menu by default. Scroll down to **OAuth 2.0 Redirect URI** and set it to `http://localhost` and be sure to click 'Save Changes'.

![Set Redirect URI](images/redirect_uri.png)


Keep this browser window open because you will need the client_id and client_secret for the next steps back in `R`.

#### 3. Connect boxr to your account
This means passing your client_id and client_secret to the `box_auth` function. These strings are not enough for someone to access your account maliciously. However, it's still a good idea to keep them safe, and out of any files or code which might be shared with others.

Run:

```R
library(boxr)
box_auth()
```

And paste/type the `client_id` and `client_secret` when prompted. If these are valid, a browser window should open, for you to formally grant yourself access to your files at box.com.


#### 4. And you're done
If `box_auth()` worked successfully, you won't need to do any of this again, and thanks to the magic of `httr` everything should *just work*. Your client_id and client_secret will be securely stored in your R environment variables, your hashed OAuth2.0 token will stored at `~/.boxr-oauth`, .gitignore'd if necessary, and automatically refreshed when needed. If you would like to automatically authenticate each time the package is loaded, consider running `box_auth_on_attach(TRUE)`.


## Notes
#### Verbosity
boxr is by default rather verbose, printing status to the console with `cat`. This is 'rude' package behaviour, and may cause unwanted output if used in conjunction with the excellent [`knitr`](https://github.com/yihui/knitr) package.

To supress messages produced using `cat`, set boxr's verbose option with:

```R
options(boxr.verbose = FALSE, boxr.progress = FALSE)
```

#### Alternatives
boxr aims to expedite data analysis/communication/distribution. Other ways to manipulate a box.com account include:

* The box desktop app
* The *other* boxr, [written in Ruby](https://github.com/cburnette/boxr). It's motivations are rather different, and it covers 100% of the box.com API (e.g account administration, etc.)
* box themselves [provide a wide range of SDKs](https://github.com/box), including [one for Python](https://github.com/box/box-python-sdk)

#### Managing your client id & secret
If you don't like the idea of typing credentials into your console, you can put them straight into `~/.Renviron` yourself, prior to the R session:

```bash
BOX_CLIENT_ID="youridhere"
BOX_CLIENT_SECRET="yoursecrethere"

```

(Note the final blank line).

#### Contributing
Always very welcome! If you'd like to submit a pull request for a new feature, ideally it would be documented, come with an addtion to [NEWS.md](NEWS.md), and have a test or two. This project has a standard [Code of Conduct](CONDUCT.md).

##### Remote Usage (e.g. ssh / rstudio-server)
This proves tricky for the OAuth2.0 dance, as it relies on browser access. While not 'officially supported', it's possible to authenticate on a local machine to generate a cached token (by default, at `~/.boxr-oauth`), and then copy the file over to the eqivalent location on the remote machine. boxr can then read the copied file during remote code execution.

## Troubleshooting
Via [GitHub issues](https://github.com/brendan-R/boxr/issues), please. It's been a while since I've read the R mailing lists!

### In General
If you run in to problems, first make sure that your credentials are correct, and that you're using the latest version of boxr. You can update boxr with:

```
install.packages("boxr")
```

### Problems Authenticating
Sometimes an old token can cause problems during authentication. If you're sure that your `client_id` and `client_secret` are correct, but `box_auth()` isn't working, try the following in a fresh session:

```r
install.packages("boxr") # Make sure you're using the latest version of boxr
library(boxr)            # Load it
box_fresh_auth()         # Delete the old token, and reauthenticate
```

This will delete the old token (by default stored at `~/.boxr-oauth`), and start the auth process afresh. A browser window will open, and you'll be prompted to sign in to your box account to verify yourself.

## License
The MIT License (MIT)

Copyright (c) 2015-2017 boxr contributors

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
