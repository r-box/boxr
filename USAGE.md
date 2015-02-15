# boxr

A lightweight, high-level `R` interface, for the box.com API.

# Installation
boxr is not currently on CRAN. If you have `devtools`, you can install the development version from github with

```R
devtools::install_github("brendan-R/boxr")
```

# Usage

# Getting set up
To use boxr, you need to enable API access for your box.com account. You only need to do it once - it takes take around 2 minutes.

## 1. Create an 'app'
At https://www.box.com/developers/services, create a new 'app' for your box.com account. You can call it anything you like. This won't actually create an app, but it allows you to access your account via the API.

## 2. Set OAuth2 Parameters
On the next screen, you'll want to set 'Content API Access Only', and `http://localhost` as your `redirect_uri` as in the screenshot below.

## 3. Connect boxr to your account
This means passing `client_id` `client_secret` to the `box_auth` function. The box.com API and boxr use OAuth2.0; these strings aren't enough for someone to access your account, however, it's still a good idea to keep them safe, and out of any files or code which might be shared with others

### The safest way
Best practice is *not* to type your id or secret into the terminal/console, as they might be logged to your `.Rhistory`, which could be accessible to others. To avoid this use a text editor to add them to `~/.Renviron` (your `R` environment variables), like so:

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

### A slightly easier way
If your .Rhistory file is secure (or you don't use one), it's easiest to run `box_auth()`, and copy and paste the `client_id` and `client_secret` when prompted. boxr will handle the rest for you.

## And you're done
If `box_auth()` worked successfully, you won't need to do any of this again, and thanks to the magic of `httr` *everything should just work*.

# How Authentication Works

your `client_id` and `client_secret` will be securely stored in your R environment variables, and `httr` will have stored an OAuth2.0 token at `~/.boxr-oauth`.



# Development
Happens on the `dev` branch. Pull requests welcome!

# License
