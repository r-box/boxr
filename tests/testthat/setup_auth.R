# To use this auth, you need an environment variable BOXR_PASSWORD.
# If you are a maintainer of this repository, you have that environment variable.
#
# On GitHub Actions, it runs many on platforms simultaneously. We want only 
# one of them to run this auth (to interact with the API), so we set another 
# environment variable, BOXR_USE_TOKEN. We set this variable to `"true"` on 
# the platform we want want to run this auth.
#
use_token <- identical(Sys.getenv("BOXR_USE_TOKEN"), "true")

path_secret <- system.file("secret", "boxr-testing.json", package = "boxr")
env_secret <- "BOXR_PASSWORD"

# NOTE 2024-01-07: 
#  - the key used with the deprecated gargle functions had 50 characters
#  - the new gargle functions can accept a key max 32 characters
#  - if you have an old "BOXR_PASSWORD", truncate it to use the first 32 chars 

if (nchar(Sys.getenv(env_secret)) > 32) {
  # TODO: replace with cli::cli_alert_warning()
  warning(
    paste(
      "You may be using an older `BOXR_PASSWORD`.", 
      "Truncate to use the first 32 characters."      
    )
  )
}

if (use_token && gargle::secret_has_key(env_secret)) {
  text <- gargle::secret_decrypt_json(path_secret, env_secret)
  box_auth_service(token_text = text)
}

