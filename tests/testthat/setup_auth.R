# To use this auth, you need an environment variable BOXR_PASSWORD.
# If you are a maintainer of this repository, you have that environment variable.
#
# On GitHub Actions, it runs many on platforms simultaneously. We want only 
# one of them to run this auth (to interact with the API), so we set another 
# environment variable, BOXR_USE_TOKEN. We set this variable to `"true"` on 
# the platform we want want to run this auth.
#
use_token <- identical(Sys.getenv("BOXR_USE_TOKEN"), "true")

if (use_token && gargle:::secret_can_decrypt("boxr")) {
  raw <- gargle:::secret_read("boxr", "boxr-testing.json")
  text <- rawToChar(raw)
  box_auth_service(token_text = text)
}

