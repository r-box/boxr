# Thoughts on how testing might work with Travis

(There doesn't seem to be a straight-forward or generally accepted way of doing this.)

1. Set Travis up with it's own 'app'
2. Encrypt the client id and secret as Travis env vars
3. Host as a public file, the token refresh key
4. Use all of the above in combination to assemble a refreshed token httr object in R, during testing