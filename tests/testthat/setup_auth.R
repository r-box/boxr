if (gargle:::secret_can_decrypt("boxr")) {
  raw <- gargle:::secret_read("boxr", "boxr-testing.json")
  text <- rawToChar(raw)
  box_auth_service(token_text = text)
}
