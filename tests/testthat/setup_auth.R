if (gargle:::secret_can_decrypt("boxr")) {
  json <- gargle:::secret_read("boxr", "boxr-testing.json")
  box_auth_jwt(user_id = 9459307839, config_file = rawToChar(json))
}
