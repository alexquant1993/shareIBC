# JSON OAuth client
json_path <- system.file("app/json/shareibc.json", package = "shareIBC")
# designate project-specific cache
secrets_path <- system.file("/.secrets", package = "shareIBC")

# Grant permission to googledrive
googledrive::drive_auth_configure(path = json_path)
# googledrive::drive_auth(cache = secrets_path)
googledrive::drive_auth(cache = secrets_path, email = TRUE)
# Googlesheets authentication
googlesheets4::gs4_auth(token = googledrive::drive_token())

# Gmail authentication
gmailr::gm_auth_configure(path = json_path)
# Authenticate with the tokens in the copied cache
# gmailr::gm_auth(cache = secrets_path)
gmailr::gm_auth(cache = secrets_path, email = TRUE)

# App URL
app_url <- "http://127.0.0.1:3838"
