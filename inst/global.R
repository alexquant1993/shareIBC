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

# App data
app_url <- "http://127.0.0.1:3838"
gmail_account <- "Share IBC <share.ibcmadrid@gmail.com>"
ibc_url <- "https://ibcmadrid.com/"
ibc_building_url <- "https://lh3.googleusercontent.com/d/1nbW-IYdtXoqzBosvgMmR5MY1Ij4faZph"
ibc_logo_url <- "https://lh3.googleusercontent.com/d/1-ojsCffeIsTovPBTBC_SKPP8CL76SoHL"