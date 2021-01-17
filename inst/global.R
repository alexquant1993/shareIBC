# Grant permission to googledrive
# designate project-specific cache
secrets_path <- system.file("/.secrets", package = "socialministryapp")
options(gargle_oauth_cache = secrets_path)
# googledrive::drive_auth()
# googlesheets4::gs4_auth(token = googledrive::drive_token())
# options(gargle_quiet = FALSE)
# gargle::gargle_oauth_cache()
googledrive::drive_auth(cache = secrets_path, email = "jobs.ibcmadrid@gmail.com")
googlesheets4::gs4_auth(token = googledrive::drive_token())