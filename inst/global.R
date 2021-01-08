# Grant permission to googledrive
# designate project-specific cache
options(gargle_oauth_cache = paste0(app_sys(""),"/.secrets"))
# googledrive::drive_auth()
# googlesheets4::gs4_auth(token = googledrive::drive_token())
# options(gargle_quiet = FALSE)
# gargle::gargle_oauth_cache()
googledrive::drive_auth(cache = paste0(app_sys(""),"/.secrets"), email = "jobs.ibcmadrid@gmail.com")
googlesheets4::gs4_auth(token = googledrive::drive_token())