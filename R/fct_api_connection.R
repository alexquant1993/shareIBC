#' Connect to Google APIs
#'
#' @description This function is executed before the app is run. It allows the
#' connection to the Google API for: googledrive, googlesheets and gmail
#' @param env type of environment to run the application. 
#' @noRd
ApiConnections <- function(env = c("default", "production")){
  # Type of configuration, choose between: default (development) and production
  env <- match.arg(env)
  Sys.setenv("R_CONFIG_ACTIVE" = env)
  
  # Authentication process - connection to GOOGLE APIS
  secret_can_decrypt <- utils::getFromNamespace("secret_can_decrypt", "gargle")
  secret_read <- utils::getFromNamespace("secret_read", "gargle")
  
  if (secret_can_decrypt("shareIBC")) {
    # JSON OAuth app
    json_path <- secret_read("shareIBC", "shareIBC_OAuth.json")
    # Set gargle options
    options(
      # Designate project-specific cache
      gargle_oauth_cache = system.file("/secret/tokens", package = "shareIBC"),
      # OAuth email
      gargle_oauth_email = get_golem_config("gmail_account")
    )
    # Grant permission to googledrive, googlesheets4 and gmailr
    # Googledrive
    googledrive::drive_auth_configure(path = rawToChar(json_path))
    googledrive::drive_auth()

    # Googlesheets authentication
    googlesheets4::gs4_auth()
    # Gmail authentication
    gmailr::gm_auth_configure(path = rawToChar(json_path))
    gmailr::gm_auth(email = get_golem_config("gmail_account"))
  } 
}


