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
  
  # Extract internal functions from packages to avoid ::: R CMD Check warning
  secret_can_decrypt <- utils::getFromNamespace("secret_can_decrypt", "gargle")
  secret_read <- utils::getFromNamespace("secret_read", "gargle")
  .auth <- utils::getFromNamespace(".auth", "gmailr")
  
  # Authentication process - connection to GOOGLE APIS
  if (secret_can_decrypt("shareIBC")) {
    # JSON OAuth app - service account encrypted token
    json_path <- secret_read("shareIBC", "shareibc_service_account.json")
    # Grant permission to googledrive, googlesheets4 and gmailr
    # Googledrive authentication
    googledrive::drive_auth(path = rawToChar(json_path))
    # Googlesheets authentication
    googlesheets4::gs4_auth(path = rawToChar(json_path))
    # Gmail authentication
    # Get token given the service account credentials
    token <- gargle::credentials_service_account(
      scopes = "https://mail.google.com/", # Full scope
      path = rawToChar(json_path), # Encrypted 
      subject = get_golem_config("gmail_account") # Impersonate service account
    )
    # Assign token to .auth internal object from gmailr package
    assign("cred", token, .auth)
  }
}
