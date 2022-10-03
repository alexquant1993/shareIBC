# Load library
library(shinytest2)

# Auxiliary functions
# Skip test if there is no valid token
skip_if_no_token <- function() {
  testthat::skip_if_not(
    googledrive::drive_has_token() &
      googlesheets4::gs4_has_token() &
      gmailr::gm_has_token(), "No token")
}
# Get function from gargle package in order to avoid ::: R CMD check warning
secret_can_decrypt <- utils::getFromNamespace("secret_can_decrypt", "gargle")

# Relevant variables
testing_email <- 
  get_golem_config("testing_email", config = "default")
type_of_posts <- c("jobs", "services", "upcycle", "mix")

# Connect to APIs
ApiConnections("default")