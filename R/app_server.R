#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_home_server("home_ui")
  mod_share_server("share_ui")
  mod_request_server("request_ui")
  mod_donate_server("donate_ui")
}
