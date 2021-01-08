#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_home_server("home_ui_1")
  mod_share_server("share_ui_1")
  mod_request_server("request_ui_1")
  mod_donate_server("donate_ui_1")
}
