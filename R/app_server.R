#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @noRd
app_server <- function( input, output, session ) {
  # Limit input files to 10 MB
  options(shiny.maxRequestSize = 10*1024^2)
  
  # Load server modules
  mod_post_server("post_ui")
  mod_share_server("share_ui")
  mod_request_server("request_ui")
  mod_about_server("about_ui")
  # mod_more_server("more_ui")
  mod_approval_server("approval_ui")
  
  # Show search and post buttons just in the share tab
  observeEvent(input[["main_tabset"]], {
    if (input[["main_tabset"]] == "Share") {
      # Add page-with-subnavbar class to show subnavbar panel
      shinyjs::runjs(
        "var element = document.querySelector('.page');
          element.classList.add('page-with-subnavbar');"
      )
    } else {
      # Remove page-with-subnavbar class to hide subnavbar panel
      shinyjs::runjs(
        "var element = document.querySelector('.page');
          element.classList.remove('page-with-subnavbar');"
      )
    }
  })
}
