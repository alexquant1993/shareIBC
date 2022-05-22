#' info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Tab f7Icon
mod_info_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Info",
    icon = f7Icon("info_circle_fill"),
    "Info tab"
  )
}
    
#' info Server Functions
#'
#' @noRd 
mod_info_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_info_ui("info_ui")
    
## To be copied in the server
# mod_info_server("info_ui")
