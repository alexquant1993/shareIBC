#' request UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_request_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Request",
    icon = f7Icon("hand_raised_fill"),
    "Requests tab"
  )
}

#' request Server Functions
#'
#' @noRd 
mod_request_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_request_ui("request_ui_1")

## To be copied in the server
# mod_request_server("request_ui_1")
