#' donate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_donate_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Donate",
    icon = f7Icon("heart_fill"),
    "Donate tab"
  )
}

#' donate Server Functions
#'
#' @noRd 
mod_donate_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_donate_ui("donate_ui_1")

## To be copied in the server
# mod_donate_server("donate_ui_1")
