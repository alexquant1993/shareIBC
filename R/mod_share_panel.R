#' share_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_share_panel_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Panel",
    icon = f7Icon("rectangle_stack_fill"),
    active = TRUE,
    f7Block(
      strong = TRUE,
      "Panel Tab"
    )
  )
}

#' share_panel Server Functions
#'
#' @noRd 
mod_share_panel_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_share_panel_ui("share_panel_ui")

## To be copied in the server
# mod_share_panel_server("share_panel_ui")
