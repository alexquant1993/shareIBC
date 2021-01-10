#' share UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom htmltools HTML
mod_share_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Share",
    icon = f7Icon("person_3_fill"),
    f7Tabs(
      id = ns("share_tabset"),
      style = "strong",
      animated = TRUE,
      swipeable = FALSE,
      mod_share_panel_ui(ns("share_panel_ui")),
      mod_share_post_ui(ns("share_post_ui")),
      mod_share_subscribe_ui(ns("share_subscribe_ui"))
    )
  )
}

#' share Server Functions
#' @noRd 
mod_share_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_share_panel_server("share_panel_ui")
    mod_share_post_server("share_post_ui")
    mod_share_subscribe_server("share_subscribe_ui")
  })
}

## To be copied in the UI
# mod_share_ui("share_ui")

## To be copied in the server
# mod_share_server("share_ui")
