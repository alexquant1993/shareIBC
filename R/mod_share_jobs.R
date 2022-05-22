#' share_jobs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_share_jobs_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Jobs",
    icon = f7Icon("briefcase_fill"),
    uiOutput(ns("posts"))
  )
}

#' share_jobs Server Functions
#'
#' @noRd 
mod_share_jobs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    rv_data <- reactiveValues()
    observe({
      data <- GetPanelData()
      rv_data$data_jobs <- data$jobs
      rv_data$data_services <- data$services
    })
  })
}

## To be copied in the UI
# mod_share_jobs_ui("share_jobs_ui_1")

## To be copied in the server
# mod_share_jobs_server("share_jobs_ui_1")
