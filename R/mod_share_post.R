#' share_post UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_share_post_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Post",
    icon = f7Icon("cloud_upload_fill"),
    f7Block(
      strong = TRUE,
      # iframe Post HTML form (GoogleForms + FormFacade + PerformFlow)
      # Googleforms: fill out form online
      # FormFacade: add-on to customize and embed googleforms within your website
      # PerformFlow: add-on to allow an approval process
      HTML("<iframe src='https://formfacade.com/headless/111562879866341271998/home/form/1FAIpQLSe1yBR-Cd-Agiyw1BVUaIRmptCxNo0BCE4usi6jOsOLWb9Jhw' scrolling='no' frameBorder='0' width='100%' style='height:400px; /*change height as required*/ overflow-y:hidden;'></iframe>")
    )
  )
}
    
#' share_post Server Functions
#'
#' @noRd 
mod_share_post_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    wb_offer <- reactive({
      drive_get("IBC_POSTS")
    })
    
    modified_time <- reactive({
      req((wb_offer())$drive_resource[[1]]$modifiedTime)
      (wb_offer())$drive_resource[[1]]$modifiedTime
    })
    observeEvent(modified_time(),{
      PostOffer(wb_offer())
    })
  })
}
    
## To be copied in the UI
# mod_share_post_ui("share_post_ui")
    
## To be copied in the server
# mod_share_post_server("share_post_ui")
