#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @noRd
app_server <- function( input, output, session ) {
  # Load server modules
  mod_home_server("home_ui")
  mod_share_server("share_ui")
  mod_request_server("request_ui")
  mod_donate_server("donate_ui")
  
  # Approve Tab----
  # Show approval tab
  observe({
    query <- parseQueryString(session$clientData$url_search)
    req(query[['tab']])
    if (query[['tab']] == 'approval') {
      shinyMobile::updateF7Tabs(session = session, id = 'main_tabset', selected = 'Approve')
      shinyjs::runjs("$('.toolbar').css('visibility', 'hidden');")
    }
  })
  # Reactive title
  output$approve_title <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    req(query[['id_request']])
    paste("The", query[['id_request']], "requires your approval")
  })
  
  # Request is approved
  observeEvent(input$request_approve,{
    # User-experience stuff
    shinyjs::disable("request_approve")
    f7ShowPreloader(color = "blue")
    shinyjs::hide("error_approve")
    on.exit({
      shinyjs::enable("request_approve")
      f7HidePreloader()
    })
    tryCatch({
      
    },
    error = function(err) {
      shinyjs::html("error_approve", err$message)
      shinyjs::show(id = "error_approve", anim = TRUE, animType = "fade")      
      shinyjs::logjs(err)
    })
  })
  
  
}
