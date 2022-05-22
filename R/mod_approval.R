#' approval UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_approval_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Approve",
    hidden = TRUE,
    f7BlockTitle(title = "Approval Request", size = "large") %>%
      f7Align(side = "center"),
    h4(textOutput("approve_title")) %>% f7Align(side = "center"),
    f7TextArea(
      inputId = ns("comment_approve"),
      label = "Your comment",
      placeholder = "Your comment here",
      resize = TRUE
    ),
    f7Segment(
      rounded = TRUE,
      container = "segment",
      f7Button(
        inputId = ns("request_approve"),
        label = "Approve"
      ),
      f7Button(
        inputId = ns("request_reject"),
        outline = TRUE,
        fill = FALSE,
        label = "Reject"
      )
    )
  )
}

#' approval Server Functions
#'
#' @noRd 
mod_approval_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Show approval tab
    observe({
      query <- parseQueryString(session$clientData$url_search)
      req(query[['tab']])
      if (query[['tab']] == 'approval') {
        shinyMobile::updateF7Tabs(
          session = session,
          id = 'main_tabset', # Not going to work due to namespace conflict.
          selected = 'Approve'
        )
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
  })
}

## To be copied in the UI
# mod_approval_ui("approval_ui")

## To be copied in the server
# mod_approval_server("approval_ui")
