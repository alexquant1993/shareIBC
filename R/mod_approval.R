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
    h4(textOutput(ns("approve_title"))) %>% f7Align(side = "center"),
    f7TextArea(
      inputId = ns("comment"),
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
        updateF7Tabs(
          session = session,
          id = 'main_tabset',
          selected = 'Approve'
        )
        # Hide toolbar where the different tabs of the app are available
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
      # UX approval button  
      shinyjs::disable("request_approve")
      showF7Preloader(color = "blue")
      on.exit({
        shinyjs::enable("request_approve")
        hideF7Preloader()
      })
      
      # Retrieve data from post URL
      query <- parseQueryString(session$clientData$url_search)
      req(query[['id_request']])
      req(query[['id_approver']])
      
      # Workflow if post is approved
      check_approval <-
        ApprovePost(
          id_request = query[['id_request']],
          id_approver = query[['id_approver']],
          comment = input$comment
        )
      if (check_approval$success) {
        # Successful operation
        f7Dialog(
          id = ns("ok_approve"),
          session = session,
          title = "Done",
          text = "Approval workflow completed successfully!",
          type = "confirm"
        )
      } else {
        # Unsuccsesful operation
        f7Dialog(
          id = ns("ok_approve_error"),
          session = session,
          title = "Error",
          text = check_approval$Ops.error,
          type = "confirm"
        )
      }
      # Clear filled data
      updateF7Text("comment", value = "")
    })
    
    # Request is rejected
    observeEvent(input$request_reject,{
      # UX approval button  
      shinyjs::disable("request_reject")
      showF7Preloader(color = "blue")
      on.exit({
        shinyjs::enable("request_reject")
        hideF7Preloader()
      })
      
      # Retrieve data from post URL
      query <- parseQueryString(session$clientData$url_search)
      req(query[['id_request']])
      req(query[['id_approver']])
      
      # Workflow if post is approved
      check_rejection <-
        RejectPost(
          id_request = query[['id_request']],
          id_approver = query[['id_approver']],
          comment = input$comment
        )
      if (check_rejection$success) {
        # Successful operation
        f7Dialog(
          id = ns("ok_reject"),
          session = session,
          title = "Done",
          text = "Rejection workflow completed successfully!",
          type = "confirm"
        )
      } else {
        # Unsuccsesful operation
        f7Dialog(
          id = ns("ok_reject_error"),
          session = session,
          title = "Error",
          text = check_rejection$Ops.error,
          type = "confirm"
        )
      }
      # Clear filled data
      updateF7Text("comment", value = "")
    })
    
    # Stop App when finished and close window after 5s, if ok bttn is triggered
    lapply(
      c("ok_approve", "ok_approve_error", "ok_reject", "ok_reject_error"),
      function(x){
        observeEvent(
          input[[x]],
          {
            f7Toast(
              "The approval workflow is finished. The window will close in 5 seconds.",
              position = "center",
              closeButton = FALSE,
              closeTimeout = 5000
            )
            shinyjs::runjs("setTimeout(function(){window.close();},5000);")
          })
      }
    )
  })
}

## To be copied in the UI
# mod_approval_ui("approval_ui")

## To be copied in the server
# mod_approval_server("approval_ui")
