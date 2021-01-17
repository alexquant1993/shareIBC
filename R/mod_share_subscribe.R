#' share_subscribe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_share_subscribe_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Subscribe",
    icon = f7Icon("at_circle_fill"),
    f7Card(
      title = strong(textOutput(ns("title_subscriptionUI"))),
      uiOutput(ns("subscriptionUI")),
      shinyjs::hidden(
        span(id = ns("subscribeMsg"), "Submitting...", style = "margin-left: 15px;")
      ),
      br(),
      f7Toggle(
        inputId = ns("unsubscribe"),
        label = "Unsubscribe?",
        checked = FALSE
      )
    )
  )
}

#' share_subscribe Server Functions
#'
#' @noRd 
mod_share_subscribe_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Reactive title
    output$title_subscriptionUI <- renderText({
      if (!input$unsubscribe) {
        "Subscribe to Share IBC Mailing List"
      } else {
        "Unsubscribe to Share IBC Mailing List"
      }
    })
    # Reactive UI
    output$subscriptionUI <- renderUI({
      if (!input$unsubscribe) {
        tagList(
          "The objective of Share IBC is to help members of IBC and those within
          our community to get connected and share different opportunities.",
          f7Text(inputId = ns("name_subs"), label = with_red_star("Enter your name")),
          f7Text(inputId = ns("email_subs"), label = with_red_star("Enter your email")),
          f7Button(
            inputId = ns("subscribeBtn"),
            label = "Subscribe",
            rounded = TRUE,
            size = "small"
          )
        )
      } else {
        tagList(
          "Thank you so much for being part of our community. We hope to see you soon!",
          f7Text(inputId = ns("email_unsubs"), label = with_red_star("Enter your email")),
          f7Button(
            inputId = ns("unsubscribeBtn"),
            label = "Unsubscribe",
            rounded = TRUE,
            size = "small"
          )
        )
      }
    })
    
    # Subscription process (send confirmation email)----
    observeEvent(input$subscribeBtn, {
      # User-experience stuff
      shinyjs::disable("subscribeBtn")
      shinyjs::show("subscribeMsg")
      shinyjs::hide("error")
      on.exit({
        shinyjs::enable("subscribeBtn")
        shinyjs::hide("subscribeMsg")
      })
      
      # Send email (show an error message in case of error)
      tryCatch({
        out <- add_email(input$name_subs, input$email_subs)
        if (out$success) {
          # Succsessful operation
          f7Dialog(
            session = session,
            title = "Done",
            text = "You are now subscribed to the IBC mailing list.",
            type = "alert"
          )
        } else {
          # Unsuccsesful operation
          f7Dialog(
            session = session,
            title = "Error",
            text = out$Ops.error,
            type = "alert"
          )
        }
        updateF7Text("name_subs", value = "")
        updateF7Text("email_subs", value = "")
      },
      error = function(err) {
        shinyjs::html("errorMsg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")      
        shinyjs::logjs(err)
      })
    })
    
    # UNSUBSCRIPTION SERVER-----
    # Unsubscription confirmation mail
    observeEvent(input$unsubscribeBtn, {
      # User-experience stuff
      shinyjs::disable("unsubscribeBtn")
      shinyjs::show("subscribeMsg")
      shinyjs::hide("error_un")
      on.exit({
        shinyjs::enable("unsubscribeBtn")
        shinyjs::hide("subscribeMsg")
      })
      
      # Send email (show an error message in case of error)
      tryCatch({
        out <- remove_email(input$email_unsubs)
        if (out$success) {
          f7Dialog(
            session = session,
            title = "Done",
            text = "You are now unsubscribed to the IBC mailing list.",
            type = "alert"
          )
        } else {
          f7Dialog(
            session = session,
            title = "Error",
            text = out$Ops.error,
            type = "alert"
          )
        }
        updateF7Text("email_unsubs", value = "")
      },
      error = function(err) {
        shinyjs::html("errorMsg", err$message)
        shinyjs::show(id = "error_un", anim = TRUE, animType = "fade")      
        shinyjs::logjs(err)
      })
    })
    
    # Enable buttons when all mandatory fields are filled out----
    observe({
      shinyjs::toggleState(id = "subscribeBtn",
                           isTruthy(input$name_subs) & isTruthy(input$email_subs))
      shinyjs::toggleState(id = "unsubscribeBtn", isTruthy(input$email_unsubs))
    })
  })
}

## To be copied in the UI
# mod_share_subscribe_ui("share_subscribe_ui")

## To be copied in the server
# mod_share_subscribe_server("share_subscribe_ui")
