#' share UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
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
      f7Tab(
        tabName = "Panel",
        icon = f7Icon("rectangle_stack_fill"),
        active = TRUE,
        f7Block(
          strong = TRUE,
          "Panel Tab"
        )
      ),
      f7Tab(
        tabName = "Post",
        icon = f7Icon("cloud_upload_fill"),
        f7Block(
          strong = TRUE,
          # iframe Post HTML form (GoogleForms + FormFacade + PerformFlow)
          HTML("<iframe src='https://formfacade.com/headless/111562879866341271998/home/form/1FAIpQLSe1yBR-Cd-Agiyw1BVUaIRmptCxNo0BCE4usi6jOsOLWb9Jhw' scrolling='no' frameBorder='0' width='100%' style='height:400px; /*change height as required*/ overflow-y:hidden;'></iframe>")
        )
      ),
      f7Tab(
        tabName = "Subscribe",
        icon = f7Icon("at_circle_fill"),
        f7Card(
          title = strong(textOutput(ns("title_subscriptionUI"))),
          uiOutput(ns("subscriptionUI")),
          br(),
          f7Toggle(
            inputId = ns("unsubscribe"),
            label = "Unsubscribe?",
            checked = FALSE
          )
        )
      )
    )
  )
}

#' share Server Functions
#' @noRd 
mod_share_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Subscribe UI server----
    # Reactive title
    output$title_subscriptionUI <- renderText({
      if (!input$unsubscribe) {
        "Subscribe to the IBC Mailing List"
      } else {
        "Unsubscribe to the IBC Mailing List"
      }
    })
    # Reactive UI
    output$subscriptionUI <- renderUI({
      if (!input$unsubscribe) {
        tagList(
          "The objective of Share IBC is to help members of IBC and those within
          our community to get connected and share different opportunities.",
          f7Row(
            f7Col(
              f7Text(inputId = ns("name_subs"), label = with_red_star("Enter your name"))
            ),
            f7Col(
              f7Text(inputId = ns("email_subs"), label = with_red_star("Enter your email"))
            ),
            gap = FALSE
          ),
          f7Button(
            inputId = ns("subscribeBtn"),
            label = "Subscribe",
            rounded = TRUE,
            size = "small"
          ),
          shinyjs::hidden(
            span(id = ns("subscribeMsg"), "Submitting...", style = "margin-left: 15px;")
          ),
          textOutput(ns("test"))
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
          ),
          shinyjs::hidden(
            span(id = ns("unsubscribeMsg"), "Submitting...", style = "margin-left: 15px;")
          ),
          textOutput(ns("test2"))
        )
      }
    })
    
    output$test <- renderText({
      paste(!is.null(input$name_subs),input$name_subs != "", !is.null(input$email_subs),input$email_subs != "")
    })
    output$test2 <- renderText({
      paste(!is.null(input$email_unsubs),input$email_unsubs != "")
    })
    
    
    
    # Subscription process (send confirmation email)
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
        # shinyjs::reset("subscribeForm")
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
      shinyjs::show("unsubscribeMsg")
      shinyjs::hide("error_un")
      on.exit({
        shinyjs::enable("unsubscribeBtn")
        shinyjs::hide("unsubscribeMsg")
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
        # shinyjs::reset("subscribeForm")
      },
      error = function(err) {
        shinyjs::html("errorMsg", err$message)
        shinyjs::show(id = "error_un", anim = TRUE, animType = "fade")      
        shinyjs::logjs(err)
      })
    })
    
    # Enable buttons when all mandatory fields are filled out----
    # Subscription toggle
    # subscription_names <- c("name_subs", "email_subs")
    observe({
      fields_filled_subs <-
        c("name_subs", "email_subs") %>%
        sapply(function(x) !is.null(input[[x]]) && input[[x]] != "") %>%
        all
      shinyjs::toggleState("subscribeBtn", fields_filled_subs)
    })
    # Unsubscription toogle
    # unsubscription_names <- c("email_unsubs")
    observe({
      fields_filled_unsubs <- 
        c("email_unsubs") %>%
        sapply(function(x) !is.null(input[[x]]) && input[[x]] != "") %>%
        all
      shinyjs::toggleState("unsubscribeBtn", fields_filled_unsubs)
    })
  })
}

## To be copied in the UI
# mod_share_ui("share_ui_1")

## To be copied in the server
# mod_share_server("share_ui_1")
