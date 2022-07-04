#' info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Tab f7Icon
mod_more_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "More",
    icon = f7Icon("ellipsis_circle_fill"),
    f7Accordion(
      id = ns("more_accordion"),
      f7AccordionItem(
        title = "Subscribe",
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
      ),
      f7AccordionItem(
        title = "Contact us",
        f7Block(
          tags$p(
            "Please contact us by sending an email to",
            tags$a(href = "", "socialministry@ibcmadrid.com.")
          ),
          tags$p(
            "For more information, please visit the",
            tags$a(
              href = "https://ibcmadrid.com/",
              class = "link external",
              "official church's website."
            )
          )
        )
      ),
      f7AccordionItem(
        title = "About this app",
        app_sys('app/more_html/howto.html') %>%
          includeHTML() %>% f7Block()
      )
    )
  )
}

#' info Server Functions
#'
#' @noRd 
mod_more_server <- function(id){
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
          tags$p("*Mandatory fields", style = "color:red;"),
          f7Text(
            inputId = ns("name_subs"),
            label = with_red_star("Name"),
            placeholder = "Your name"
          ),
          f7Text(
            inputId = ns("email_subs"),
            label = with_red_star("E-mail"),
            placeholder = "Your e-mail"
          ),
          br(),
          formCheckBoxGroup(
            inputId = ns("ml_subs"),
            label = "Choose the mailing list(s) you want to subscribe:",
            choices = c("Job opportunities" = "jobs",
                        "Offer your services" = "services",
                        "Upcycle and donate" = "upcycle"),
            selected = c("jobs", "services", "upcycle")
          ),
          br(),
          f7Button(
            inputId = ns("subscribeBtn"),
            label = "Subscribe",
            rounded = TRUE,
            size = "small"
          )
        )
      } else {
        tagList(
          "Thank you so much for being part of our community.
          We hope to see you soon!",
          tags$p("*Mandatory fields", style = "color:red;"),
          f7Text(
            inputId = ns("email_unsubs"),
            label = with_red_star("E-mail"),
            placeholder = "Your e-mail"
          ),
          br(),
          formCheckBoxGroup(
            inputId = ns("ml_unsubs"),
            label = "Choose the mailing list(s) you want to unsubscribe:",
            choices = c("Job opportunities" = "jobs",
                        "Offer your services" = "services",
                        "Upcycle and donate" = "upcycle"),
            selected = c("jobs", "services", "upcycle")
          ),
          br(),
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
      f7ShowPreloader(color = "blue")
      shinyjs::hide("error")
      on.exit({
        shinyjs::enable("subscribeBtn")
        f7HidePreloader()
      })
      
      # Send email (show an error message in case of error)
      tryCatch({
        out <- add_email(input$name_subs, input$email_subs, input$ml_subs)
        if (out$success) {
          # Succsessful operation
          f7Dialog(
            session = session,
            title = "Done",
            text = "You have successfully updated your preferences!",
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
        shinyjs::html("error", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")      
        shinyjs::logjs(err)
      })
    })
    
    # UNSUBSCRIPTION SERVER-----
    # Unsubscription confirmation mail
    observeEvent(input$unsubscribeBtn, {
      # User-experience stuff
      shinyjs::disable("unsubscribeBtn")
      f7ShowPreloader(color = "blue")
      shinyjs::hide("error_un")
      on.exit({
        shinyjs::enable("unsubscribeBtn")
        f7HidePreloader()
      })
      
      # Send email (show an error message in case of error)
      tryCatch({
        out <- remove_email(input$email_unsubs, input$ml_unsubs)
        if (out$success) {
          f7Dialog(
            session = session,
            title = "Done",
            text = "You have successfully updated your preferences!",
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
        shinyjs::html("error_un", err$message)
        shinyjs::show(id = "error_un", anim = TRUE, animType = "fade")      
        shinyjs::logjs(err)
      })
    })
    
    # Enable buttons when all mandatory fields are filled out----
    observe({
      shinyjs::toggleState(
        id = "subscribeBtn",
        isTruthy(input$name_subs) & isTruthy(input$email_subs) & isTruthy(input$ml_subs))
      shinyjs::toggleState(
        id = "unsubscribeBtn",
        isTruthy(input$email_unsubs) & isTruthy(input$ml_unsubs))
    })
  })
}

## To be copied in the UI
# mod_more_ui("more_ui")

## To be copied in the server
# mod_more_server("more_ui")
