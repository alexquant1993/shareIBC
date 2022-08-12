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
              href = ibc_url, class = "link external",
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
      # Subscription server side
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
                        "Upcycle and donate" = "upcycle",
                        "Miscellaneous" = "mix")
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
        # Unsubscription server side
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
                        "Upcycle and donate" = "upcycle",
                        "Miscellaneous" = "mix")
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
    
    # Subscription process (send confirmation email)
    observeEvent(input$subscribeBtn, {
      # User-experience stuff
      shinyjs::disable("subscribeBtn")
      showF7Preloader(color = "blue")
      on.exit({
        shinyjs::enable("subscribeBtn")
        hideF7Preloader()
      })
      
      # Add email to selected mailing lists and send confirmation email
      check_subs <- 
        SubscribeEmail(
          name = input$name_subs,
          email = input$email_subs,
          mailing_lists = input$ml_subs,
          session = session
        )
      if (check_subs$success) {
        # Clean filled data
        updateF7Text("name_subs", value = "")
        updateF7Text("email_subs", value = "")
        # Uncheck all boxes
        shinyjs::runjs(
          "    
          function uncheckAll() {
            var inputs = document.querySelectorAll('.checkbox_group');
            for (var i = 0; i < inputs.length; i++) {
                inputs[i].checked = false;
            }
          }
          uncheckAll()
          "
        )
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
          text = check_subs$Ops.error,
          type = "alert"
        )
      }
    })
    
    # Unsubscription process (send confirmation email)
    observeEvent(input$unsubscribeBtn, {
      # User-experience stuff
      shinyjs::disable("unsubscribeBtn")
      showF7Preloader(color = "blue")
      on.exit({
        shinyjs::enable("unsubscribeBtn")
        hideF7Preloader()
      })
      
      # Remove preferences to selected mailing lists and send confirmation email
      check_unsubs <- 
        UnsubscribeEmail(
          email = input$email_unsubs,
          mailing_lists = input$ml_unsubs,
          session = session
        )
      if (check_unsubs$success) {
        # Clean filled data
        updateF7Text("email_unsubs", value = "")
        # Uncheck all boxes
        shinyjs::runjs(
          "    
          function uncheckAll() {
            var inputs = document.querySelectorAll('.checkbox_group');
            for (var i = 0; i < inputs.length; i++) {
                inputs[i].checked = false;
            }
          }
          uncheckAll()
          "
        )
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
          text = check_subs$Ops.error,
          type = "alert"
        )
      }
    })
    
    # Enable buttons when all mandatory fields are filled out
    observe({
      shinyjs::toggleState(
        id = "subscribeBtn",
        isTruthy(input$name_subs) & isTruthy(input$email_subs)
        & isTruthy(input$ml_subs))
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
