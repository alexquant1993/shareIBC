#' subscription UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_subscription_ui <- function(id){
  ns <- NS(id)
  tagList(
    f7Accordion(
      id = ns("subs_accordion"),
      f7AccordionItem(
        title = "Subscribe",
        open = TRUE,
        f7Card(
          title = strong("Subscribe to Share IBC Mailing List"),
          tags$p(
            "Subscribe to the mailing lists of your choice, to receive 
              automatic emails every time a post is made on ShareIBC."
          ),
          tags$p(
            tags$strong(
              "If you do not receive our mails in your inbox, please check your
              spam folder and mark our emails as 'Not Spam'.")
          ),
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
          actionButton(
            inputId = ns("subscribeBtn"),
            label = "Subscribe",
            class = "button button-fill button-round button-small"
          )
        )
      ),
      f7AccordionItem(
        title = "Unsubscribe",
        f7Card(
          title = strong("Unsubscribe to Share IBC Mailing List"),
          tags$p(
            "Thank you so much for being part of our community.
            We hope to see you soon!"
          ),
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
          actionButton(
            inputId = ns("unsubscribeBtn"),
            label = "Unsubscribe",
            class = "button button-fill button-round button-small"
          )
        )
      )
    )
  )
}

#' subscription Server Functions
#'
#' @noRd 
mod_subscription_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Redirect to unsubscribe tab, if requested
    observe({
      query <- parseQueryString(session$clientData$url_search)
      req(query[['unsubscribe']])
      if (query[['unsubscribe']] == 'TRUE') {
        # Open subscription popup
        shinyjs::runjs("app.popup.open('#post_ui-popup_subs')")
        # Open accordion item in the Subscribe tab
        # Number 1, because it is the second accordion item
        shinyjs::runjs(
          "var item = $('#post_ui-subscription-subs_accordion .accordion-item')[1];
           app.accordion.toggle($(item));"
        )
      }
    })
    
    # Enable buttons when all mandatory fields are filled out
    observe({
      shinyjs::toggleState(
        id = "subscribeBtn",
        isTruthy(input$name_subs) & isTruthy(input$email_subs)
        & isTruthy(input$ml_subs)
      )
      shinyjs::toggleState(
        id = "unsubscribeBtn",
        isTruthy(input$email_unsubs) & isTruthy(input$ml_unsubs)
      )
    })
    
    # Create hostess and waiter loading helpers
    hostess <- waiter::Hostess$new(infinite = TRUE)
    waiter <-
      waiter::Waiter$new(
        html = 
          hostess$get_loader(
            fill_color = "#FFFFFF",
            svg = "www/shareibc_hex.svg",
            progress_type = "fill",
            fill_direction = "ttb",
            center_page = TRUE
          ),
        color = "#D14D42",
        fadeout = 1000
      )
    # Subscription process (send confirmation email)
    observeEvent(input$subscribeBtn, {
      # User-experience stuff
      shinyjs::disable("subscribeBtn")
      waiter$show()
      hostess$start()
      # showF7Preloader(color = "blue")
      on.exit({
        shinyjs::enable("subscribeBtn")
        hostess$close()
        waiter$hide()
        # hideF7Preloader()
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
          title = "Done",
          text = "You have successfully updated your preferences!",
          type = "alert",
          session = session
        )
      } else {
        # Unsuccsesful operation
        f7Dialog(
          title = "Error",
          text = check_subs$Ops.error,
          type = "alert",
          session = session
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
          title = "Done",
          text = "You have successfully updated your preferences!",
          type = "alert",
          session = session
        )
      } else {
        # Unsuccsesful operation
        f7Dialog(
          title = "Error",
          text = check_unsubs$Ops.error,
          type = "alert",
          session = session
        )
      }
    })
    
  })
}

## To be copied in the UI
# mod_subscription_ui("subscription_1")

## To be copied in the server
# mod_subscription_server("subscription_1")
