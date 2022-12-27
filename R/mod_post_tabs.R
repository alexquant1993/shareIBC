#' post_tabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label button popup name
#' @param icon framework7 icon name
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_post_tabs_ui <- function(id, label, icon){
  ns <- NS(id)
  tagList(
    # Button that triggers the post tab
    actionButton(
      inputId = ns("bttn_post_tab"),
      label = tagList(f7Icon(icon), label),
      class = "button button-fill button-large"
    ),
    # Popup UI
    f7Popup(
      id = ns("popup_post_tab"),
      title = "Submit your post",
      uiOutput(ns("post_instr")),
      formList(
        formText(
          inputId = ns("name_poster"),
          label = with_red_star("Name"),
          placeholder = "Your name"
        ),
        formText(
          inputId = ns("email_poster"),
          label = with_red_star("E-mail"),
          placeholder = "Your e-mail"
        ),
        formText(
          inputId = ns("subject"),
          label = with_red_star("Subject"),
          placeholder = "Enter the subject of your post"
        ),
        formTextArea(
          inputId = ns("description"),
          label = with_red_star("Description"),
          placeholder = "Offer a brief description of your post. Include all relevant details."
        ),
        formText(
          inputId = ns("contact_email"),
          label = "Contact information - email",
          placeholder = "Email to contact about the post"
        ),
        formText(
          inputId = ns("contact_phone"),
          label = "Contact information - phone number",
          placeholder = "Phone to contact about the post"
        ),
        br(),
        f7File(
          inputId = ns("attach_post"),
          label = 
            div(strong("Attach files, if any"),
                helpPopup('Only image files are allowed. 10MB max size.')
            ),
          multiple = TRUE,
          accept = "image/*"
        ),
        br(),
        formCheckBox(
          inputId = ns("check_rgpd_post"),
          label = 
            HTML(
              "By using this form you agree with the storage and 
            handling of your data by this website in accordance with",
              as.character(
                actionLink(
                  inputId = ns("rgpd_link_post"),
                  label = with_red_star("our Privacy Policy.")
                )
              )
            )
        ),
        f7Popup(
          id = ns("popup_rgpd_post"),
          title = "Privacy Policy",
          includeHTML(
            app_sys("app/www/data_protection.html")
          )
        ),
        br(),
        actionButton(
          inputId = ns("submit_post"),
          label = "Submit",
          class = "button button-fill button-round"
        )
      )
    )
  )
}

#' post_tabs Server Functions
#'
#' @noRd 
mod_post_tabs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Toggle post specific section popup
    observeEvent(input$bttn_post_tab, {
      updateF7Popup(id = "popup_post_tab")
    })
    
    # Post instructions reactive UI
    output$post_instr <- renderUI({
      if (id == "jobs") {
        instr <-
          tagList(
            strong(f7BlockHeader("Share a job opportunity")),
            "Share a job opportunity with your IBC community!
            Maybe you have heard about a new vacant position in your company,
            maybe you have seen an ad walking down the street or on social
            networks, or maybe you want to offer a temporary job for a few hours,
            whatever the situation, post it here."
          )
      } else if (id == "services") {
        instr <-
          tagList(
            strong(f7BlockHeader("Offer your professional services")),
            "Are you a music or French teacher, do you have construction or
            cleaning skills, or maybe you're an excellent dog walker?
            Whatever your expertise, share it here!",
          )
      } else if (id == "upcycle") {
        instr <-
          tagList(
            strong(f7BlockHeader("Upcycle and donate")),
            "Upcycle and donate something rather than throw it away.
            Whether it's a chair, some jam jars, or an old door,
            feel free to post it. Or maybe you're looking to acquire
            something yourself. Please follow the rules below:",
            tags$ul(
              tags$li(
                strong("Free items ONLY!:"),
                "Only post items that are free or for trade. No cash exchanges."
              ),
              tags$li(
                strong("Location identification:"),
                "Indicate point of pick-up and conditions (time, day, etc)
               clearly in your post."
              )
            )
          )
      } else if (id == "mix"){
        instr <-
          tagList(
            strong(f7BlockHeader("Miscellaneous")),
            "You can put anything here that doesn't fall into the categories
          above, here are some examples:",
            tags$ul(
              tags$li(
                "Are you interested in joining a small group or connecting with
              other Christians your age?"
              ),
              tags$li(
                "Are you looking for a roommate or an apartment to rent?"
              ),
              tags$li(
                "Are you looking for someone to play football or
              paddle on weekends?"
              )
            )
          )
      }
      # Instructions block
      f7Block(
        instr,
        br(),
        strong("Request removal of your post:"),
        paste0(
          "Request removal of your post to '",
          get_golem_config("gmail_account"),
          "' once it is no longer valid. Please include details to identify it.
          Posts older than 4 months will be removed at the discretion of the 
          administration."
        ),
        tags$p("*Mandatory fields", style = "color:red;")
      )
    })
    
    # Toggle data protection pop up
    observeEvent(input$rgpd_link_post, {
      updateF7Popup(id = "popup_rgpd_post")
    })
    
    # Enable buttons when all mandatory fields are filled out
    observe({
      shinyjs::toggleState(
        id = "submit_post",
        isTruthy(input$name_poster) & isTruthy(input$email_poster) &
          isTruthy(input$subject) & isTruthy(input$description) &
          isTruthy(input$check_rgpd_post)
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
    # Upload post data into googledrive and send pre-approval email
    observeEvent(input$submit_post, {
      # User loading experience
      shinyjs::disable("submit_post")
      waiter$show()
      hostess$start()
      # showF7Preloader(color = "blue")
      on.exit({
        shinyjs::enable("submit_post")
        hostess$close()
        waiter$hide()
        # hideF7Preloader()
      })
      
      browser()
      # Upload data and send pre-approval email
      check_upload <- 
        UploadPost(
          input$name_poster,
          input$email_poster,
          type_post = id,
          input$subject,
          input$description,
          input$contact_email,
          input$contact_phone,
          input$attach_post,
          input$check_rgpd_post,
          session = session
        )
      if (check_upload$success) {
        # Clean filled data
        lapply(
          c("name_poster", "email_poster", "subject", "description",
            "contact_email", "contact_phone"),
          function(x) updateF7Text(x, value = "")
        )
        updateF7Checkbox("check_rgpd_post", value = FALSE)
        # Succsessful operation
        f7Dialog(
          session = session,
          title = "Done",
          text = "Thank you for your contribution! A confirmation email will be sent to you once your post is approved.",
          type = "alert"
        )
      } else {
        # Unsuccsesful operation
        f7Dialog(
          session = session,
          title = "Error",
          text = check_upload$Ops.error,
          type = "alert"
        )
      }
    })
  })
}

## To be copied in the UI
# mod_post_tabs_ui("post_tabs_ui")

## To be copied in the server
# mod_post_tabs_server("post_tabs_ui")
