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
      strong(f7BlockHeader("Post a job opportunity or offer your services")),
      "God delights in His benevolent nature. He is kind, gracious, generous,
      patient, and loving. He is unrelenting in his care for us, and we are
      called to do the same with others. Thank you for sharing!",
      tags$p("*Mandatory fields", style = "color:red;")
    ),
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
      formSelect(
        ns("type_post"),
        label = with_red_star("Select type of post"),
        choices = c("Job opportunity" = "jobs",
                    "Offer your services" = "services")
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
              helpPopup('Only image files are allowed: .jpg and .png')
          ),
        multiple = TRUE,
        accept = c("image/jpeg", "image/png")
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
      f7Button(
        inputId = ns("submit_post"),
        label = "Submit",
        rounded = TRUE
      )
    )
  )
}

#' share_post Server Functions
#'
#' @noRd 
mod_share_post_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Toggle data protection pop up
    observeEvent(input$rgpd_link_post, {
      f7TogglePopup(id = "popup_rgpd_post")
    })
    
    # Enable buttons when all mandatory fields are filled out
    observe({
      shinyjs::toggleState(
        id = "submit_post",
        isTruthy(input$name_poster) & isTruthy(input$email_poster) &
          isTruthy(input$subject) & isTruthy(input$description) &
          shiny::isTruthy(input$check_rgpd_post)
      )
    })
    
    # Upload post data into googledrive and send pre-approval email
    observeEvent(input$submit_post, {
      # User loading experience
      shinyjs::disable("submit_post")
      f7ShowPreloader(color = "blue")
      shinyjs::hide("error_post")
      on.exit({
        shinyjs::enable("submit_post")
        f7HidePreloader()
      })
      
      # Upload data and send pre-approval email
      tryCatch({
        out <- 
          UploadPost(input$name_poster,
                     input$email_poster,
                     input$type_post,
                     input$subject,
                     input$description,
                     input$contact_email,
                     input$contact_phone,
                     input$attach_post,
                     input$check_rgpd_post)
        if (out$success) {
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
            text = out$Ops.error,
            type = "alert"
          )
        }
      }, error = function(err){
        shinyjs::html("error_post", err$message)
        shinyjs::show(id = "error_post", anim = TRUE, animType = "fade")      
        shinyjs::logjs(err)
      })
      
    })
    
  })
}

## To be copied in the UI
# mod_share_post_ui("share_post_ui")

## To be copied in the server
# mod_share_post_server("share_post_ui")
