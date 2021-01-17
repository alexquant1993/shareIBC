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
      called to do the same with others. Thank you for sharing!"
    ),
    formList(
      formText(
        inputId = ns("name_poster"),
        label = with_red_star("Name"),
        placeholder = "Your name"
      ),
      formText(
        inputId = ns("name_subs"),
        label = with_red_star("E-mail"),
        placeholder = "Your e-mail"
      ),
      formSelect(
        ns("type_post"),
        label = with_red_star("Type of post"),
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
        inputId = ns("contact_mail"),
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
        inputId = ns("upload_post"),
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
    # f7Block(
    #   strong = TRUE,
    #   # iframe Post HTML form (GoogleForms + FormFacade + PerformFlow)
    #   # Googleforms: fill out form online
    #   # FormFacade: add-on to customize and embed googleforms within your website
    #   # PerformFlow: add-on to allow an approval process
    #   # HTML("<iframe src='https://formfacade.com/headless/111562879866341271998/home/form/1FAIpQLSe1yBR-Cd-Agiyw1BVUaIRmptCxNo0BCE4usi6jOsOLWb9Jhw' scrolling='no' frameBorder='0' width='100%' style='height:400px; /*change height as required*/ overflow-y:hidden;'></iframe>")
    #   # HTML("<iframe src='https://formfacade.com/headless/111562879866341271998/home/form/1FAIpQLSe1yBR-Cd-Agiyw1BVUaIRmptCxNo0BCE4usi6jOsOLWb9Jhw' scrolling='no' frameBorder='0' width='100%' style='height:400px; /*change height as required*/ overflow-y:hidden;'></iframe>")
    #   # HTML("<iframe src='https://formfacade.com/headless/111562879866341271998/home/form/1FAIpQLSe1yBR-Cd-Agiyw1BVUaIRmptCxNo0BCE4usi6jOsOLWb9Jhw' scrolling='no' frameBorder='0' width='100%' style='height:400px; /*change height as required*/ overflow-y:hidden;'></iframe>")
    #   # HTML("<iframe src='https://docs.google.com/forms/d/e/1FAIpQLSe1yBR-Cd-Agiyw1BVUaIRmptCxNo0BCE4usi6jOsOLWb9Jhw/viewform?embedded=true' frameBorder='0' width='100%' style='height:400px; /*change height as required*/ overflow-y:hidden;'></iframe>")
    #   # HTML("<iframe src='https://docs.google.com/forms/d/e/1FAIpQLSe1yBR-Cd-Agiyw1BVUaIRmptCxNo0BCE4usi6jOsOLWb9Jhw/viewform?embedded=true' frameBorder='0' width='100%' style='height:400px; /*change height as required*/ overflow-y:hidden;'>Loading…</iframe>")
    # )
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
    # Only enable the upload buttons when their corresponding input has a file selected
    # observe({
    #   fields_filled_post <-
    #     post_names %>%
    #     sapply(function(x) !is.null(input[[x]]) && input[[x]] != "") %>%
    #     all
    #   shinyjs::toggleState("loadFileBtn", fields_filled_post)
    # })
    
  })
}

## To be copied in the UI
# mod_share_post_ui("share_post_ui")

## To be copied in the server
# mod_share_post_server("share_post_ui")
