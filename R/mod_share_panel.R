#' share_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_share_panel_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Panel",
    icon = f7Icon("rectangle_stack_fill"),
    active = TRUE,
    f7Select(
      ns("type_post_panel"),
      label = "Choose a type of offer",
      choices = c("Job offers" = "jobs",
                  "Services offerings" = "services"),
      selected = "jobs"
    ),
    conditionalPanel(
      "input.type_post_panel == 'jobs'",
      uiOutput(ns("post_jobs")),
      ns = ns
    ),
    conditionalPanel(
      "input.type_post_panel == 'services'",
      uiOutput(ns("post_services")),
      ns = ns
    ),
    br(),
    f7Button(
      inputId = ns("refresh_panel"),
      label = inlineBlock(icon = "goforward", "Refresh panels"),
      rounded = TRUE
    ),
    br()
  )
}

#' share_panel Server Functions
#'
#' @noRd 
mod_share_panel_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Reactive data
    rv_data <- reactiveValues()
    # Update the panel data (add new posts to the UI)
    # When the app starts, it loads the most recent data (ignoreNULL)
    observeEvent(input$refresh_panel,{
      req(!is.null(input$refresh_panel))
      data <- GetPanelData()
      rv_data$data_jobs <- data$jobs
      rv_data$data_services <- data$services
    }, ignoreNULL = FALSE)
    
    # Create UI Cards in a loop
    # Post Jobs
    output$post_jobs <- renderUI({
      lapply(1:nrow(rv_data$data_jobs), function(i){
        date <- rv_data$data_jobs$`Marca temporal`[i]
        subject <- rv_data$data_jobs$`Enter the subject of your post`[i]
        description <- rv_data$data_jobs$`Offer a brief description of the post`[i]
        phone_number <- replaceTruthy(rv_data$data_jobs$`Enter contact information - phone number`[i])
        email <- replaceTruthy(rv_data$data_jobs$`Enter contact information - email`[i])
        footer <-
          tagList(
            f7Row(
              f7Col(
                inlineBlock(icon = "envelope", email)
              ),
              f7Col(
                inlineBlock(icon = "phone", phone_number)
              )
            ),
            br(),
            f7Button(
              inputId = ns(paste0("jobs_", i)),
              label =
                inlineBlock(icon = "photo_fill_on_rectangle_fill", "See pictures"),
              rounded = TRUE
            )
          )
        f7Post(subject = subject, date = date, description, footer = footer)
      })
    })
    output$post_services <- renderUI({
      lapply(1:nrow(rv_data$data_services), function(i){
        date <- rv_data$data_services$`Marca temporal`[i]
        subject <- rv_data$data_services$`Enter the subject of your post`[i]
        description <- rv_data$data_services$`Offer a brief description of the post`[i]
        phone_number <- replaceTruthy(rv_data$data_services$`Enter contact information - phone number`[i])
        email <- replaceTruthy(rv_data$data_services$`Enter contact information - email`[i])
        footer <-
          tagList(
            f7Row(
              f7Col(
                inlineBlock(icon = "envelope", email)
              ),
              f7Col(
                inlineBlock(icon = "phone", phone_number)
              )
            ),
            br(),
            f7Button(
              inputId = ns(paste0("services_", i)),
              label =
                inlineBlock(icon = "photo_fill_on_rectangle_fill", "See pictures"),
              rounded = TRUE
            )
          )
        f7Post(subject = subject, date = date, description, footer = footer)
      })
    })
  })
}

## To be copied in the UI
# mod_share_panel_ui("share_panel_ui")

## To be copied in the server
# mod_share_panel_server("share_panel_ui")
