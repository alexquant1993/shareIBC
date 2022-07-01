#' share_tabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param tabName Tab name within the share panel
#' @param icon framework7 icon name
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_share_tabs_ui <- function(id, tabName, icon){
  ns <- NS(id)
  f7Tab(
    tabName = tabName,
    icon = f7Icon(icon),
    uiOutput(ns("ls_posts")) %>% f7Found(),
    f7Block(
      p("Nothing found.")
    ) %>% f7NotFound()
  )
}

#' share_tabs Server Functions
#' @param dt_posts post dataframe given the type of posts 
#' @noRd 
mod_share_tabs_server <- function(id, dt_posts){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Create UI Cards in a loop
    output$ls_posts <- renderUI({
      if (nrow(dt_posts) > 0) {
        lapply(1:nrow(dt_posts), function(i){
          # Retrieve post information
          id_post <- dt_posts$ID_POST[i]
          date <- format(dt_posts$DATE_POST[i], "%B %d, %Y")
          subject <- dt_posts$SUBJECT[i]
          description <- dt_posts$DESCRIPTION[i]
          email <- replaceTruthy(dt_posts$CONTACT_EMAIL[i])
          phone_number <- replaceTruthy(dt_posts$CONTACT_PHONE[i])
          type <- dt_posts$TYPE_POST[i]
          
          # Create post footer UI
          # Conditional UI depending if there are uploaded pictures or not.
          if (!is.na(dt_posts$FILES_URL[i])) {
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
                  inputId = ns(paste0("toggle_", id_post)),
                  label =
                    inlineBlock(
                      icon = "photo_fill_on_rectangle_fill",
                      "See pictures"
                    ),
                  rounded = TRUE
                )
              )
          } else {
            footer <-
              tagList(
                f7Row(
                  f7Col(
                    inlineBlock(icon = "envelope", email)
                  ),
                  f7Col(
                    inlineBlock(icon = "phone", phone_number)
                  )
                )
              )
          }
          
          # Create Post UI
          f7Post(
            subject = subject,
            date = date,
            type = type,
            content = description,
            footer = footer
          )
        })
      }
    })
    
    # Photo browser triggers, just for the posts with pictures
    lapply(
      1:nrow(dt_posts),
      function (i){
        id_post <- dt_posts$ID_POST[i]
        # Browse pictures, when they are available in the post
        files_id <- dt_posts$FILES_URL[i]
        if (!is.na(files_id)) {
          # Split the id files
          files_id <- strsplit(files_id, ",", fixed = TRUE)[[1]]
          # Get the Google Drive public URL addresses
          files_url <-
            sapply(
              files_id,
              function (x) paste0("https://lh3.googleusercontent.com/d/", x))
          # If there is just one picture uploaded, then add an additional picture
          # to prevent an error of Framework7 (Photo Browser doesn't work with
          # more than one picture).
          if (length(files_url) == 1) {
            files_url <- 
              c(
                files_url,
                "https://lh3.googleusercontent.com/d/1nbW-IYdtXoqzBosvgMmR5MY1Ij4faZph"
              )
          }
          
          # Toggle photo browser
          observeEvent(input[[paste0("toggle_", id_post)]], {
            f7PhotoBrowser(
              theme = "dark",
              type = "page",
              photos = files_url
            )
          })
        }
      }
    )
  })
}

## To be copied in the UI
# mod_share_tabs_ui("share_tabs_ui_1")

## To be copied in the server
# mod_share_tabs_server("share_tabs_ui_1")
