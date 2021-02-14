#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    f7Page(
      title = "Social Ministry IBC",
      init = f7Init(
        skin = "auto",
        theme = "light",
        filled = TRUE,
        color = "pink"
      ),
      f7TabLayout(
        navbar = f7Navbar(
          title = "Social Ministry IBC",
          hairline = TRUE,
          shadow = TRUE,
          transparent = FALSE
        ),
        f7Tabs(
          id =  "main_tabset",
          swipeable = TRUE,
          animated = FALSE,
          mod_home_ui("home_ui"),
          mod_share_ui("share_ui"),
          mod_request_ui("request_ui"),
          mod_donate_ui("donate_ui"),
          f7Tab(
            tabName = "Approve",
            hidden = TRUE,
            f7BlockTitle(title = "Approval Request", size = "large") %>% f7Align(side = "center"),
            h4(textOutput("approve_title")) %>% f7Align(side = "center"),
            f7TextArea(
              inputId = "comment_approve",
              label = "Your comment",
              placeholder = "Your comment here",
              resize = TRUE
            ),
            f7Segment(
              rounded = TRUE,
              container = "segment",
              f7Button(label = "Approve", inputId = "request_approve"),
              f7Button(outline = TRUE, fill = FALSE, label = "Reject", inputId = "request_reject")
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'socialministryapp'
    ),
    # Add here other external resources
    shinyjs::useShinyjs()
    # for example, you can add shinyalert::useShinyalert() 
  )
}

