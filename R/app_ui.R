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
      title = "Share IBC",
      allowPWA = TRUE,
      options = list(
        theme = "auto",
        dark = FALSE,
        filled = TRUE,
        pullToRefresh = TRUE
      ),
      # Waiting background before the UI is ready to be shown
      waiter::waiter_preloader(
        html = waiter::spin_1(),
        color = "#D14D42",
        image = "www/IBC_logo_intro2.png",
        fadeout = TRUE
      ),
      f7TabLayout(
        navbar = f7Navbar(
          title = "Social Ministry IBC Madrid",
          hairline = TRUE,
          shadow = TRUE,
          transparent = FALSE,
          subNavbar = f7SubNavbar(
            f7Searchbar(
              id = "search_post",
              inline = TRUE,
              options = 
                list(
                  searchContainer = '.shiny-html-output', #Posts container
                  searchItem = ".card", # CSS selector unit
                  searchIn = c('.card-subject', '.card-content') # where to search
                )
            ),
            mod_post_ui("post_ui")
          )
        ),
        f7Tabs(
          id =  "approval_ui-main_tabset", # name set to avoid name issues with approval module
          swipeable = TRUE,
          animated = FALSE,
          mod_share_ui("share_ui"),
          mod_request_ui("request_ui"),
          mod_about_ui("about_ui"),
          mod_info_ui("info_ui"),
          mod_approval_ui("approval_ui")
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
      app_title = 'shareIBC'
    ),
    # Add here other external resources
    shinyjs::useShinyjs(),
    waiter::use_waiter()
    # for example, you can add shinyalert::useShinyalert() 
  )
}

