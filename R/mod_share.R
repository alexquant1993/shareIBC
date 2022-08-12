#' share UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom htmltools HTML
mod_share_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Share",
    icon = f7Icon("arrowshape_turn_up_right_circle"),
    active = TRUE,
    f7Tabs(
      id = ns("share_tabset"),
      style = "strong",
      animated = TRUE,
      swipeable = FALSE,
      mod_share_tabs_ui(
        ns("jobs"),
        "Jobs",
        "briefcase_fill",
        "Job Opportunities"
      ),
      mod_share_tabs_ui(
        ns("services"),
        "Services",
        "person_2_square_stack",
        "Professional Services"
      ),
      mod_share_tabs_ui(
        ns("upcycle"),
        "Upcycle",
        "gift_fill",
        "Upcycle and Donate"
      ),
      mod_share_tabs_ui(
        ns("mix"),
        "Miscellaneous",
        "burst_fill",
        "Miscellaneous"
      )
    )
  )
}

#' share Server Functions
#' @noRd 
mod_share_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Load Post Data
    ls_posts <- GetPostData()
    dt_jobs <- ls_posts$jobs
    dt_services <- ls_posts$services
    dt_upcycle <- ls_posts$upcycle
    dt_mix <- ls_posts$mix
    
    # Posts tabs server side
    mod_share_tabs_server("jobs", dt_jobs)
    mod_share_tabs_server("services", dt_services)
    mod_share_tabs_server("upcycle", dt_upcycle)
    mod_share_tabs_server("mix", dt_mix)
    
  })
}

## To be copied in the UI
# mod_share_ui("share_ui")

## To be copied in the server
# mod_share_server("share_ui")
