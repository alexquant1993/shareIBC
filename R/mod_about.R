#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "About us",
    icon = f7Icon("person_3_fill"),
    f7Accordion(
      id = ns("about_accordion"),
      f7AccordionItem(
        title = "About IBC Madrid",
        app_sys('app/about_html/about_ibc.html') %>%
          includeHTML() %>% f7Block()
      ),
      f7AccordionItem(
        title = "About Social Ministry IBC",
        f7Accordion(
          id = ns("about_sm_accordion"),
          f7AccordionItem(
            title = "Philosophy",
            app_sys('app/about_html/philosophy.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "Purpose",
            app_sys('app/about_html/purpose.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "The SM is based upon Scripture",
            app_sys('app/about_html/based_scripture.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "What do we do?",
            f7Block(
              tags$p("The Social Ministry is currently providing the
                     following list of services and activities. See the
                     Social Ministry Guidelines for more detailed information:")
            ),
            tags$ul(
              style = 
                "list-style: disc;
                  padding-left: calc(var(--f7-list-item-padding-horizontal) + var(--f7-list-in-list-padding-left));",
              tags$li("Financial assistance"),
              tags$li("Food program"),
              tags$li("Clothing drives"),
              tags$li("Adopt a family - Christmas Edition"),
              tags$li("Financial wisdom workshops"),
              tags$li("Career workshops")
            )
          )
        )
      ),
      f7AccordionItem(
        title = "Donate to this Ministry",
        app_sys('app/about_html/donate.html') %>%
          includeHTML() %>% f7Block()
      ),
      f7AccordionItem(
        title = "Social Ministry Guidelines",
        f7Block(
          tags$p("The following document defines the purpose of the Social
                 Ministry (SM) and outlines the conditions for which Immanuel
                 Baptist Church (IBC) may authorize and disburse funds, food,
                 clothing, consultation, and training that are classified as
                 benevolence. It also outlines the process and guidelines for
                 executing the duties of the Social Ministry Committee (SMC)
                 and the Social Ministry Team (SMT).")
        ),
        f7DownloadButton(
          ns("sm_guide"),
          "SM Guidelines"
        )
      )
    )
  )
}
    
#' about Server Functions
#'
#' @noRd 
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Get files' IDs
    forms <- drive_ls("forms")
    sm_guide_name <- "Social Ministry Guidelines_vf0.pdf"
    id_sm_guide <- subset(forms, name == sm_guide_name)$id
    
    # Download handlers
    output$sm_guide <- downloadHandler(
      filename = sm_guide_name,
      content = function(file) {
        drive_download(id_sm_guide, file)
      }
    )
  })
}
    
## To be copied in the UI
# mod_about_ui("about_ui")
    
## To be copied in the server
# mod_about_server("about_ui")
