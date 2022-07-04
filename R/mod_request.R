#' request UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_request_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "Request",
    icon = f7Icon("hand_raised_fill"),
    f7Tabs(
      id = ns("request_tabset"),
      style = "strong",
      animated = TRUE,
      swipeable = FALSE,
      f7Tab(
        tabName = "Financial",
        icon = f7Icon("money_euro_circle_fill"),
        f7BlockTitle(title = "Request Financial Assistance") %>%
          f7Align(side = "center"),
        f7Accordion(
          id = ns("finance_accordion"),
          f7AccordionItem(
            title = "Objective",
            app_sys('app/request_html/finance_objective.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "Financial Assistance Criteria",
            app_sys('app/request_html/finance_criteria.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "Application Process",
            app_sys('app/request_html/finance_application.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "Download Forms",
            f7Block(
              f7DownloadButton(
                ns("sm_ibc_a1"),
                "Request for Financial Assistance"
              ),
              br(),
              f7DownloadButton(
                ns("sm_ibc_c1"),
                "Data Protection Form"
              )
            )
          )
        )
      ),
      f7Tab(
        tabName = "Food",
        icon = f7Icon("cart_fill"),
        f7BlockTitle(title = "Request Food Assistance") %>%
          f7Align(side = "center"),
        f7Accordion(
          id = ns("food_accordion"),
          f7AccordionItem(
            title = "Objective",
            app_sys('app/request_html/food_objective.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "Eligibility Criteria",
            app_sys('app/request_html/food_criteria.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "Obligations of the Beneficiaries",
            app_sys('app/request_html/food_obligations.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "Required Documentation",
            app_sys('app/request_html/food_documentation.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "Application Process",
            app_sys('app/request_html/food_application.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "Delivery",
            app_sys('app/request_html/food_delivery.html') %>%
              includeHTML() %>% f7Block()
          ),
          f7AccordionItem(
            title = "Download Forms",
            f7Block(
              f7DownloadButton(
                ns("sm_ibc_b1"),
                "Application for Food Assistance"
              ),
              br(),
              f7DownloadButton(
                ns("sm_ibc_b3"),
                "Affidavit of Household Income"
              ),
              br(),
              f7DownloadButton(
                ns("sm_ibc_c12"),
                "Data Protection Form"
              )
            )
          )
        )
      )
    )
  )
}

#' request Server Functions
#' @importFrom googledrive drive_ls drive_download
#' @noRd 
mod_request_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Get files' IDs
    forms <- drive_ls("forms")
    sm_ibc_a1_name <- "SM_IBC_A1 - Request for Financial Assistance.docx"
    sm_ibc_b1_name <- "SM_IBC_B1 - Application for Food Assistance.docx"
    sm_ibc_b3_name <- "SM_IBC_B3 - Affidavit of income.docx"
    sm_ibc_c1_name <- "SM_IBC_C1 - Data Protection Form.docx"
    id_sm_ibc_a1 <- subset(forms, name == sm_ibc_a1_name)$id
    id_sm_ibc_b1 <- subset(forms, name == sm_ibc_b1_name)$id
    id_sm_ibc_b3 <- subset(forms, name == sm_ibc_b3_name)$id
    id_sm_ibc_c1 <- subset(forms, name == sm_ibc_c1_name)$id
    
    # Download handlers
    output$sm_ibc_a1 <- downloadHandler(
      filename = sm_ibc_a1_name,
      content = function(file) {
        drive_download(id_sm_ibc_a1, file)
      }
    )
    
    output$sm_ibc_b1 <- downloadHandler(
      filename = sm_ibc_b1_name,
      content = function(file) {
        drive_download(id_sm_ibc_b1, file)
      }
    )
    
    output$sm_ibc_b3 <- downloadHandler(
      filename = sm_ibc_b3_name,
      content = function(file) {
        drive_download(id_sm_ibc_b3, file)
      }
    )
    
    output$sm_ibc_c1 <- downloadHandler(
      filename = sm_ibc_c1_name,
      content = function(file) {
        drive_download(id_sm_ibc_c1, file)
      }
    )
    
    output$sm_ibc_c12 <- downloadHandler(
      filename = sm_ibc_c1_name,
      content = function(file) {
        drive_download(id_sm_ibc_c1, file)
      }
    )
  })
}

## To be copied in the UI
# mod_request_ui("request_ui_1")

## To be copied in the server
# mod_request_server("request_ui_1")
