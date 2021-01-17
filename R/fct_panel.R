#' Specific function for the UI of the offers' panel
#' @param subject description of the offer
#' @param date date when the offer was posted
#' @param footer Footer content, if any. Must be wrapped in a tagList
f7Post <- function(..., subject = NULL, date = NULL, footer = NULL){
  headerTag <-
    shiny::tags$div(class = "card-header", 
                    shiny::tags$div(class = "post-subject", subject), 
                    shiny::tags$div(class = "post-date", date))
  contentTag <-
    shiny::tags$div(class = "card-content card-content-padding", ...)
  footerTag <- if (!is.null(footer)) 
    shiny::tags$div(class = "card-footer", footer)
  shiny::tags$div(class = "card demo-facebook-card", headerTag, contentTag, footerTag)
}

#' Gets the available panel data
#' Retrieves the available panel data in the googlesheets file
#' and just gathers the ones that are approved and less than 6 months old.
#' @return A list of dataframes filtered by type of offer: jobs and services
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet
#' @noRd
GetPanelData <- function(){
  wb_offer <- drive_get("IBC_POSTS")
  col_types <- paste0("T", stringr::str_dup("c", 12))
  dt_posts <- read_sheet(wb_offer, sheet = 1, col_types = col_types)
  # Filter posts that have been approved and are less than 6 months old (180 days)
  limit_date <- Sys.Date() - 180
  dt_posts$`Marca temporal`<- as.Date(dt_posts$`Marca temporal`)
  panel_data <-
    subset(dt_posts,
           `Final Status [DO NOT DELETE]` == "Approved" & `Marca temporal` >= limit_date)
  return(
    list(
      jobs = subset(panel_data, `Please select a type of post` == "Job opportunity"),
      services = subset(panel_data, `Please select a type of post` != "Job opportunity")
    )
  )
}