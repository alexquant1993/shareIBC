#' Function to create UI posts given inputs
#' @param subject post brief description
#' @param date date of the post

#' @param content post description and details
#' @param footer Footer content, if any. Must be wrapped in a tagList
#' @noRd
f7Post <- function(subject,
                   date,
                   content,
                   footer = NULL){
  # Post header UI
  headerTag <-
    shiny::tags$div(
      class = "card-header",
      shiny::tags$div(class = "card-subject", subject),
      shiny::tags$div(class = "card-date", date)
    )
  
  # Post content UI
  contentTag <-
    shiny::tags$div(class = "card-content card-content-padding", content)
  
  # Post footer UI
  footerTag <- if (!is.null(footer)) 
    shiny::tags$div(class = "card-footer", footer)
  
  # Return post bundled UI
  shiny::tags$div(
    class = "card demo-facebook-card",
    headerTag,
    contentTag,
    footerTag
  )
}

#' Function that gets the available post data
#' @description Retrieves the available post data in the googlesheets file
#' and just gathers the ones that are approved and less than 4 months old.
#' @return A list of dataframes filtered by the type of post
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet
#' @noRd
GetPostData <- function(){
  # Access googlesheets file
  wb_posts <- drive_get(get_golem_config("posts_path"))
  dt_posts <- read_sheet(wb_posts, sheet = 'DATABASE')
  dt_posts$DATE_POST <- as.Date(dt_posts$DATE_POST)
  
  # Filter posts given the following conditions:
  # Only approved posts are shown
  # Only posts that have been posted less than 4 months ago are shown
  # Only posts with condition 'Open' are shown (still available)
  dt_out <-
    subset(
      dt_posts,
      STATUS == "Approved" &
        DATE_POST >= Sys.Date() - 120 &
        CONDITION == "Open"
    )
  # Order posts in chronological order
  dt_out <- dt_out[order(dt_out$DATE_POST, decreasing = TRUE),]
  return(
    list(
      jobs = subset(dt_out, TYPE_POST == "jobs"),
      services = subset(dt_out, TYPE_POST == "services"),
      upcycle = subset(dt_out, TYPE_POST == "upcycle"),
      mix = subset(dt_out, TYPE_POST == "mix")
    )
  )
}
