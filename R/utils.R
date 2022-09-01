#' A random image
#' @description This function returns a random image
#' @usage random_image()
#' @return an image
#' @examples
#' random_image()
#' @noRd
RandomPic <- function() {
  l <- list.files(system.file("img", package = "shareIBC"), 
                  full.names = TRUE)
  img <- normalizePath(sample(l, 1))
  tmpimg <- tempfile(fileext = ".jpg")
  file.copy(img, tmpimg)
  tmpimg
}

#' Get inputs from a headless app object from the shinytest2 package
#' @description It allows you to unpack the values given a vector of input names
#' @param app headless shiny app, an object from shinytest2 package
#' @param inputs a vector of input names
#' @return a vector of values given the provided input names
#' @noRd
GetInputs <- function(app, inputs){
  ls <- app$get_values()$input
  unname(unlist(ls[inputs]))
}


#' Populate posts database with random data
#' @usage RandomPosts()
#' @param n number of random posts
#' @return A data.frame
#' @noRd
RandomPosts <- function(n = 20) {
  # Create dataframe with random posts
  ID_POST <- paste0("ID_", stringr::str_pad(1:n, width = 6, pad = "0"))
  NAME_POSTER <- charlatan::ch_name(n)
  EMAIL_POSTER <- rep(get_golem_config("testing_email"), n)
  TYPE_POST <- 
    sample(c("jobs", "services", "upcycle", "mix"), size = n, replace = TRUE)
  SUBJECT <-
    sample(
      c(charlatan::ch_taxonomic_epithet(n),
        charlatan::ch_color_name(n),
        charlatan::ch_company(n),
        charlatan::ch_name(n)),
      size = n, replace = TRUE
    )
  DESCRIPTION <- rep(shinipsum::random_text(nwords = 20), n)
  CONTACT_EMAIL <- rep("email@example.com", n)
  CONTACT_PHONE <- charlatan::ch_phone_number(n, locale = "es_ES")
  FILES_URL <- rep(NA, n)
  GDPR_ACCEPTANCE <- rep(TRUE, n)
  DATE_POST <- rep(Sys.time(), n)
  STATUS <- rep("Approved", n)
  ID_APPROVER <- rep("APV_01", n)
  COMMENTS_APPROVER <- rep(shinipsum::random_text(nwords = 10), n)
  DATE_REVISION <- rep(Sys.time(), n)
  CONDITION <- rep("Open", n)
  
  dt <-
    data.frame(
      ID_POST, NAME_POSTER, EMAIL_POSTER, TYPE_POST, SUBJECT, DESCRIPTION,
      CONTACT_EMAIL, CONTACT_PHONE, FILES_URL, GDPR_ACCEPTANCE, DATE_POST,
      STATUS, ID_APPROVER, COMMENTS_APPROVER, DATE_REVISION, CONDITION
    )
  
  # Connect to APIs
  shareIBC:::ApiConnections("default")
  # Read posts database
  wb <- 
    googledrive::drive_get(
      shareIBC:::get_golem_config("posts_path", config = "default")
    )
  # Populate the posts database for testing purposes
  googlesheets4::range_write(
    wb,
    data = dt,
    sheet = "DATABASE",
    range = "A2",
    col_names = FALSE
  )
  NULL
}
