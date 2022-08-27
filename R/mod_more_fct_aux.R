#' Adds email to the selected mailing lists and sends a confirmation mail
#' to the subscriber
#' @param name string, subscriber's name
#' @param email string, subscriber's email
#' @param mailing_lists string vector, mailing lists for subscription
#' @param session	shiny session
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet sheet_append range_write
#' @importFrom gmailr gm_mime gm_to gm_bcc gm_from gm_subject gm_html_body
#'  gm_send_message
#' @noRd
SubscribeEmail <- function(name, email, mailing_lists, session){
  Ops.error <- NULL
  # Add email to selected mailing lists
  tryCatch(
    {
      # Load mailing list database
      wb <- drive_get(get_golem_config("mailing_list_path"))
      dt <- read_sheet(wb)
      
      # Check if email is valid
      cond_email <- ifelse(isTruthy(email) & is_valid_email(email), TRUE, FALSE)
      
      # Output conditioned to whether the email is valid or not
      if (cond_email) {
        email <- paste0("<", email, ">")
        time_stamp <- as.character(Sys.time())
        # Check if email is already registered in the mailing list
        if (email %in% dt$EMAIL) {
          # Update preferences for an existing entry
          index <- which(dt$EMAIL == email)
          dt_index <- dt[index, ]
          if (!dt_index$ACTIVE_JOBS) {
            dt_index$ACTIVE_JOBS <- "jobs" %in% mailing_lists
          }
          if (!dt_index$ACTIVE_SERVICES) {
            dt_index$ACTIVE_SERVICES <- "services" %in% mailing_lists
          }
          if (!dt_index$ACTIVE_UPCYCLE) {
            dt_index$ACTIVE_UPCYCLE <- "upcycle" %in% mailing_lists
          }
          if (!dt_index$ACTIVE_MIX) {
            dt_index$ACTIVE_MIX <- "mix" %in% mailing_lists
          }
          dt_index$LAST_ACTIVITY <- time_stamp
          # Update entry
          row <- index + 1
          start_range <- paste0(LETTERS[1], row)
          end_range <- paste0(LETTERS[ncol(dt)], row)
          range_update <- paste0(start_range, ":", end_range)
          range_write(
            wb$id,
            data = dt_index,
            sheet = "DATABASE",
            range = range_update,
            col_names = FALSE
          )
        } else {
          # New entry
          id_member <- 
            paste0("ID_", stringr::str_pad(nrow(dt) + 1, width = 6, pad = "0"))
          new_entry <-
            data.frame(
              ID_MEMBER = id_member,
              NAME = name,
              EMAIL = email,
              ACTIVE_JOBS = "jobs" %in% mailing_lists,
              ACTIVE_SERVICES = "services" %in% mailing_lists,
              ACTIVE_UPCYCLE = "upcycle" %in% mailing_lists,
              ACTIVE_MIX = "mix" %in% mailing_lists,
              FIRST_ACTIVITY = time_stamp,
              LAST_ACTIVITY = time_stamp
            )
          # Add new entry to mailing list database
          sheet_append(ss = wb, data = new_entry)
        }
        # Compose subscription confirmation email
        SubscriptionHTML(mailing_lists)
        # Send confirmation email
        message <- 
          gm_mime() %>% 
          gm_to(paste(name, email)) %>% 
          gm_from(get_gmail_account()) %>% 
          gm_subject("Subscription confirmation") %>% 
          gm_html_body(
            paste(
              readLines(app_sys("app/messages/page_subs.html")),
              collapse = ""
            )
          )
        gm_send_message(message)
      } else {
        # Not successfull operation
        Ops.error <- "Not valid Email, please try again."
      }
    },
    error = function(e){
      message(e)
      Ops.error <<- e
      f7Dialog(
        session = session,
        title = "Error",
        text = e,
        type = "alert"
      )
      NULL
    }
  )
  
  # If there is an error, the operation is not successful
  success <- ifelse(!is.null(Ops.error), FALSE, TRUE)
  
  return(
    list(success = success,
         Ops.error = Ops.error)
  )
}


#' It builds a HTML subscription form
#' @param mailing_lists string vector, mailing lists for subscription
#' @noRd
SubscriptionHTML <- function(mailing_lists){
  # Map values to a readable format
  mailing_lists <- 
    plyr::mapvalues(mailing_lists,
                    from = c("jobs", "services", "upcycle", "mix"),
                    c("Job opportunities", "Offer your services",
                      "Upcycle and Donate", "Miscellaneous"),
                    warn_missing = FALSE)
  # Create HTML report
  doc_subs <- 
    tags$html(
      lang = "eng",
      tags$body(
        tags$h2('Subscription Update'),
        tags$p('Dear brother/sister,'),
        tags$p('We hope you are having a great day. This is a confirmation
        email indicating that you have successfully subscribed to the
        following mailing list(s):'),
        tags$ul(
          lapply(seq_along(mailing_lists), function(i){
            tags$li(mailing_lists[i])
          })
        ),
        tags$p("Please do not reply to this email as it was generated
          automatically and is not being monitored."),
        tags$p('Thank you so much for being part of the IBC community!'),
        tags$hr(),
        tags$p(tags$strong("Social Ministry Team")),
        tags$img(
          src = get_golem_config("ibc_logo_url"),
          width = 200, height = 50)
      )
    )
  fileConn <- file(app_sys("app/messages/page_subs.html"))
  writeLines(as.character(doc_subs), fileConn)
  close(fileConn)
}

#' Checks the validity of an email input
#' @param x string, email input
#' @noRd
is_valid_email <- function(x) {
  grepl(
    "^\\s*[A-Z0-9._%&'*+`/=?^{}~-]+@[A-Z0-9.-]+\\.[A-Z0-9]{2,}\\s*$",
    as.character(x),
    ignore.case = TRUE
  )
}

#' Get gmail account - pretty name
#' @noRd
get_gmail_account <- function(pretty = TRUE) {
  out <- get_golem_config("gmail_account")
  if (pretty) {
    out <- paste0(get_golem_config("golem_name"), " <", out, ">")
  }
  return(out)
}


#' Adds a new entry to the subscription list
#' @description  Adds a new entry to the googlesheets where the subscription list is located
#' @param name string, subscriber's name
#' @param email string, subscriber's email
#' @param mailing_lists string vector, mailing lists for unsubscription
#' @param session	shiny session
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet range_write
#' @importFrom gmailr gm_mime gm_to gm_bcc gm_from gm_subject gm_html_body
#'  gm_send_message
#' @noRd
UnsubscribeEmail <- function(email, mailing_lists, session){
  Ops.error <- NULL
  # Add email to selected mailing lists
  tryCatch(
    {
      # Load mailing list database
      wb <- drive_get(get_golem_config("mailing_list_path"))
      dt <- read_sheet(wb)
      email <- paste0("<", email, ">")
      
      # Check if the email is registered in the mailing list database
      if(email %in% dt$EMAIL){
        # The email is in the database
        index <- which(dt$EMAIL == email)
        # Update preferences
        dt_index <- dt[index, ]
        if ("jobs" %in% mailing_lists) {
          dt_index$ACTIVE_JOBS <- FALSE
        }
        if ("services" %in% mailing_lists) {
          dt_index$ACTIVE_SERVICES <- FALSE
        }
        if ("services" %in% mailing_lists) {
          dt_index$ACTIVE_UPCYCLE <- FALSE
        }
        if ("mix" %in% mailing_lists) {
          dt_index$ACTIVE_MIX <- FALSE
        }
        dt_index$LAST_ACTIVITY <- as.character(Sys.time())
        # Update entry
        row <- index + 1
        start_range <- paste0(LETTERS[1], row)
        end_range <- paste0(LETTERS[ncol(dt)], row)
        range_update <- paste0(start_range, ":", end_range)
        range_write(
          wb$id,
          data = dt_index,
          sheet = "DATABASE",
          range = range_update,
          col_names = FALSE
        )
        # Compose unsubscription email
        UnsubscriptionHTML(mailing_lists)
        # Send unsubscription confirmation email
        message <- 
          gm_mime() %>% 
          gm_to(email) %>% 
          gm_from(get_gmail_account()) %>% 
          gm_subject("Unsubscription confirmation") %>% 
          gm_html_body(
            paste(
              readLines(app_sys("app/messages/page_unsubs.html")),
              collapse = ""
            )
          )
        gm_send_message(message)
      } else {
        # The email is not in the database
        Ops.error <- "Email not found in the current mailing list."
      }
    },
    error = function(e){
      message(e)
      Ops.error <<- e
      f7Dialog(
        session = session,
        title = "Error",
        text = e,
        type = "alert"
      )
      NULL
    }
  )
  
  # If there is an error, the operation is not successful
  success <- ifelse(!is.null(Ops.error), FALSE, TRUE)
  
  return(
    list(success = success,
         Ops.error = Ops.error)
  )
}


#' It builds a HTML unsubscription form
#' @param mailing_lists string vector, mailing lists for unsubscription
#' @noRd
UnsubscriptionHTML <- function(mailing_lists){
  # Map values to a readable format
  mailing_lists <- 
    plyr::mapvalues(mailing_lists,
                    from = c("jobs", "services", "upcycle", "mix"),
                    c("Job opportunities", "Offer your services",
                      "Upcycle and Donate", "Miscellaneous"),
                    warn_missing = FALSE)
  # Create HTML report
  doc_unsubs <- 
    tags$html(
      lang = "eng",
      tags$body(
        tags$h2('Subscription Update'),
        tags$p('Dear brother/sister,'),
        tags$p('We hope you are having a great day. This is a confirmation
        email indicating that you have successfully unsubscribed to the
        following mailing list(s):'),
        tags$ul(
          lapply(seq_along(mailing_lists), function(i){
            tags$li(mailing_lists[i])
          })
        ),
        tags$p("Please do not reply to this email as it was generated
                automatically and is not being monitored."),
        tags$p('Thank you so much for being part of the IBC community!'),
        tags$hr(),
        tags$p(tags$strong("Social Ministry Team")),
        tags$img(
          src = get_golem_config("ibc_logo_url"),
          width = 200, height = 50)
      )
    )
  fileConn <- file(app_sys("app/messages/page_unsubs.html"))
  writeLines(as.character(doc_unsubs), fileConn)
  close(fileConn)
}
