# SUBSCRIPTION FUNCTIONS----

#' @description It builds a HTML subscription form
#' @param mailing_lists string vector, mailing lists for subscription
#' @noRd
SubscriptionHTML <- function(mailing_lists){
  mailing_lists <- 
    plyr::mapvalues(mailing_lists,
                    from = c("jobs", "services"),
                    c("Job opportunities", "Offer your services"),
                    warn_missing = FALSE)
  doc_subs <- tags$html(
    tags$head(
      tags$title('SHARE IBC Subscription Update')
    ),
    tags$body(
      tags$img(src = app_sys('app/www/IBC_logo_1.png'), alt = 'My Logo'),
      h1('Subscription Update'),
      p('Hey there,'),
      p('Hope you are having a great day. This is a confirmation that you just
        successfully subscribed to the following mailing list(s):'),
      tags$ul(
        lapply(seq_along(mailing_lists), function(i){
          tags$li(mailing_lists[i])
        })
      ),
      p("If this is a mistake (which we hope is not) or you didn't
             subscribe to the list, please visit ",
        a(href = "http://178.62.110.168:3838/?unsubscribe=TRUE",
          "IBC Jobs Update "),
        "to update your subscription preferences."),
      p("Thanks"),
      p("Benevolence Ministry Team,"),
      p("Immanuel Baptist Church")
    )
  )
  fileConn <- file(app_sys("app/www/page_subs.html"))
  writeLines(as.character(doc_subs), fileConn)
  close(fileConn)
}

#' Checks the validity of an email input
#' @param x string, email input
is_valid_email <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case = TRUE)
}

#' Adds a new entry to the subscription list
#' @description  Adds a new entry to the googlesheets where the subscription list is located
#' @param name string, subscriber's name
#' @param email string, subscriber's email
#' @param mailing_lists string vector, mailing lists for subscription
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet sheet_append range_write
#' @importFrom mailR send.mail
#' @noRd
add_email <- function(name, email, mailing_lists){
  # Reading ibc mailing list
  wb <- drive_get("mailing_list/DT_MAILING_LIST")
  dt <- read_sheet(wb)
  ## check that email format is okay
  if(is_valid_email(email)){
    # operation successful
    email <- paste0("<", email, ">")
    time_stamp <- as.character(Sys.time())
    ## check if email is already in data
    if (email %in% dt$EMAIL) {
      # Update preferences for an existing entry
      success <- TRUE
      Ops.error <- NULL
      index <- which(dt$EMAIL == email)
      dt_index <- dt[index, ]
      dt_index$ACTIVE_JOBS <- "jobs" %in% mailing_lists
      dt_index$ACTIVE_SERVICES <- "services" %in% mailing_lists
      dt_index$LAST_ACTIVITY <- time_stamp
      # Update entry
      row <- index + 1
      start_range <- paste0(LETTERS[1], row)
      end_range <- paste0(LETTERS[ncol(dt)], row)
      range_update <- paste0(start_range, ":", end_range)
      range_write(wb$id, data = dt_index, range = range_update, col_names = FALSE)
    } else{
      # New entry
      success <- TRUE
      Ops.error <- NULL
      id_member <- paste0("ID_", stringr::str_pad(nrow(dt) + 1, width = 6, pad = "0"))
      new_entry <-
        data.frame(ID_MEMBER = id_member,
                   NAME = name,
                   EMAIL = email,
                   ACTIVE_JOBS = "jobs" %in% mailing_lists,
                   ACTIVE_SERVICES = "services" %in% mailing_lists,
                   FIRST_ACTIVITY = time_stamp,
                   LAST_ACTIVITY = time_stamp)
      SubscriptionHTML(mailing_lists)
      send.mail(from = "jobs@ibcmadrid.com",
                to = c(paste(name, email)),
                replyTo = c("jobs@ibcmadrid.com"),
                subject = "Subscription confirmation",
                body = app_sys("app/www/page_subs.html"),
                html = TRUE,
                inline = T,
                smtp = list(host.name = "smtp.yandex.com", port =  465,
                            user.name = "jobs@ibcmadrid.com",
                            passwd = "fAC3pGdUnd1", ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
      # Refresh data mailing list
      sheet_append(ss = wb, data = new_entry)
    }
  } else {
    ## operation not successful: notify that email is not valid, try another email
    success <- FALSE
    Ops.error <- c("Not valid Email, please try again.")
  }
  return(
    list(success = success,
         Ops.error = Ops.error)
  )
}

# UNSUBSCRIPTION FUNCTIONS----
#' @description It builds a HTML unsubscription form
#' @param mailing_lists string vector, mailing lists for unsubscription
#' @noRd
UnsubscriptionHTML <- function(mailing_lists){
  mailing_lists <- 
    plyr::mapvalues(mailing_lists,
                    from = c("jobs", "services"),
                    c("Job opportunities", "Offer your services"),
                    warn_missing = FALSE)
  doc_unsubs <- tags$html(
    tags$head(
      tags$title('SHARE IBC Subscription Update')
    ),
    tags$body(
      tags$img(src = app_sys('app/www/IBC_logo_1.png'), alt = 'My Logo'),
      h1('Subscription Update'),
      p('Hey there,'),
      p('Hope you are having a great day. This is a confirmation that you just
        successfully unsubscribed from the following mailing list(s):'),
      tags$ul(
        lapply(seq_along(mailing_lists), function(i){
          tags$li(mailing_lists[i])
        })
      ),
      p("If you would like to subscribe again, ",
        "please visit ",
        a(href = "http://178.62.110.168:3838/", "IBC Jobs Update "),
        "to update the subscription."),
      p("Thanks"),
      p("Benevolence Ministry Team,"),
      p("Immanuel Baptist Church")
    )
  )
  fileConn <- file(app_sys("app/www/page_unsubs.html"))
  writeLines(as.character(doc_unsubs), fileConn)
  close(fileConn)
}

#' Adds a new entry to the subscription list
#' @description  Adds a new entry to the googlesheets where the subscription list is located
#' @param name string, subscriber's name
#' @param email string, subscriber's email
#' @param mailing_lists string vector, mailing lists for unsubscription
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet range_write
#' @importFrom mailR send.mail
#' @noRd
remove_email <- function(email, mailing_lists){
  # Reading ibc mailing list
  wb <- drive_get("mailing_list/DT_MAILING_LIST")
  dt <- read_sheet(wb)
  email <- paste0("<", email, ">")
  if(email %in% dt$EMAIL){
    success <- TRUE
    Ops.error <- NULL
    index <- which(dt$EMAIL == email)
    # Send unsubscription email
    UnsubscriptionHTML(mailing_lists)
    send.mail(from = "jobs@ibcmadrid.com",
              to = c(paste(dt$NAME[index], email)),
              replyTo = c("jobs@ibcmadrid.com"),
              subject = "Unsubscription confirmation",
              body = app_sys("app/www/page_unsubs.html"),
              html = TRUE,
              inline = T,
              smtp = list(host.name = "smtp.yandex.com", port =  465,
                          user.name = "jobs@ibcmadrid.com",
                          passwd = "fAC3pGdUnd1", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    # Update preferences
    dt_index <- dt[index, ]
    if ("jobs" %in% mailing_lists) {
      dt_index$ACTIVE_JOBS <- FALSE
    }
    if ("services" %in% mailing_lists) {
      dt_index$ACTIVE_SERVICES <- FALSE
    }
    dt_index$LAST_ACTIVITY <- as.character(Sys.time())
    # Update entry
    row <- index + 1
    start_range <- paste0(LETTERS[1], row)
    end_range <- paste0(LETTERS[ncol(dt)], row)
    range_update <- paste0(start_range, ":", end_range)
    range_write(wb$id, data = dt_index, range = range_update, col_names = FALSE)
  } else{
    # operation not successful, email not found in list.
    success <- FALSE
    Ops.error <- "Email not found in the current mailing list."
  }
  return(
    list(success = success,
         Ops.error = Ops.error)
  )
}