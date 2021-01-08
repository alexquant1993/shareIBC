# SUBSCRIPTION FUNCTIONS----

#' @description It builds a HTML subscription form
#' @noRd
SubscriptionHTML <- function(){
  doc_subs <- tags$html(
    tags$head(
      tags$title('IBC Job Offers Subscription Update')
    ),
    tags$body(
      tags$img(src = app_sys('app/www/IBC_logo_1.png'), alt = 'My Logo'),
      h1('IBC Jobs Subscription Update'),
      p('Hey there,'),
      p('Hope you are having a great day. This is a confirmation that you just
        successfully subscribed to the IBC Jobs Update mailing list.'),
      tags$p("If this is a mistake (which we hope is not) or you didn't
             subscribe to the list, please visit ",
             tags$a(href = "http://178.62.110.168:3838/?unsubscribe=TRUE",
                    "IBC Jobs Update "),
             "to update the subscription."),
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
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet sheet_append
#' @importFrom mailR send.mail
#' @noRd
add_email <- function(name, email){
  # Reading ibc mailing list
  wb <- drive_get("JOBS_IBC_MAILING_LIST")
  dt <- read_sheet(wb, sheet = "Subscribers")
  ## check that email format is okay
  if(is_valid_email(email)){
    email <- paste0("<", email, ">")
    ## check if email is already in data
    if (email %in% dt$EMAIL) {
      success <- FALSE
      Ops.error <- "Email already exists in the list."
      # operation not successful: email already exists
    } else{
      # operation successful
      success <- TRUE
      Ops.error <- NULL
      new_entry <-
        data.frame(ID_MEMBER = tail(dt$ID_MEMBER, 1) + 1, NAME = name,
                   EMAIL = email, DATE = as.character(Sys.time()))
      SubscriptionHTML()
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
      sheet_append(ss = wb, data = new_entry, sheet = "Subscribers")
    }
  } else {
    ## operation not successful: notify that email not valid, try another email
    success <- FALSE
    Ops.error <- c("Email not valid, please try again.")
  }
  return(
    list(success = success,
         Ops.error = Ops.error)
  )
}

# UNSUBSCRIPTION FUNCTIONS----
#' @description It builds a HTML unsubscription form
#' @noRd
UnsubscriptionHTML <- function(){
  doc_unsubs <- tags$html(
    tags$head(
      tags$title('IBC Job Offers Subscription Update')
    ),
    tags$body(
      tags$img(src = app_sys('app/www/IBC_logo_1.png'), alt = 'My Logo'),
      h1('IBC Jobs Subscription Update'),
      p('Hey there,'),
      p('Hope you are having a great day. This is a confirmation that you just
        successfully unsubscribed from the IBC Jobs Update mailing list.'),
      tags$p("If you would like to subscribe again, ",
             "please visit ",
             tags$a(href = "http://178.62.110.168:3838/",
                    "IBC Jobs Update "),
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
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet sheet_write
#' @importFrom mailR send.mail
#' @noRd
remove_email <- function(email){
  # Reading ibc mailing list
  wb <- drive_get("JOBS_IBC_MAILING_LIST")
  dt <- read_sheet(wb, sheet = "Subscribers")
  email <- paste0("<", email, ">")
  if(email %in% dt$EMAIL){
    success <- TRUE
    Ops.error <- NULL
    index <- which(dt$EMAIL == email)
    # Send unsubscription email
    UnsubscriptionHTML()
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
    # update email_list
    dt <- dt[-index, ]
    sheet_write(ss = wb, data = dt, sheet = "Subscribers")
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