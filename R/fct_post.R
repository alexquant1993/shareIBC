#' Send the approved post to the mailing list
#' It reads the posts from the IBC_POSTS googlesheet and sends the
#' approved post.
#' @param wb_offer An object of class dribble, where the offer googlesheet is located
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet sheet_append
#' @importFrom mailR send.mail
#' @noRd
PostOffer <- function(wb_offer){
  # Get offer to send to the mailing list
  dt_posts <- read_sheet(wb_offer, sheet = 1)
  # Filter posts that have been approved and haven't been sent
  dt_posts <-
    subset(dt_posts,`Final Status [DO NOT DELETE]` == "Approved" & is.na(Sent))
  # Select the last one (defensive programming)
  dt_posts <- tail(dt_posts, 1)
  
  # Get current mailing list
  wb <- drive_get("JOBS_IBC_MAILING_LIST")
  dt_mailing_list <- read_sheet(wb, sheet = "Subscribers")
  recipients <- dt_mailing_list$EMAIL
  
  # Get offer HTML
  subject <- dt_posts$`Enter the subject of your post`
  type_offer <- dt_posts$`Please select a type of post`
  description <- dt_posts$`Offer a brief description of the post`
  email <- dt_posts$`Enter contact information - email`
  phone_number <- dt_posts$`Enter contact information - phone number`
  OfferHTML(subject, type_offer, description, email, phone_number)
  
  # Send email
  send.mail(from = "jobs@ibcmadrid.com",
            to = "jobs@ibcmadrid.com",
            bcc = recipients,
            subject = "Job Offers Update",
            replyTo = c("jobs@ibcmadrid.com"),
            body = app_sys("app/www/page_offer.html"),
            html = TRUE,
            inline = T,
            smtp = list(host.name = "smtp.yandex.com", port =  465,
                        user.name = "jobs@ibcmadrid.com",
                        passwd = "fAC3pGdUnd1", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
}

#' Build a HTML object containing the details of the post
#' @param subject string, subject of the post
#' @param type_offer string, type of post, it can be:
#' "Job opportunity" or "Offer your services"
#' @param description string, description of the post
#' @param email string, post's contact details
#' @param phone_number string, post's contact details
#' @importFrom htmltools tags strong p HTML hr em
#' @noRd

OfferHTML <- function(subject, type_offer, description, email, phone_number){
  type_offer <- match.arg(type_offer, c("Job opportunity"), "Offer your services")
  description <- stringr::str_replace_all(description, c("\n" = "<br>"))
  # Head title
  if (type_offer == "Job opportunity") {
    head_title <- tags$h1('Job Update')
  } else {
    head_title <- tags$h1('Services Update')
  }
  # Contact details
  if (isTruthy(email) | isTruthy(phone_number)) {
    contact <-
      tagList(
        strong(p("Contact details:")),
        tags$ul(
          tags$li(paste("Phone number:", phone_number)),
          tags$li(paste("Email:", email)),
        )
      )
  } else {
    contact <- NULL
  }
  # Main HTML
  doc_job <- tags$html(
    tags$head(
      tags$title('Share IBC Update')
    ),
    tags$body(
      tags$img(src = app_sys('app/www/IBC_logo_1.png'), alt = 'My Logo'),
      head_title,
      p('Hey there,'),
      p('Hope you are having a great day. We just recieved the following post.
        Please have a look at it to see if it is of interest to you.'),
      hr(),
      strong(p(paste("Title:", subject))),
      p(HTML(description)),
      contact,
      hr(),
      p("This post was sent by a fellow church member. Hence, if you have any question or
        enquiry about the post, please reach out to the relevant contact information provided in the posting."),
      p("Thanks."),
      p("Benevolence Ministry Team,"),
      p("Immanuel Baptist Church"),
      em(tags$p("To update your newsletter preferences or unsubscribe, ",
                tags$a(href = "http://178.62.110.168:3838/?unsubscribe=TRUE",
                       "click here")))
    )
  )
  fileConn <- file(app_sys("app/www/page_offer.html"))
  writeLines(as.character(doc_job), fileConn)
  close(fileConn)
}