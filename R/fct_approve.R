#' Send approval confirmation mail to poster and send approved post
#' to the respective mailing list
#' @param id_request string, id of the request
#' @param id_approver string, id of the post approver
#' @param comment string, comments related to the post approval
approve_post <- function(id_request, id_approver, comment){
  # Read post googlesheet----
  wb <- drive_get("posts/DT_POSTS")
  dt <- read_sheet(wb, sheet = "DATABASE")
  dt_post <- dt[dt$ID_POST == id_request, ]
  dt_approvers <- read_sheet(wb, sheet = "APPROVERS")
  
  # Poster's confirmation email----
  ConfirmationPostHTML(id_request)
  # Send mail
  send.mail(from = "Social Ministry IBC <jobs@ibcmadrid.com>",
            to = c(paste(dt_post$NAME_POSTER, "<", dt_post$EMAIL_POSTER, ">")),
            replyTo = c("jobs@ibcmadrid.com"),
            subject = "Post approval confirmation",
            body = app_sys("app/messages/request_approve.html"),
            html = TRUE,
            inline = T,
            smtp = list(host.name = "smtp.yandex.com", port =  465,
                        user.name = "jobs@ibcmadrid.com",
                        passwd = "fAC3pGdUnd1", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
  # Send approved post to the mailing list----
  ApprovedPostHTML(dt_post)
  # Get mailing list
  wb_ml <- drive_get("mailing_list/DT_MAILING_LIST")
  dt_ml <- read_sheet(wb_ml)
  if (dt_post$TYPE_OFFER == "jobs") {
    mailing_list <- dt_ml[dt_ml$ACTIVE_JOBS, ]$EMAIL
  } else if (dt_post$TYPE_OFFER == "services") {
    mailing_list <- dt_ml[dt_ml$ACTIVE_SERVICES, ]$EMAIL
  }
  # Get attached files if any
  
  # Send mail
  send.mail(from = "Social Ministry IBC <jobs@ibcmadrid.com>",
            to = dt_approvers$EMAIL,
            subject = "A New Request Requires Your Approval!",
            body = app_sys("app/messages/request_post.html"),
            html = TRUE,
            inline = T,
            smtp = list(host.name = "smtp.yandex.com", port =  465,
                        user.name = "jobs@ibcmadrid.com",
                        passwd = "fAC3pGdUnd1", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE,
            attach.files = files_path)
  
  
  # Update post status to approved
  
}

#' HTML file to be sent to the poster confirming that
#' his/her post was approved.
#' @param id_request string, id of the request
ConfirmationPostHTML <- function(id_request){
  approve_html <- tags$html(
    tags$head(
      tags$title('SHARE IBC')
    ),
    tags$body(
      h2('Post approval confirmation'),
      p('Dear brother/sister,'),
      p(paste('Your post with ID', id_request, 'has been approved.')),
      p('Thank you so much for being part of the IBC community!'),
      hr(),
      p(strong("Benevolence Ministry Team")),
      tags$img(src = app_sys('app/messages/IBC_logo_1.png'), alt = 'My Logo', width = '200', height = '50')
    )
  )
  fileConn <- file(app_sys("app/messages/confirmation_post.html"))
  writeLines(as.character(approve_html), fileConn)
  close(fileConn)
}


#' Create HTML report - approved post - to be sent to the mailing list
#' @param dt_post A tibble element that contains all relevant information about the post
ApprovedPostHTML <- function(dt_post){
  # Pretty type of post names
  type_post <- 
    plyr::mapvalues(dt_post$TYPE_OFFER,
                    from = c("jobs", "services"),
                    c("Job opportunities", "Offer your services"),
                    warn_missing = FALSE)
  # Create custom HTML doc
  html_post <-
    tags$html(
      lang = "en",
      tags$body(
        style = "font-size: 16px;",
        tags$div(
          style = "width: 100%; text-align: center;",
          tags$div(
            style = "width:70%;text-align:center;border:1px solid #dddddd;border-radius:5px;padding:10px 50px;margin:0 auto 20px",
            tags$p(style = "font-size: 18px", paste("#", dt_post$ID_POST, "|", format(Sys.Date(), "%A, %d %B %Y"))),
            tags$h2("SHARE IBC Update"),
            tags$p(
              style = "text-align: left; padding-left: 10px; margin-bottom: 0;",
              "Hope you are having a great day. We just recieved the following post. Please have a look at it to see if it is of interest to you.!"
            ),
            br(),
            tags$div(
              style = "width: 100%;",
              tags$table(
                style = "width: 100%;border-collapse: collapse; margin: 0 auto;",
                tags$colgroup(
                  tags$col(
                    span = "1",
                    style = "width: 20%;"
                  ),
                  tags$col(
                    span = "1",
                    style = "width: 80%;"
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(
                      style = "text-align: left; padding: 15px 10px; font-weight: bold;",
                      "Type of post"
                    ),
                    tags$td(
                      style = "text-align: left; padding: 15px 10px;",
                      type_post
                    )
                  ),
                  tags$tr(
                    style = "border-top: 1px solid #dddddd;",
                    tags$td(
                      style = "text-align: left; padding: 15px 10px; font-weight: bold;",
                      "Subject of the post"
                    ),
                    tags$td(
                      style = "text-align: left; padding: 15px 10px;",
                      dt_post$SUBJECT
                    )
                  ),
                  tags$tr(
                    style = "border-top: 1px solid #dddddd;",
                    tags$td(
                      style = "text-align: left; padding: 15px 10px; font-weight: bold;",
                      "Description of the post"
                    ),
                    tags$td(
                      style = "text-align: left; padding: 15px 10px;",
                      dt_post$DESCRIPTION
                    )
                  ),
                  tags$tr(
                    style = "border-top: 1px solid #dddddd;",
                    tags$td(
                      style = "text-align: left; padding: 15px 10px; font-weight: bold;",
                      "Contact information - email"
                    ),
                    tags$td(
                      style = "text-align: left; padding: 15px 10px;",
                      dt_post$CONTACT_EMAIL
                    )
                  ),
                  tags$tr(
                    style = "border-top: 1px solid #dddddd;",
                    tags$td(
                      style = "text-align: left; padding: 15px 10px; font-weight: bold;",
                      "Contact information - phone number"
                    ),
                    tags$td(
                      style = "text-align: left; padding: 15px 10px;",
                      dt_post$CONTACT_PHONE
                    )
                  )
                )
              )
            ),
            hr(),
            tags$p(
              style = "text-align: left; padding-left: 10px; margin-bottom: 0;",
              "This post was sent by a fellow church member. Hence, if you have any question or enquiry about the job post, please reach out to the relevant address provided in the posting."
            ),
            p(strong("Benevolence Ministry Team")),
            tags$img(src = app_sys('app/messages/IBC_logo_1.png'), alt = 'My Logo', width = '200', height = '50')
          )
        )
      )
    )
  fileConn <- file(app_sys("app/messages/approved_post.html"))
  writeLines(as.character(html_post), fileConn)
  close(fileConn)
}
