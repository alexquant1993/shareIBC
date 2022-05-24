#' Send approval confirmation mail to poster and send approved post
#' to the mailing list
#' @param id_request string, id of the request
#' @param id_approver string, id of the post approver
#' @param comment string, comments related to the post approval
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet range_write
#' @importFrom gmailr gm_mime gm_to gm_from gm_subject gm_html_body gm_send_message
ApprovePost <- function(id_request, id_approver, comment){
  Ops.error <- NULL
  
  tryCatch(
    {
      print("Reading googlesheets database...")
      # Pull post information from database
      wb <- drive_get("posts/DT_POSTS")
      dt <- read_sheet(wb, sheet = "DATABASE")
      dt_post <- dt[dt$ID_POST == id_request, ]
      
      # Send confirmation mail to the poster
      print("Creating HTML confirmation mail...")
      ConfirmationPostHTML(id_request)
      print("Sending confirmation mail to posters...")
      message <- 
        gm_mime() %>% 
        gm_to(dt_post$EMAIL_POSTER) %>% 
        gm_from("Social Ministry IBC <jobs.ibcmadrid@gmail.com>") %>% 
        gm_subject(paste("Post approval confirmation with ID code", id_request)) %>% 
        gm_html_body(
          paste(
            readLines(app_sys("app/messages/confirmation_post.html")),
            collapse = ""
          )
        )
      gm_send_message(message)
      
      # Send post to the current mailing list
      print("HTML creation of the approved post...")
      ApprovedPostHTML(dt_post)
      print("Getting current mailing list...")
      wb_ml <- drive_get("mailing_list/DT_MAILING_LIST")
      dt_ml <- read_sheet(wb_ml, sheet = "DATABASE")
      if (dt_post$TYPE_POST == "jobs") {
        mailing_list <- dt_ml[dt_ml$ACTIVE_JOBS, ]$EMAIL
      } else if (dt_post$TYPE_POST == "services") {
        mailing_list <- dt_ml[dt_ml$ACTIVE_SERVICES, ]$EMAIL
      } else if (dt_post$TYPE_POST == "upcycle") {
        mailing_list <- dt_ml[dt_ml$ACTIVE_UPCYCLE, ]$EMAIL
      }
      
      print("Sending approved post to current mailing list...")
      message <- 
        gm_mime() %>% 
        gm_bcc(mailing_list) %>% 
        gm_from("Social Ministry IBC <jobs.ibcmadrid@gmail.com>") %>% 
        gm_subject("Share IBC Update!") %>% 
        gm_html_body(
          paste(
            readLines(app_sys("app/messages/approved_post.html")),
            collapse = ""
          )
        )
      # Attach pictures, when they are available in the post
      files_id <- dt_post$FILES_URL
      if (!is.na(files_id)) {
        print("Attaching pictures...")
        # Split the id files
        files_id <- strsplit(files_id, ",", fixed = TRUE)[[1]]
        # Get the Google Drive public URL addresses
        files_url <-
          sapply(
            files_id,
            function (x) paste0("https://lh3.googleusercontent.com/d/", x)
          )
        # Attach files recursively
        if (length(files_url) > 1) {
          for (k in 1:length(files_url)) {
            # TODO - fix this, attach file from URL
            message <- gm_attach_url(message, files_url[k])
          }
        }
      }
      gm_send_message(message)
      
      # Update post status to approved
      print("Updating post status to approved")
      col <- which(colnames(dt) == "STATUS")
      col <- LETTERS[col]
      row <- which(dt$ID_POST == id_request) + 1
      range <- paste0(col,row)
      range_write(
        wb,
        data = data.frame("Approved", id_approver, comment, Sys.time()),
        sheet = "DATABASE",
        range = range,
        col_names = FALSE
      )
    },
    error = function(e){
      message(e)
      Ops.error <<- e
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


#' Auxiliary function to attach files from URL to message - gmailr package
#' @param mime message
#' @param description complete URL including scheme (such as http://, https://, ftp:// or file://)
#' @importFrom gmailr gm_attach_part
gm_attach_url <- function(mime, description){
  con <- url(description, "rb")
  size <- httr::HEAD(description)$headers$`content-length`
  type <- httr::HEAD(description)$headers$`content-type`
  body <- readBin(con, "raw", size)
  close(con)
  
  base_name <- basename(description)
  
  gm_attach_part(mime, body,
                 content_type = type,
                 name = base_name,
                 filename = base_name,
                 disposition = "attachment")
}


#' HTML file to be sent to the poster confirming that
#' his/her post was approved.
#' @param id_request string, id of the request
ConfirmationPostHTML <- function(id_request){
  html_post <-
    tags$html(
      lang = "en",
      tags$body(
        h2('Post approval confirmation'),
        p('Dear brother/sister,'),
        p(paste('Your post with ID code', id_request, 'has been approved.')),
        p('Thank you so much for being part of the IBC community!'),
        hr(),
        p(strong("Social Ministry Team")),
        tags$img(
          src = "https://lh3.googleusercontent.com/d/1NYp9t0PuytBXQrQs_oI8t6XqS3hHye7G",
          width = 200,
          height = 50
        )
      )
    )
  fileConn <- file(app_sys("app/messages/confirmation_post.html"))
  writeLines(as.character(html_post), fileConn)
  close(fileConn)
}


#' Create HTML report - approved post - to be sent to the mailing list
#' @param dt_post A tibble element that contains all relevant information
#' about the post
ApprovedPostHTML <- function(dt_post){
  # Pretty names - type of post
  type_post <- 
    plyr::mapvalues(dt_post$TYPE_POST,
                    from = c("jobs", "services", "upcycle"),
                    c("Job opportunities", "Offer your services",
                      "Upcycle and donate"),
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
            tags$h2("Share with your IBC church community"),
            tags$p(
              style = "text-align: left; padding-left: 10px; margin-bottom: 0;",
              "We hope you are having a great day. We have just received the
              following post. Take a look to see if it is of interest to you!"
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
              "This post was submitted by a church member. Therefore, 
              if you have any questions or concerns about the publication,
              please contact the appropriate address provided in the publication."
            ),
            p(strong("Social Ministry Team")),
            tags$img(
              src = "https://lh3.googleusercontent.com/d/1NYp9t0PuytBXQrQs_oI8t6XqS3hHye7G",
              width = 200,
              height = 50
            )
          )
        )
      )
    )
  fileConn <- file(app_sys("app/messages/approved_post.html"))
  writeLines(as.character(html_post), fileConn)
  close(fileConn)
}


#' Sends rejection mail to the poster with the corresponding explanation
#' @param id_request string, id of the request
#' @param id_approver string, id of the post approver
#' @param comment string, comments related to the post rejection
#' @importFrom googledrive drive_get
#' @importFrom googlesheets4 read_sheet
#' @importFrom gmailr gm_mime gm_to gm_from gm_subject gm_html_body gm_send_message
RejectPost <- function(id_request, id_approver, comment){
  Ops.error <- NULL
  
  tryCatch(
    {
      # Pull post information from database
      wb <- drive_get("posts/DT_POSTS")
      dt <- read_sheet(wb, sheet = "DATABASE")
      dt_post <- dt[dt$ID_POST == id_request, ]
      
      # Send rejection mail to the poster
      print("Creating HTML rejection mail...")
      RejectionPostHTML(id_request, comment)
      print("Sending rejection mail to poster...")
      message <- 
        gm_mime() %>% 
        gm_to(dt_post$EMAIL_POSTER) %>% 
        gm_from("Social Ministry IBC <jobs.ibcmadrid@gmail.com>") %>% 
        gm_subject(paste("Post rejection notification with ID code", id_request)) %>% 
        gm_html_body(
          paste(
            readLines(app_sys("app/messages/rejected_post.html")),
            collapse = ""
          )
        )
      gm_send_message(message)
      
      # Update post status to rejected
      print("Updating post status to rejected...")
      col <- which(colnames(dt) == "STATUS")
      col <- LETTERS[col]
      row <- which(dt$ID_POST == id_request) + 1
      range <- paste0(col,row)
      range_write(
        wb,
        data = data.frame("Rejected", id_approver, comment, Sys.time()),
        sheet = "DATABASE",
        range = range,
        col_names = FALSE
      )
    },
    error = function(e){
      message(e)
      Ops.error <<- e
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


#' HTML file to be sent to the poster stating that
#' his/her post was rejected.
#' @param id_request string, id of the request
#' @param comment string, comments related to the post rejection
RejectionPostHTML <- function(id_request, comment){
  html_post <-
    tags$html(
      lang = "en",
      tags$body(
        h2('Post rejection notification'),
        p('Dear brother/sister,'),
        p(paste('Your post with ID code', id_request, 'has been rejected.
                Below you can find more details about this decision:')),
        hr(),
        p(comment),
        hr(),
        p('We kindly ask that you resubmit your publication according to the
          specifications listed above.'),
        p('Thank you so much for being part of the IBC community!'),
        hr(),
        p(strong("Social Ministry Team")),
        tags$img(
          src = "https://lh3.googleusercontent.com/d/1NYp9t0PuytBXQrQs_oI8t6XqS3hHye7G",
          width = 200,
          height = 50
        )
      )
    )
  fileConn <- file(app_sys("app/messages/rejected_post.html"))
  writeLines(as.character(html_post), fileConn)
  close(fileConn)
}