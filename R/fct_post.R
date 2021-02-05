#' Upload post data to googledrive
#' @param name_poster string, name of the poster
#' @param email_poster string, email of the poster
#' @param type_post string, type of post. It can be "jobs" or "services"
#' @param subject string, subject of the post
#' @param description string, description of the post
#' @param contact_email string, email to contact about the post
#' @param contact_phone string, phone number to contact about the post
#' @param files_tmp dataframe that contains the names, sizes, MIME types and datapaths of the uploaded files
#' @param gdpr_acceptance boolean, whether the poster has accepted or not the
#' @importFrom googledrive drive_get drive_mkdir drive_ls
#' @importFrom googlesheets4 read_sheet sheet_append
#' general data protection regulation.
UploadPost <- function(name_poster,
                       email_poster,
                       type_post = c("jobs", "services"),
                       subject,
                       description,
                       contact_email,
                       contact_phone,
                       files_tmp,
                       gdpr_acceptance){
  # Read googlesheet
  wb <- drive_get("posts/DT_POSTS")
  dt <- read_sheet(wb, sheet = "DATABASE")
  dt_approvers <- read_sheet(wb, sheet = "APPROVERS")
  # Condition depending whether the contact email is provided or not
  if (isTruthy(contact_email)) {
    cond_email <- is_valid_email(email_poster) & is_valid_email(contact_email)
  } else {
    cond_email <- is_valid_email(email_poster)
  }
  # Output conditioned to whether the email is valid or not
  if (cond_email) {
    # Successfull operation
    success <- TRUE
    Ops.error <- NULL
    id_post <- paste0("POST_", stringr::str_pad(nrow(dt) + 1, width = 6, pad = "0"))
    # Create googledrive directory to store data about the post
    # Store uploaded files, if any
    if (isTruthy(files_tmp)) {
      drive_directory <- paste0("posts/", id_post)
      drive_mkdir(drive_directory)
      # Beautify files path
      files_names <- files_tmp$name
      files_path <- fixUploadedFilesNames(files_tmp)$datapath
      # Upload files to googledrive
      for (k in 1:length(files_path)) {
        drive_upload(files_path[k], path = drive_directory, name = files_names[k])
      }
      files_id <- paste(drive_ls(drive_directory)$id, collapse = ",")
    } else {
      files_path <- NULL
      files_id <- ""
    }
    # Append new entry
    new_entry <-
      data.frame(ID_POST = id_post,
                 NAME_POSTER = name_poster,
                 EMAIL_POSTER = email_poster,
                 TYPE_OFFER = type_post,
                 SUBJECT = subject,
                 DESCRIPTION = description,
                 CONTACT_EMAIL = contact_email,
                 CONTACT_PHONE = contact_phone,
                 FILES_ID = files_id,
                 GDPR_ACCEPTANCE = gdpr_acceptance,
                 DATE_POST = as.character(Sys.time()),
                 STATUS = "In progress",
                 ID_APPROVER = "",
                 COMMENTS_APPROVER = "",
                 DATE_REVISION = "")
    sheet_append(ss = wb, data = new_entry, sheet = "DATABASE")
    # Get HTML email for approval
    PostApprovalHTML(id_post,
                     name_poster,
                     email_poster,
                     type_post,
                     subject,
                     description,
                     contact_email,
                     contact_phone)
    # Send approval email
    send.mail(from = "Social Ministry IBC <jobs@ibcmadrid.com>",
              to = dt_approvers$EMAIL,
              subject = "A New Request Requires Your Approval!",
              body = app_sys("app/messages/page_approval.html"),
              html = TRUE,
              inline = T,
              smtp = list(host.name = "smtp.yandex.com", port =  465,
                          user.name = "jobs@ibcmadrid.com",
                          passwd = "fAC3pGdUnd1", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE,
              attach.files = files_path)
  } else {
    # Not successfull operation
    success <- FALSE
    Ops.error <- "One or both of the introduced emails are not valid, please try again."
  }
  return(
    list(success = success,
         Ops.error = Ops.error)
  )
}

#' Create HTML report to approve post
#' @param id_post string, name of the poster
#' @param name_poster string, name of the poster
#' @param email_poster string, email of the poster
#' @param type_post string, type of post. It can be "jobs" or "services"
#' @param subject string, subject of the post
#' @param description string, description of the post
#' @param contact_email string, email to contact about the post
#' @param contact_phone string, phone number to contact about the post
PostApprovalHTML <- function(id_post,
                             name_poster,
                             email_poster,
                             type_post = c("jobs", "services"),
                             subject,
                             description,
                             contact_email,
                             contact_phone){
  # Pretty type of post names
  type_post <- 
    plyr::mapvalues(type_post,
                    from = c("jobs", "services"),
                    c("Job opportunities", "Offer your services"),
                    warn_missing = FALSE)
  # Create custom HTML doc
  html_post <-
    tags$html(
      lang = "en",
      htmltools::includeCSS(app_sys("app/messages/post_approval.css")),
      tags$body(
        tags$div(
          class = "container",
          tags$div(
            class = "wrapper",
            style = "width:70%;text-align:center;border:1px solid #dddddd;border-radius:5px;padding:10px 50px;margin:0 auto 20px",
            tags$p(style = "font-size: 18px", paste("Request #", id_post, "|", format(Sys.Date(), "%A, %d %B %Y"))),
            tags$h2("Post a job opportunity or offer your services"),
            tags$p(class = "message", "A new request requires your approval!"),
            tags$div(
              class = "responses-table",
              tags$table(
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
                      tags$span(class = "respondent", "Respondent email")
                    ),
                    tags$td(
                      tags$a(
                        href = paste0("mailto:", email_poster),
                        email_poster
                      )
                    )
                  ),
                  tags$tr(
                    class = "response-row",
                    tags$td(
                      "Respondent name"
                    ),
                    tags$td(
                      name_poster
                    )
                  ),
                  tags$tr(
                    class = "response-row",
                    tags$td(
                      "Type of post"
                    ),
                    tags$td(
                      type_post
                    )
                  ),
                  tags$tr(
                    class = "response-row",
                    tags$td(
                      "Subject of the post"
                    ),
                    tags$td(
                      subject
                    )
                  ),
                  tags$tr(
                    class = "response-row",
                    tags$td(
                      "Description of the post"
                    ),
                    tags$td(
                      description
                    )
                  ),
                  tags$tr(
                    class = "response-row",
                    tags$td(
                      "Contact information - email"
                    ),
                    tags$td(
                      contact_email
                    )
                  ),
                  tags$tr(
                    class = "response-row",
                    tags$td(
                      "Contact information - phone number"
                    ),
                    tags$td(
                      contact_phone
                    )
                  )
                )
              )
            )
          ),
          tags$div(
            class = "button-group",
            tags$div(
              class = "buttonWrapper",
              tags$a(
                class = "button approveButton",
                target = "_blank",
                href = "http://178.62.110.168:3838/?unsubscribe=TRUE",
                "Approve"
              )
            ),
            tags$div(
              class = "buttonWrapper",
              style = "margin-bottom: 10px;",
              tags$a(
                class = "button rejectButton",
                target = "_blank",
                href = "http://178.62.110.168:3838/?unsubscribe=TRUE",
                "Reject"
              )
            )
          )
        )
      )
    )
  fileConn <- file(app_sys("app/messages/page_approval.html"))
  writeLines(as.character(html_post), fileConn)
  close(fileConn)
}

