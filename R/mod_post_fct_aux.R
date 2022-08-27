#' Upload post data to Googlesheets
#' @param name_poster string, name of the poster
#' @param email_poster string, email of the poster
#' @param type_post string, type of post. It can be "jobs", "services", 
#' "upcycle" or "mix"
#' @param subject string, subject of the post
#' @param description string, description of the post
#' @param contact_email string, email to contact about the post
#' @param contact_phone string, phone number to contact about the post
#' @param files_tmp dataframe that contains the names, sizes, MIME types and datapaths of the uploaded files
#' @param gdpr_acceptance boolean, whether the poster has accepted or not the
#' @param session	shiny session
#' @importFrom googledrive drive_get drive_mkdir drive_ls drive_upload drive_share_anyone
#' @importFrom googlesheets4 read_sheet sheet_append
#' @importFrom gmailr gm_mime gm_to gm_from gm_subject gm_html_body gm_attach_file gm_send_message
UploadPost <- function(name_poster,
                       email_poster,
                       type_post = c("jobs", "services", "upcycle", "mix"),
                       subject,
                       description,
                       contact_email,
                       contact_phone,
                       files_tmp,
                       gdpr_acceptance,
                       session){
  # Type of post - input control
  type_post <- match.arg(type_post)
  Ops.error <- NULL
  
  # Send post for approval
  tryCatch(
    {
      # Read googlesheets' tables
      print("Reading googlesheets database...")
      wb <- drive_get(get_golem_config("posts_path"))
      dt_approvers <- read_sheet(wb, sheet = "APPROVERS")
      dt <- read_sheet(wb, sheet = "DATABASE")
      
      # Condition depending whether the contact email is provided or not
      if (isTruthy(contact_email)) {
        cond_email <- is_valid_email(email_poster) & is_valid_email(contact_email)
      } else {
        cond_email <- is_valid_email(email_poster)
      }
      
      # Output conditioned to whether the email is valid or not
      if (cond_email) {
        # Create POST ID
        id_post <- 
          paste0("POST_", stringr::str_pad(nrow(dt) + 1, width = 6, pad = "0"))
        
        # Create googledrive directory to store data about the post
        # Store uploaded files, if any
        if (isTruthy(files_tmp)) {
          print("Creating folder to store images of the post...")
          drive_directory <- 
            paste0(dirname(get_golem_config("posts_path")), "/", id_post)
          if (id_post %in% drive_ls("posts")$name) {
            # Create aux name when there is already a folder name with the id_post
            drive_directory <- paste0(drive_directory, "_", sample(1:100, 1))
          }
          drive_mkdir(drive_directory)
          # Beautify files path
          files_names <- files_tmp$name
          files_path <- fixUploadedFilesNames(files_tmp)$datapath
          # Upload files to googledrive
          print("Uploading images to googledrive directory...")
          for (k in 1:length(files_path)) {
            files_path[k] %>% 
              drive_upload(path = drive_directory, name = files_names[k]) %>% 
              drive_share_anyone()
          }
          files_id <- paste(drive_ls(drive_directory)$id, collapse = ",")
        } else {
          files_path <- NULL
          files_id <- ""
        }
        
        # Append new entry
        print("Add new entry to database...")
        new_entry <-
          data.frame(
            ID_POST = id_post,
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
            DATE_REVISION = "",
            CONDITION = "Open"
          )
        sheet_append(ss = wb, data = new_entry, sheet = "DATABASE")
        
        # Send approval email
        print("Sending post approval request to admins...")
        # Compose email tailored to each approver
        for (j in 1:length(dt_approvers$EMAIL)) {
          print("Creating HTML approval request...")
          PostApprovalHTML(
            id_post,
            name_poster,
            email_poster,
            type_post,
            subject,
            description,
            contact_email,
            contact_phone,
            id_approver = dt_approvers$ID_APPROVER[j]
          )
          # Compose mail
          message <- 
            gm_mime() %>% 
            gm_to(dt_approvers$EMAIL[j]) %>% 
            gm_from(get_gmail_account()) %>% 
            gm_subject("A New Request Requires Your Approval!") %>% 
            gm_html_body(
              paste(
                readLines(app_sys("app/messages/request_post.html")),
                collapse = ""
              )
            )
          # Attach files, if any
          if (length(files_path) > 1) {
            for (k in 1:length(files_path)) {
              message <- gm_attach_file(message, files_path[k])
            }
          }
          # Send email
          gm_send_message(message)
        }
      } else {
        # Not successfull operation
        Ops.error <- 
          "One or both of the introduced emails are not valid,
          please try again."
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

#' Create HTML report to be sent to the post approvers
#' @param id_post string, name of the poster
#' @param name_poster string, name of the poster
#' @param email_poster string, email of the poster
#' @param type_post string, type of post.
#' @param subject string, subject of the post
#' @param description string, description of the post
#' @param contact_email string, email to contact about the post
#' @param contact_phone string, phone number to contact about the post
#' @param id_approver string, id of the post approver

PostApprovalHTML <- function(id_post,
                             name_poster,
                             email_poster,
                             type_post = c("jobs", "services", "upcycle", "mix"),
                             subject,
                             description,
                             contact_email,
                             contact_phone,
                             id_approver){
  # Type of post - input control
  type_post <- match.arg(type_post)
  
  # Pretty names - type of post
  type_post <- 
    plyr::mapvalues(type_post,
                    from = c("jobs", "services", "upcycle", "mix"),
                    c("Job opportunities", "Offer your services",
                      "Upcycle and donate", "Miscellaneous"),
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
            tags$p(style = "font-size: 18px", paste("Request #", id_post, "|", format(Sys.Date(), "%A, %d %B %Y"))),
            tags$h2("Share with your IBC church community"),
            tags$p(
              style = "text-align: left; padding-left: 10px; margin-bottom: 0;",
              "A new request requires your approval!"
            ),
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
                      style = "text-align: left; padding: 15px 10px;",
                      tags$span(style = "font-weight: bold;", "Respondent email")
                    ),
                    tags$td(
                      style = "text-align: left; padding: 15px 10px;",
                      tags$a(
                        href = paste0("mailto:", email_poster),
                        email_poster
                      )
                    )
                  ),
                  tags$tr(
                    style = "border-top: 1px solid #dddddd;",
                    tags$td(
                      style = "text-align: left; padding: 15px 10px; font-weight: bold;",
                      "Respondent name"
                    ),
                    tags$td(
                      style = "text-align: left; padding: 15px 10px;",
                      name_poster
                    )
                  ),
                  tags$tr(
                    style = "border-top: 1px solid #dddddd;",
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
                      subject
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
                      description
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
                      contact_email
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
                      contact_phone
                    )
                  )
                )
              )
            )
          ),
          tags$div(
            style = "margin-top: 25px; text-align: center;",
            tags$div(
              style = "margin-bottom: 10px;",
              tags$a(
                style = "display: block;
                        width: 40%;
                        margin: 0 auto;
                        padding: 10px 10px;
                        color: #ccc;
                        text-decoration: none;
                        font-family: Arial, Helvetica, sans-serif;
                        font-size: 16px;
                        color: white;
                        background-color: #88be00;
                        border: 2px solid #88be00;",
                target = "_blank",
                href = 
                  paste0(
                    get_golem_config("app_url"),
                    "/?tab=approval&id_request=",
                    id_post,
                    "&id_approver=",
                    id_approver
                  ),
                "Approve/Reject"
              )
            )
          ),
          hr(),
          p(strong("Social Ministry Team")),
          tags$img(
            src = get_golem_config("ibc_logo_url"),
            width = 200, height = 50)
        )
      )
    )
  fileConn <- file(app_sys("app/messages/request_post.html"))
  writeLines(as.character(html_post), fileConn)
  close(fileConn)
}

