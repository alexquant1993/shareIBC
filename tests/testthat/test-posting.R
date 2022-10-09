# What are we testing?
# Approval workflow - Post tab
# 1) User makes a post and is sent for approval
# - Emails with the post details are sent to the list of approvers
# - A confirmation email of the post is sent to the poster, acknowledging that
#   it will be sent for approval.

# Read development posting databases
if (secret_can_decrypt("shareIBC")) {
  # Mailing distribution list database
  wb_ml <- 
    googledrive::drive_get(
      get_golem_config("mailing_list_path", config = "default")
    )
  # Remove all previous records
  googlesheets4::range_delete(wb_ml, sheet = "DATABASE", range = "2:1048576")
  # Add a subscriber to test the approval workflow
  SubscribeEmail(
    name = charlatan::ch_name(),
    email = testing_email,
    mailing_lists = c("jobs", "services", "upcycle", "mix"),
    session = NULL
  )
  # Posts database
  wb <- 
    googledrive::drive_get(
      get_golem_config("posts_path", config = "default")
    )
  # Remove all previous records
  googlesheets4::range_delete(wb, sheet = "DATABASE", range = "2:1048576")
  # Open headless app
  app <- AppDriver$new(app_dir = testthat::test_path("apps"))
}

test_that("Posting workflow - send post for approval...", {
  skip_if_no_token()
  # Open the post tab, select the type randomly
  app$click("post_ui-bttn_post")
  app$set_inputs(`post_ui-popup_post` = TRUE)
  type_post <- sample(type_of_posts, 1)
  post_ui <- paste0("post_ui-", type_post, "-")
  app$click(paste0(post_ui, "bttn_post_tab"))
  
  # Fill out the fields randomly
  app$set_inputs(!!paste0(post_ui, "popup_post_tab") := TRUE)
  name_subs <- charlatan::ch_name()
  app$set_inputs(!!paste0(post_ui, "name_poster") := name_subs)
  app$set_inputs(!!paste0(post_ui, "email_poster") := testing_email)
  app$set_inputs(
    !!paste0(post_ui, "subject") := shinipsum::random_text(nwords = 5)
  )
  app$set_inputs(
    !!paste0(post_ui, "description") := shinipsum::random_text(nwords = 20)
  )
  app$set_inputs(!!paste0(post_ui, "contact_email") := "email@example.com")
  contact_phone <- charlatan::ch_phone_number(locale = "es_ES")
  app$set_inputs(!!paste0(post_ui, "contact_phone") := contact_phone)
  app$upload_file(!!paste0(post_ui, "attach_post") := RandomPic())
  app$set_inputs(!!paste0(post_ui, "check_rgpd_post") := TRUE)
  
  # Submit post for approval
  app$click(paste0(post_ui, "submit_post"), timeout_ = 30 * 1000)
  
  # Check if fields are properly populated
  dt <- googlesheets4::read_sheet(wb, sheet = "DATABASE")
  exp_list <-
    list(
      ID_POST = "POST_000000001",
      NAME_POSTER = name_subs,
      EMAIL_POSTER = testing_email,
      TYPE_POST = type_post,
      SUBJECT = shinipsum::random_text(nwords = 5),
      DESCRIPTION = shinipsum::random_text(nwords = 20),
      CONTACT_EMAIL = "email@example.com",
      CONTACT_PHONE = contact_phone,
      GDPR_ACCEPTANCE = TRUE,
      STATUS = "In progress",
      CONDITION = "Open"
    )
  expect_identical(
    as.list(dt[names(dt) %in% names(exp_list)]),
    exp_list
  )
})

# 2) Approver receives the mail and approves/rejects the post
# - An email is sent to the poster communicating whether the post 
#   was approved or rejected.
# - An email is sent to the current/active mailing list for the specific
#   category of the post.
if (secret_can_decrypt("shareIBC")) {
  # Open headless app
  app2 <- 
    AppDriver$new(
      paste0(
        app$get_url(),
        "?tab=approval&id_request=POST_000000001&id_approver=APV_01"
      )
    )
}

test_that("Posting workflow - accept/reject post...", {
  skip_if_no_token()
  # Approve post
  app2$set_inputs(`approval_ui-comment` = shinipsum::random_text(nwords = 10))
  app2$click("approval_ui-request_approve", timeout_ = 30 * 1000)
  # Check if fields are properly populated
  dt <- googlesheets4::read_sheet(wb, sheet = "DATABASE")
  exp_list <-
    list(
      STATUS = "Approved",
      ID_APPROVER = "APV_01",
      COMMENTS_APPROVER = shinipsum::random_text(nwords = 10)
    )
  expect_identical(
    as.list(dt[names(dt) %in% names(exp_list)]),
    exp_list
  )
  
  # Try to reject a post that already has been approved
  app2$set_inputs(`approval_ui-comment` = shinipsum::random_text(nwords = 10))
  app2$click("approval_ui-request_reject", timeout_ = 30 * 1000)
  # A post that already has been approved cannot be rejected
  dt <- googlesheets4::read_sheet(wb, sheet = "DATABASE")
  expect_identical(
    as.list(dt[names(dt) %in% names(exp_list)]),
    exp_list
  )
  
  # Reset status to "In progress" so we can test the rejection process
  googlesheets4::range_write(
    wb,
    data = data.frame("In progress"),
    sheet = "DATABASE",
    range = "L2",
    col_names = FALSE
  )
  # Reject post
  app2$set_inputs(`approval_ui-comment` = shinipsum::random_text(nwords = 10))
  app2$click("approval_ui-request_reject", timeout_ = 30 * 1000)
  dt <- googlesheets4::read_sheet(wb, sheet = "DATABASE")
  expect_identical(dt$STATUS, "Rejected")
})

if (secret_can_decrypt("shareIBC")) {
  # Remove created registers and folders
  googlesheets4::range_delete(wb_ml, sheet = "DATABASE", range = "2")
  googlesheets4::range_delete(wb, sheet = "DATABASE", range = "2")
  googledrive::drive_rm(
    paste0(
      dirname(get_golem_config("posts_path")),
      "/POST_000000001"
    )
  )
  # Close both headless apps
  app$stop()
  app2$stop()
}
