# What are we testing?
# Subscription workflow - More tab
# - User subscribes to a given set of categories
# - User unsubscribes to a given set of categories
# In both cases a confirmation mail is sent to the user

# Load required libraries
library(shinytest2)

# Auxiliary functions
skip_if_no_token <- function() {
  testthat::skip_if_not(
    googledrive::drive_has_token() &
      googlesheets4::gs4_has_token() &
      gmailr::gm_has_token(), "No token")
}
secret_can_decrypt <- utils::getFromNamespace("secret_can_decrypt", "gargle")

# Auxiliary steps
testing_email <- 
  get_golem_config("testing_email", config = "default")

# Connect to APIs
ApiConnections("default")

# Read development mailing list worksheet
if (secret_can_decrypt("shareIBC")) {
  wb <- 
    googledrive::drive_get(
      get_golem_config("mailing_list_path", config = "default")
    )
  check_cols <-
    c("ACTIVE_JOBS", "ACTIVE_SERVICES", "ACTIVE_UPCYCLE", "ACTIVE_MIX")
}

test_that("Check is_valid_email function...", {
  skip_if_no_token()
  # Check valid emails
  expect_true(is_valid_email("email@example.com"))
  expect_true(is_valid_email("firstname.lastname@example.com"))
  expect_true(is_valid_email("email@subdomain.example.com"))
  expect_true(is_valid_email("firstname-lastname@example.com"))
  expect_true(is_valid_email("email@example-one.com"))
  
  # Check invalid emails
  expect_false(is_valid_email("plainaddress"))
  expect_false(is_valid_email("@example.com"))
  expect_false(is_valid_email("Joe Smith <email@example.com>"))
  expect_false(is_valid_email("email.example.com"))
  expect_false(is_valid_email("email@example@example.com"))
  expect_false(is_valid_email("email@example.com (Joe Smith)"))
  expect_false(is_valid_email("email@example"))
})


# Make sure that you are executing these tests on the development environment
test_that("Test subscription functions...", {
  skip_if_no_token()
  # Email not registered on the mailing list
  expect_true(
    SubscribeEmail(
      name = charlatan::ch_name(),
      email = testing_email,
      mailing_lists = c("services", "mix"),
      session = NULL
    )$success
  )
  # Check successful database update
  dt <- googlesheets4::read_sheet(wb)
  expect_equal(
    unname(unlist(dt[check_cols])),
    c(FALSE, TRUE, FALSE, TRUE)
  )
  
  # Email already registered on the mailing list
  expect_true(
    SubscribeEmail(
      name = charlatan::ch_name(),
      email = testing_email,
      mailing_lists = c("jobs", "upcycle"),
      session = NULL
    )$success
  )
  # Check successful database update
  dt <- googlesheets4::read_sheet(wb)
  expect_equal(
    unname(unlist(dt[check_cols])),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  
  # Invalid email
  expect_false(
    SubscribeEmail(
      name = charlatan::ch_name(),
      email = "@example.com",
      mailing_lists = c("services", "mix"),
      session = NULL
    )$success
  )
})

test_that("Test unsubscription functions...", {
  skip_if_no_token()
  # Email registered on the mailing list
  expect_true(
    UnsubscribeEmail(
      email = testing_email,
      mailing_lists = c("jobs", "upcycle"),
      session = NULL
    )$success
  )
  expect_true(
    UnsubscribeEmail(
      email = testing_email,
      mailing_lists = c("mix", "services"),
      session = NULL
    )$success
  )
  # Check successful database update
  dt <- googlesheets4::read_sheet(wb)
  expect_equal(
    unname(unlist(dt[check_cols])),
    c(FALSE, FALSE, FALSE, FALSE)
  )
  
  # Email NOT registered on the mailing list
  expect_false(
    UnsubscribeEmail(
      email = "email@example.com",
      mailing_lists = c("jobs", "upcycle"),
      session = NULL
    )$success
  )
  
  # Remove register
  googlesheets4::range_delete(wb, sheet = "DATABASE", range = "2")
})

# Open headless app
if (secret_can_decrypt("shareIBC")) {
  app <- AppDriver$new(app_dir = testthat::test_path("apps"))
}

test_that("App subscription process checkup...", {
  skip_if_no_token()
  # Open more tabset
  app$set_inputs(main_tabset = "More")
  
  # Subscription process
  app$set_inputs(`more_ui-more_accordion` = c("TRUE", "Subscribe"))
  app$set_inputs(`more_ui-name_subs` = charlatan::ch_name())
  app$set_inputs(`more_ui-email_subs` = testing_email)
  app$set_inputs(`more_ui-ml_subs` = c("jobs", "services", "upcycle", "mix"))
  app$click("more_ui-subscribeBtn", timeout_ = 10 * 1000)
  # 'more_ui-ml_subs' field does not get restarted - shinytest2 issue!
  # Incompatibility with custom JS function
  expect_identical(app$get_value("more_ui-name_subs"), "")
  expect_identical(app$get_value("more_ui-email_subs"), "")
  expect_identical(
    app$get_value("more_ui-ml_subs"),
    c("jobs", "services", "upcycle", "mix")
  )
  expect_identical(app$get_value("more_ui-subscribeBtn"), "1")
  
  # expect_identical(
  #   GetInputs(
  #     app,
  #     c("more_ui-name_subs", "more_ui-email_subs",
  #       "more_ui-ml_subs", "more_ui-subscribeBtn")
  #   ),
  #   c("", "", "jobs", "services", "upcycle", "mix", "1")
  # )
  
  # Unsubscription process
  app$set_inputs(`more_ui-more_accordion` = c("TRUE", "Unsubscribe"))
  app$set_inputs(`more_ui-email_unsubs` = testing_email)
  app$set_inputs(`more_ui-ml_unsubs` = c("jobs", "services", "upcycle", "mix"))
  app$click("more_ui-unsubscribeBtn", timeout_ = 10 * 1000)
  # 'more_ui-ml_subs' field does not get restarted - shinytest2 issue!
  # Incompatibility with custom JS function
  # expect_identical(
  #   GetInputs(
  #     app,
  #     c("more_ui-email_unsubs", "more_ui-ml_unsubs", "more_ui-unsubscribeBtn")
  #   ),
  #   c("", "jobs", "services", "upcycle", "mix", "1")
  # )
})

if (secret_can_decrypt("shareIBC")) {
  # Finish headless app
  app$stop()
  # Remove register
  googlesheets4::range_delete(wb, sheet = "DATABASE", range = "2")
}