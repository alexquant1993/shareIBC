# 1) Subscription workflow - More tab
# 1.1) User subscribes to a given set of categories
# 1.2) User unsubscribes to a given set of categories
# In both cases a confirmation mail is sent to the user

# 2) Approval workflow - Post tab
# 2.1) User makes a post and is sent for approval
# - Emails with the post details are sent to the list of approvers
# - A confirmation email of the post is sent to the poster, acknowledging that
#   it will be sent for approval.
# 2.2) Approver receives the mail and approves/rejects the post
# - An email is sent to the poster communicating whether the post 
#   was approved or rejected.
# - An email is sent to the current/active mailing list for the specific
#   category of the post.

# 3) Check searchbar in the Share Tab. Use shinytest snapshot

# 4) Other tabs - check downloading forms

test_that("Check share aux functions", {
  # Check f7Post function
  f7_post <- 
    f7Post(
      subject = "Test subject",
      date = "2022-08-13",
      content = random_text(nwords = 10),
      footer = tagList(
        f7Row(inlineBlock(icon = "envelope", "random@email.com")),
        f7Row(inlineBlock(icon = "phone", "695216785"))
      )
    )
  expect_identical(digest(f7_post), "1bb0a7625a5f253a2ea9da51f9a6d867")
  
  # GetPostData function
  
  
})
