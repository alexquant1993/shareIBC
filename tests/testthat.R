library(shareIBC)
library(shinytest2)
library(testthat)

test_check("shareIBC")
# shinytest2::test_app()

# test_dir(
#   "./testthat",
#   # Run in the app's environment containing all support methods.
#   env = shiny::loadSupport(),
#   # Display the regular progress output and throw an error if any test error is found
#   reporter = c("progress", "fail")
# )
