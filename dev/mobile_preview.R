# Mobile preview execution

# Libraries
library(shiny)
library(shinyMobile)

# Run in R console
preview_mobile(getwd(), device = "iphoneX")

# Run in terminal
# R -e "socialministryapp::run_app(options=list(port = 3838))"

# To stop the preview
# - Ctrl C in terminal
# - Right click and reload in console
# - Stop button