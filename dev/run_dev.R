# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()

# Mobile preview execution

# Libraries
library(shiny)
library(shinyMobile)

# Run in R console
preview_mobile(getwd(), device = "iphoneX")

# Run in terminal
# R -e "shareIBC::run_app(options=list(port = 3838))"

# To stop the preview
# - Ctrl C in terminal
# - Right click and reload in console
# - Stop button