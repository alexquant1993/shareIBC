library(shareIBC)
library(shiny)
shareIBC:::ApiConnections("production")
shiny::shinyApp(shareIBC:::app_ui, shareIBC:::app_server)