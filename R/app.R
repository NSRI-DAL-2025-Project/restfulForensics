library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinybusy)
library(plotly)
library(dplyr)

   options(shiny.maxRequestSize = 5000 * 1024^2)
   shiny::addResourcePath(
      "restful-www",
      system.file(
         "www", package = "restfulForensics"
      )
   )
   
   shiny::shinyApp(
      ui = app_ui,
      server = app_server
   )
    