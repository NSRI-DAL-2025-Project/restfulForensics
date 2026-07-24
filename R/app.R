options(shiny.maxRequestSize = 5000 * 1024^2)

#' Run restful Forensics app
#' 
#' @import shiny
#' @import shinyjs
#' @import shinydashboard
#' @import shinybusy
#' @import dplyr
#' 
#' @export
run_app <- function(){
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
    }