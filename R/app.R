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