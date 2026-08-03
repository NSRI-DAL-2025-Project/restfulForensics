plot_structure_server <- function(input, output, session, rv) {
   
   structureResults <- reactiveVal(NULL)
   
   observe({
      file_ready <- !is.null(input$zippedMatrices)
      shinyjs::toggleState("plotStructureResults", condition = file_ready)
   })
   
   output.dir <- tempdir()
   
   observeEvent(input$plotStructureResults, {
      disable("plotStructureResults")
      req(input$zippedMatrices)
      Sys.sleep(1.5)
      
      # create a folder and unpack the zipped files there
      str_results <- unpack_input_file(input$zippedMatrices, output.dir = output.dir)
      structureResults(str_results)
      
      

   })
}