classification_server <- function(input, output, session, rv){
   
   # ================== CLASSIFICATION =====================#
   
   classificationRef <- data.frame(
      Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
      Population = c("POP1", "POP2", "POP3", "POP4", "..."),
      rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
      rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
      rs_n = c("...", "...", "...", "...", "...")
   )
   
   output$classificationRef_UI <- DT::renderDataTable(
      {
         req(classificationRef)
         classificationRef
      },
      options = list(
         scrollX = TRUE,
         pageLength = 5
      )
   )
   
   observe({
      file_ready <- !is.null(input$forPredFile)
      shinyjs::toggleState("runNaiveBayes", condition = file_ready)
   })
   
   predResults <- reactiveVal(NULL)
   
   observeEvent(input$runNaiveBayes, {
      disable("runNaiveBayes")
      req(input$forPredFile)
      
      result <- calculate_naive_bayes(input$forPredFile$datapath)
      predResults(result)
   })
   
   output$predictionTableResult <- renderPrint({
      req(predResults())
      predResults()$predTable
   })
   
   output$statbyClassResult <- renderPrint({
      req(predResults())
      predResults()$otherStat
   })
   
   output$overallStatResult <- renderPrint({
      req(predResults())
      predResults()$predStat
   })
   
   output$downloadClassification <- downloadHandler(
      filename = function() {
         timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
         paste0("classification-results_", timestamp, ".xlsx")
      },
      content = function(file) {
         dataset <- list(
            "Table" = as.data.frame(predResults()$predTable),
            "Stats per Class" = as.data.frame(predResults()$otherStat),
            "Overall Stats" = as.data.frame(predResults()$predStat)
         )
         openxlsx::write.xlsx(dataset, file = file)
      }
   )
   
   output$downloadClassification_UI <- renderUI({
      req(predResults())
      downloadButton("downloadClassification", "Download Results")
   })
   
}