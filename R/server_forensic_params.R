forensic_params_server <- function(input, output, session, rv){
   
   # =============== FORENSIC PARAMETERS ===================#
   
   referenceData <- data.frame(
      Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
      Population = c("POP1", "POP2", "POP3", "POP4", "..."),
      rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
      rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
      rs_n = c("...", "...", "...", "...", "...")
   )
   
   afSample <- data.frame(
      markers = c("rs101.A", "rs101.T", "rs102.C", "rs102.G", "..."),
      POP1 = c("0.18518", "0.81481", ".77777", "0.22222", "..."),
      POP2 = c("0.89285", "0.10714", "0.89285", "0.10714", "..."),
      POP3 = c("0.15789", "0.84210", "0.87894", "0.12105", "..."),
      POPn = c("...", "...", "...", "...", "...")
   )
   
   profileSample <- data.frame(
      markers = c("rs101", "rs102", "rs103", "rs104", "..."),
      profile = c("A/T", "G/C", "G/A", "T/T", "...")
   )
   
   output$referenceData_UI <- DT::renderDataTable(
      {
         req(referenceData)
         referenceData
      },
      options = list(
         scrollX = TRUE,
         pageLength = 5
      )
   )
   
   output$afSample_UI <- DT::renderDataTable(
      {
         req(afSample)
         afSample
      },
      options = list(
         scrollX = TRUE,
         pageLength = 5
      )
   )
   
   output$profileSample_UI <- DT::renderDataTable(
      {
         req(profileSample)
         profileSample
      },
      options = list(
         scrollX = TRUE,
         pageLength = 5
      )
   )
   
   results_rv <- reactiveValues(
      overall_metrics = NULL,
      pop_metrics = NULL,
      rmp_value = NULL
   )
   genotype_freqs <- reactiveVal(NULL)
   gt_freq_all <- reactiveVal(NULL)
   gt_freq_pop <- reactiveVal(NULL)
   snpsFile <- reactiveVal(NULL)
   
   observe({
      shinyjs::toggleState("calcIISNPs", !is.null(input$iisnpsFile))
   })
   
   observeEvent(input$calcIISNPs, {
      shinyjs::disable("calcIISNPs")
      req(input$iisnpsFile)
      
      fileUploaded <- load_csv_xlsx_files(input$iisnpsFile$datapath)
      data_type <- evaluate_file(fileUploaded)
      snpsFile(fileUploaded)
      computed_af <- NULL
      pop <- NULL
      
      if (data_type == "gts") {
         file <- clean_input_data(snpsFile())
         file <- convert_to_genind(file, to_str = FALSE, popinfo = TRUE)
         computed_af <- compute_af(file)
         pop <- nrow(file)
      } else if (data_type == "freqs") {
         computed_af <- snpsFile()
      }
      
      profile_df <- NULL
      theta <- 0
      pop <- NULL
      if (!is.null(input$fileProfile)) {
         profile_df <- load_csv_xlsx_files(input$fileProfile$datapath)
         theta <- input$thetaValue
      }
      
      if (!is.null(input$floorCeiling)) {
         pop <- input$totalPop
      }
      
      gt_freqs <- calc_genotype_freq(computed_af, pop = pop)
      gt_freq_all(gt_freqs$gt_complete)
      gt_freq_pop(gt_freqs$gt_by_pop)
      
      res <- calc_iisnps_params(gt_freq_all(), profile = profile_df, theta = theta)
      
      
      if (!is.null(profile_df)) {
         results_rv$rmp_value <- res$RMP_profile
         results_rv$overall_metrics <- res$marker_metrics
         results_rv$pop_metrices <- NULL
      } else {
         results_rv$overall_metrics <- res$overall
         results_rv$pop_metrics <- res$by_population
         results_rv$rmp_value <- NULL
      }
      shinyjs::enable("calcIISNPs")
   })
   
   observe({
      req(results_rv$pop_metrics, results_rv$overall_metrics)
      
      updateSelectInput(
         session,
         "selected_pop",
         choices = c(
            "Overall",
            names(results_rv$pop_metrics)
         )
      )
   })
   
   observe({
      req(gt_freq_pop())
      
      updateSelectInput(
         session,
         "selected_pop_gt",
         choices = c(
            "Overall",
            names(gt_freq_pop())
         )
      )
   })
   
   output$genotypeFreqs_UI <- DT::renderDataTable({
      req(gt_freq_pop(), gt_freq_all())
      
      if (input$selected_pop == "Overall") {
         req(gt_freq_all())
         DT::datatable(gt_freq_all(), rownames = FALSE)
      } else {
         req(gt_freq_pop())
         DT::datatable(gt_freq_pop()[[input$selected_pop_gt]],
                       rownames = FALSE
         )
      }
   })
   
   output$popTable <- DT::renderDataTable({
      req(input$selected_pop)
      
      if (input$selected_pop == "Overall") {
         req(results_rv$overall_metrics)
         DT::datatable(results_rv$overall_metrics, rownames = FALSE)
      } else {
         req(results_rv$pop_metrics)
         DT::datatable(results_rv$pop_metrics[[input$selected_pop]],
                       rownames = FALSE
         )
      }
   })
   
   output$downloadMetrics <- downloadHandler(
      filename = function() {
         paste0("forensic_metrics_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
         req(results_rv$overall_metrics, results_rv$pop_metrics, gt_freq_pop(), gt_freq_all())
         sheets <- list()
         sheets[["Forensic Params (FP)"]] <- results_rv$overall_metrics
         pop_sheets <- results_rv$pop_metrics
         names(pop_sheets) <- paste0("FP_", substr(gsub(
            "[^A-Za-z0-9]", "_",
            names(pop_sheets)
         ), 1, 25))
         sheets <- c(sheets, pop_sheets)
         
         sheets[["Genotype Frequency (GF)"]] <- gt_freq_all()
         gt_pop <- gt_freq_pop()
         names(gt_pop) <- paste0("FP_", substr(gsub(
            "[^A-Za-z0-9]", "_",
            names(gt_pop)
         ), 1, 25))
         sheets <- c(sheets, gt_pop)
         writexl::write_xlsx(sheets, path = file)
      }
   )
   
   output$downloadRMP <- downloadHandler(
      filename = function() {
         paste0("RMP_profile_", Sys.Date(), ".csv")
      },
      content = function(file) {
         req(results_rv$rmp_value)
         write.csv(data.frame(RMP = results_rv$rmp_value), file, row.names = FALSE)
      }
   )
   
   output$downloadMetrics_UI <- renderUI({
      req(results_rv$overall_metrics)
      downloadButton("downloadMetrics", "Download Forensic Parameters")
   })
   
   output$downloadRMP_UI <- renderUI({
      req(results_rv$rmp_value)
      downloadButton("downloadRMP", "Download RMP")
   })
   
}