pop_stats_server <- function(input, output, session, rv) {
   
   # =============== POPULATION STATISTICS =================#
   
   examplePop <- data.frame(
      Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
      Population = c("POP1", "POP2", "POP3", "POP4", "..."),
      rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
      rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
      rs_n = c("...", "...", "...", "...", "...")
   )
   
   output$examplePop_UI <- DT::renderDataTable(
      {
         req(examplePop)
         examplePop
      },
      options = list(
         scrollX = TRUE,
         pageLength = 5
      )
   )
   
   observe({
      file_ready <- !is.null(input$popStatsFile)
      shinyjs::toggleState("runPopStats", condition = file_ready)
   })
   
   privAlleles <- reactiveVal(NULL)
   popStats <- reactiveVal(NULL)
   hardyWeinberg <- reactiveVal(NULL)
   fstStats <- reactiveVal(NULL)
   fstData <- reactiveVal(NULL)
   afData <- reactiveVal(NULL)
   statsMatrix <- reactiveVal(NULL)
   hwMatrix <- reactiveVal(NULL)
   fstMatrix <- reactiveVal(NULL)
   
   fsnps_gen <- reactive({
      req(input$popStatsFile)
      df <- load_csv_xlsx_files(input$popStatsFile$datapath)
      cleaned <- clean_input_data(df)
      convert_to_genind(cleaned, to_str = FALSE, popinfo = TRUE)
   })
   
   observeEvent(input$runPopStats, {
      disable("runPopStats")
      req(input$popStatsFile)
      
      withProgress(message = "Running population analysis...", value = 0, {
         tryCatch(
            {
               req(fsnps_gen())
               
               incProgress(0.4, detail = "Computing private alleles...")
               priv_alleles <- poppr::private_alleles(fsnps_gen())
               if (is.null(priv_alleles)) {
                  priv_alleles <- list(message = "No private alleles detected")
               } else {
                  priv_alleles <- data.frame(rownames(priv_alleles), priv_alleles)
                  priv_alleles <- dplyr::rename(priv_alleles, Pop = 1)
                  rownames(priv_alleles) <- NULL
               }
               privAlleles(priv_alleles)
               
               incProgress(0.6, detail = "Computing population statistics...")
               population_stats <- compute_pop_stats(fsnps_gen())
               popStats(population_stats)
               
               ## AF
               af_stats <- compute_af(fsnps_gen())
               afData(af_stats)
               
               ## HWE
               incProgress(0.8, detail = "Running HWE and FST calculations...")
               hardy_weinberg_stats <- compute_hwe(fsnps_gen(), correction = input$correctionModel, alpha = input$alphaValue)
               hardyWeinberg(hardy_weinberg_stats)
               
               ## FST
               incProgress(1.0, detail = "Still running HWE and FST calculations...")
               fst_stats <- compute_fst(fsnps_gen())
               fstStats(fst_stats)
               fst_data <- fstStats()$fst_dataframe
               fstData(fst_data)
               
               showNotification("Rendering outputs, this might take some time...", type = "message", duration = 30)
               print(Sys.time())
               
               enable("runPopStats")
            },
            error = function(e) {
               showNotification(paste("Population stats error:", e$message), type = "error")
               enable("runPopStats")
            }
         )
         
         stats_matrix <- compute_pop_stats(fsnps_gen())
         hw_matrix <- compute_hwe(fsnps_gen())
         fst_matrix <- compute_fst(fsnps_gen())
         statsMatrix(stats_matrix)
         hwMatrix(hw_matrix)
         fstMatrix(fst_matrix)
      })
   })
   
   output$privateAlleleTable <- DT::renderDataTable(
      {
         as.data.frame(privAlleles())
      },
      options = list(pageLength = 10, scrollX = TRUE)
   )
   
   output$meanallelic <- DT::renderDataTable({
      popStats()$mar_list
   })
   
   output$heterozygosity_table <- DT::renderDataTable({
      popStats()$heterozygosity
   })
   
   output$heterozygosity_plot <- renderImage(
      {
         req(popStats()$heterozygosity)
         
         plot_path <- plot_heterozygosity(
            Het_fsnps_df = popStats()$heterozygosity,
            out_dir = tempdir()
         )
         list(
            src = plot_path,
            contentType = "image/png",
            alt = "Heterozygosity Plot",
            width = "100%"
         )
      },
      deleteFile = TRUE
   )
   
   output$inbreeding_table <- DT::renderDataTable({
      popStats()$inbreeding_coeff
   })
   
   output$ttest_table <- DT::renderDataTable({
      popStats()$ttest
   })
   
   output$allele_freq_table <- DT::renderDataTable(
      {
         afData()
      },
      options = list(scrollX = TRUE)
   )
   
   
   output$hwe_summary_text <- DT::renderDataTable(
      {
         hardyWeinberg()$hw_summary
      },
      options = list(scrollX = TRUE)
   )
   
   output$hwe_chisq_table <- DT::renderDataTable(
      {
         hardyWeinberg()$hw_dataframe
      },
      options = list(scrollX = TRUE)
   )
   
   output$hwe_loci <- DT::renderDataTable(
      {
         hardyWeinberg()$loci_HWE_failure
      },
      options = list(scrollX = TRUE)
   )
   
   output$hwe_pop <- DT::renderDataTable(
      {
         hardyWeinberg()$pops_out_of_HWE
      },
      options = list(scrollX = TRUE)
   )
   
   output$fstMatrixUI <- renderUI({
      fst <- fstStats()$fst_matrix
      if (is.list(fst) && "message" %in% names(fst)) {
         tags$p(style = "color:gray;", fst$message)
      } else {
         DT::dataTableOutput("fstMatrixTable")
      }
   })
   
   output$fstMatrixTable <- DT::renderDataTable(
      {
         matrix_data <- matrix(unlist(fstStats()$fst_matrix),
                               nrow = sqrt(length(fstStats()$fst_matrix)),
                               byrow = TRUE
         )
         rownames(matrix_data) <- colnames(matrix_data) <- attr(fsnps_gen(), "pop.names")
         as.data.frame(matrix_data)
      },
      options = list(scrollX = TRUE)
   )
   
   output$fstDfTable <- DT::renderDataTable({
      fstStats()$fst_dataframe
   })
   
   output$fst_heatmap_plot <- renderImage(
      {
         req(fstData())
         
         plot_path <- plot_fst(
            fst_df = fstData(),
            out_dir = tempdir()
         )
         list(
            src = plot_path,
            contentType = "image/png",
            alt = "FST Heatmap",
            width = "100%"
         )
      },
      deleteFile = TRUE
   )
   
   output$downloadHeterozygosityPlot <- downloadHandler(
      filename = function() {
         "heterozygosity_plot.png"
      },
      content = function(file) {
         plot_path <- plot_heterozygosity(
            Het_fsnps_df = popStats()$heterozygosity,
            out_dir = tempdir()
         )
         file.copy(plot_path, file)
      }
   )
   
   output$downloadHeterozygosityPlot_UI <- renderUI({
      downloadButton("downloadHeterozygosityPlot", "Download Heterozygosity Plot")
   })
   
   output$downloadFstHeatmap <- downloadHandler(
      filename = function() {
         "fst_heatmap.png"
      },
      content = function(file) {
         plot_path <- plot_fst(
            fst_df = fstData(),
            out_dir = tempdir()
         )
         file.copy(plot_path, file)
      }
   )
   
   output$downloadFstHeatmap_UI <- renderUI({
      downloadButton("downloadFstHeatmap", "Download Fst Plot")
   })
   
   ## download all results
   output$downloadStatsXLSX <- downloadHandler(
      filename = function() {
         timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
         paste0("population-statistics-results_", timestamp, ".xlsx")
      },
      content = function(file) {
         req(statsMatrix(), hwMatrix(), fstMatrix(), privAlleles(), afData())
         path <- export_pop_results(
            allele_freq = afData(),
            priv_alleles = privAlleles(),
            stats_matrix = statsMatrix(),
            hw_matrix = hwMatrix(),
            fst_matrix = fstMatrix(), dir = tempdir()
         )
         
         file.copy(path, file)
      }
   )
   
   output$downloadStatsXLSX_UI <- renderUI({
      req(statsMatrix(), hwMatrix(), fstMatrix(), privAlleles())
      downloadButton("downloadStatsXLSX", "Download Results (excel)")
   })
   
   # ==================== To Arlequin-compatible file =======================#
   exampleForArlecore <- data.frame(
      Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
      Population = c("POP1", "POP2", "POP3", "POP4", "..."),
      rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
      rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
      rs_n = c("...", "...", "...", "...", "...")
   )
   
   output$exampleForArlecore <- DT::renderDataTable(
      {
         req(exampleForArlecore)
         exampleForArlecore
      },
      options = list(
         scrollX = TRUE,
         pageLength = 10
      )
   )
   
   #======== REVISE
   
   arlequinResults <- reactiveVal(NULL)
   
   observe({
      shinyjs::toggleState("runArlecore", !is.null(input$fileForArlecore))
   })
   
   observeEvent(input$runArlecore, {
      disable("runArlecore")
      
      withProgress(message = "Analysis ongoing...", {
         incProgress(0.2, detail = "Loading input file...")
         for_arp <- load_csv_xlsx_files(input$fileForArlecore$datapath)
         for_arp <- clean_input_data(for_arp)
         
         # All null values are "N", set to ""
         for_arp <- for_arp %>%
            mutate(across(everything(), ~ case_when(
               . == "N" ~ "",
               TRUE ~ .x
            )))
         for_arp <- as.data.frame(for_arp)
         
         incProgress(0.4, detail = "Creating input file...")
         # create the arp file
         arp_file <- build_arp_per_population(for_arp,
                                              genotypic_data = as.numeric(input$genotypicData),
                                              gametic_phase = as.numeric(input$gameticPhase),
                                              recessive_data = as.numeric(input$recessiveData),
                                              locus_sep = input$locusSep,
                                              output.prefix = "arp_file",
                                              data_type = "STANDARD"
         )
         
         incProgress(0.6, detail = "Running arlecore...")
         #run arlecore, returns path of res folder
         if (isTRUE(input$calcLD)) {
            results <- run_arlequin(arp_file, ld = TRUE)
         } else {
            results <- run_arlequin(arp_file, ld = FALSE)
         }
         
         incProgress(0.8, detail = "Loading report...")
         # get report file
         file_dir <- dirname(results)
         xml_file <- file.path(results, "arp_file.xml")
         
         if (!file.exists(xml_file)) {
            stop("Resulting 'arp_file.xml' report not found.")
         }
         
         doc <- XML::xmlParse(xml_file)
         arlequinResults(parse_arlequin_report(doc))
         
         print(str(arlequinResults()))
      })
      
      enable("runArlecore")
   }) # end of observe event
   
   output$arlequinTables <- renderUI({
      req(arlequinResults())
      
      lapply(seq_along(arlequinResults()$sections), function(i) {
         section <- arlequinResults()$sections[[i]]
         
         tagList(
            h3(section$title),
            DT::DTOutput(paste0("arlequin_table_", i)),
            br()
         )
      })
   })
   
   observe({
      req(arlequinResults())
      lapply(seq_along(arlequinResults()$sections), function(i) {
         local({
            ii <- i
            output[[paste0("arlequin_table_", ii)]] <- DT::renderDT({
               arlequinResults()$sections[[ii]]$data
            },
            options = list(
               scrollX = TRUE,
               pageLength = 10
            )
            )
         })
      })
   })
   
   
   observe({
      req(arlequinResults())
      
      lapply(seq_along(arlequinResults()$sections), function(i) {
         local({
            ii <- i
            
            output[[paste0("arlequin_plot_", i)]] <- renderPlot({
               section <- arlequinResults()$sections[[ii]]
               df <- section$data
               df_long <- tidyr::pivot_longer(
                  df,
                  cols = -Locus,
                  names_to = "Population",
                  values_to = "Value"
               )
               
               ggplot2::ggplot(
                  df_long,
                  ggplot2::aes(x = Locus, y = Value, color = Population)) +
                  ggplot2::geom_line() +
                  ggplot2::theme_minimal() +
                  ggplot2::labs(
                     title = section$title,
                     x = "Locus",
                     y = NULL
                  )
            })
         })
      })
   })
   
   observe({
      req(arlequinResults())
      lapply(seq_along(arlequinResults()$sections), function(i){
         local ({
            ii <- i
            output[[paste0("download_plot_", ii)]] <- downloadHandler(
               filename = function() {
                  paste0(
                     arlequinResults()$sections[[ii]]$type,
                     "_plot.png"
                  )
               }, content = function(file) {
                  section <- arlequinResults()$sections[[ii]]
                  df <- section$data
                  df_long <- tidyr::pivot_longer(
                     df,
                     cols = -Locus,
                     names_to = "Population",
                     values_to = "Value"
                  )
                  
                  p <- ggplot2::ggplot(
                     df_long,
                     ggplot2::aes(x = Locus, y = Value, color = Population)) +
                     ggplot2::geom_line() +
                     ggplot2::theme_minimal() +
                     ggplot2::labs(
                        title = section$title,
                        x = "Locus",
                        y = NULL
                     )
                  
                  ggplot2::ggsave(
                     filename = file,
                     plot = p,
                     width = 10,
                     height = 6,
                     dpi = 300
                  )
                  
               }
            )
         })
      })
   })
   
   output$arlequinPlots <- renderUI({
      req(arlequinResults())
      
      lapply(seq_along(arlequinResults()$sections), function(i) {
         section <- arlequinResults()$sections[[i]]
         
         tagList(
            h3(section$title),
            plotOutput(paste0("arlequin_plot_", i), height = "500px"),
            
            downloadButton(paste0("download_plot_", i),
                           paste0("Download ", section$title)
            ),
            br()
         )
      })
   })
   
   output$downloadArleResults <- downloadHandler(
      filename = function() {
         paste0(
            "arlequin_results_",
            Sys.Date(),
            ".xlsx"
         )
      }, content = function(file) {
         wb <- openxlsx::createWorkbook()
         sections <- arlequinResults()$sections
         
         for (i in seq_along(sections)){
            section <- sections[[i]]
            sheet_name <- substr(
               section$type, 1, 31
            )
            
            openxlsx::addWorksheet(wb, sheet_name)
            
            openxlsx::writeData(wb, sheet_name, section$data)
         }
         openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
   )
   
   output$downloadArleResults_UI <- renderUI({
      req(arlequinResults())
      downloadButton("downloadArleResults", "Download Arlequin Results (excel)")
   })
   
}