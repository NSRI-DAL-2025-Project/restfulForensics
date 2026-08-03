run_structure_analysis <- function(input, output, session, rv) {
   
   examplePop_STR2 <- data.frame(
      Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
      Population = c("POP1", "POP2", "POP3", "POP4", "..."),
      rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
      rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
      rs_n = c("...", "...", "...", "...", "...")
   )
   
   output$examplePop_STR2UI <- DT::renderDataTable(
      {
         req(examplePop_STR2)
         examplePop_STR2
      },
      options = list(
         scrollX = TRUE,
         pageLength = 5
      )
   )
   
   observe({
      file_ready <- !is.null(input$structureFile)
      shinyjs::toggleState("runStructure", condition = file_ready)
   })
   
   analysis_done <- reactiveVal(FALSE)
   qmatrices_result <- reactiveVal(NULL)
   output_dir <- tempdir()
   out_path <- file.path(output_dir, "structure_input.str")
   
   observeEvent(input$runStructure, {
      disable("runStructure")
      req(input$structureFile)
      Sys.sleep(1.5)
      
      withProgress(message = "Running STRUCTURE analysis...", {
         incProgress(0.2, detail = "Loading input file...")
         df <- load_csv_xlsx_files(input$structureFile$datapath)
         df <- clean_input_data(df)
         fsnps_gen <- convert_to_genind(df, to_str = TRUE, popinfo = TRUE)
         
         incProgress(0.4, detail = "Converting to STRUCTURE file...")
         
         structure_df <- to_structure(fsnps_gen$fsnps_gen, include_pop = TRUE)
         structure_df[] <- lapply(structure_df, function(col) as.numeric(as.character(col)))
         
         write.table(structure_df,
                     file = out_path, quote = FALSE, sep = " ",
                     row.names = FALSE, col.names = FALSE
         )
         
         incProgress(0.6, detail = "Running STRUCTURE analysis...")
         
         if (isTRUE(input$useAlpha)) {
            alphaValue <- 1
         } else {
            if (grepl("^-?[0-9]*\\.?[0-9]+$", input$alphaval)) {
               alphaValue <- as.numeric(input$alphaval)
            } else {
               stop("Alpha value should strictly be integers/floats.")
            }
         }
         
         result <- running_structure(out_path,
                                     k.range = input$kMin:input$kMax,
                                     num.k.rep = input$numKRep,
                                     burnin = input$burnin,
                                     numreps = input$numreps,
                                     noadmix = input$noadmix,
                                     phased = input$phased,
                                     alpha_value = alphaValue,
                                     ploidy = input$ploidy,
                                     linkage = input$linkage,
                                     structure_path = "structure/structure.exe",
                                     output_dir = output_dir
         )
         
         incProgress(0.8, detail = "Extracting q matrices...")
         qmatrices_result(q_matrices(output_dir))
         
         populations_df <- fsnps_gen$pop_labels
         str_files <- list.files(output_dir, pattern = "_f$", full.names = TRUE)
         
         enable("runStructure")
      })
      analysis_done(TRUE)
   })
   
   output$downloadLogs <- downloadHandler(
      filename = function() {
         paste0("structure_logs_", Sys.Date(), ".zip")
      },
      content = function(file) {
         log_files <- list.files(output_dir, pattern = "_log.*$", full.names = TRUE)
         if (length(log_files) == 0) {
            return(NULL)
         }
         
         log_files_renamed <- sapply(log_files, function(f) {
            ext <- tools::file_ext(f)
            if (ext == "") {
               new_f <- paste0(f, ".txt")
               file.rename(f, new_f)
               return(new_f)
            }
            return(f)
         })
         zip::zipr(zipfile = file, files = log_files_renamed, recurse = FALSE)
      },
      contentType = "application/zip"
   )
   
   output$downloadFOutputs <- downloadHandler(
      filename = function() {
         paste0("structure_outputs_", Sys.Date(), ".zip")
      },
      content = function(file) {
         f_files <- list.files(output_dir, full.names = TRUE)
         f_files <- f_files[grepl("_f", basename(f_files))] # specify actual file name, check directory
         if (length(f_files) == 0) {
            return(NULL)
         }
         zip::zipr(zipfile = file, files = f_files)
      },
      contentType = "application/zip"
   )
   
   output$downloadQMatrixTxtZip <- downloadHandler(
      filename = function() {
         paste0("q_matrices_", Sys.Date(), ".zip")
      },
      content = function(file) {
         req(qmatrices_result())
         q_files <- tempfile()
         dir.create(q_files)
         lapply(names(qmatrices_result()), function(name) {
            mat <- qmatrices_result()[[name]]
            if (!is.null(mat)) {
               write.table(mat, file.path(q_files, paste0(name, ".txt")),
                           row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t"
               )
            }
         })
         zip::zipr(zipfile = file, files = list.files(q_files, full.names = TRUE))
      },
      contentType = "application/zip"
   )
   
   output$downloadButtons <- renderUI({
      req(analysis_done())
      tagList(
         downloadButton("downloadLogs", "Download Log Files (.zip)"),
         downloadButton("downloadFOutputs", "Download STRUCTURE _f Files (.zip)"),
         downloadButton("downloadQMatrixTxtZip", "Download Q Matrices (.zip)")
      )
   })
   
}