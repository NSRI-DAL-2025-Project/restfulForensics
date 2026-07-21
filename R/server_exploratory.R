exploratory_analysis_server <- function(input, output, session, rv) {
   
   # ======================= PCA ===========================#
   PCAResults <- reactiveVal(NULL)
   LabelColors <- reactiveVal(NULL)
   output$examplePCA <- renderTable({
      data.frame(
         Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
         Population = c("POP1", "POP2", "POP3", "POP4", "..."),
         rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
         rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
         rs_n = c("...", "...", "...", "...", "...")
      )
   })
   
   observe({
      hasDataFile <- !is.null(input$pcaFile)
      toggleState("runPCA", hasDataFile)
   })
   
   observeEvent(input$runPCA, {
      disable("runPCA")
      req(input$pcaFile)
      
      withProgress(message = "Running PCA...", {
         tryCatch(
            {
               incProgress(0.2, detail = "Loading input file...")
               df <- load_csv_xlsx_files(input$pcaFile$datapath)
               cleaned <- clean_input_data(df)
               fsnps_gen <- convert_to_genind(cleaned, to_str = FALSE)
               
               incProgress(0.4, detail = "Preparing color and label sets...")
               label_file <- NULL
               
               if (!input$useDefaultColors) {
                  req(input$pcaStyleFile)
                  label_file <- input$pcaStyleFile$datapath
               }
               
               labels_colors <- get_labels(
                  fsnps_gen = fsnps_gen,
                  use_default = input$useDefaultColors,
                  label_file = label_file
               )
               LabelColors(labels_colors)
               
               incProgress(0.6, detail = "Computing PCA...")
               pca_results1 <- compute_pca(fsnps_gen)
               PCAResults(pca_results1)
               
               output$barPlot <- renderPlot({
                  req(PCAResults())
                  
                  barplot(PCAResults()$percent,
                          ylab = "Genetic variance explained by eigenvectors (%)", ylim = c(0, 25),
                          names.arg = round(PCAResults()$percent, 1)
                  )
               })
               
               incProgress(0.8, detail = "Rendering PCA plot...")
               output$pcaPlot <- renderPlot({
                  req(PCAResults(), LabelColors())
                  
                  p <- plot_pca(
                     ind_coords = PCAResults()$ind_coords,
                     centroid = PCAResults()$centroid,
                     percent = PCAResults()$percent,
                     labels_colors = LabelColors(),
                     pc_x = input$pcX,
                     pc_y = input$pcY
                  )
                  print(p)
               })
               
               enable("runPCA")
            },
            error = function(e) {
               showNotification(paste("PCA Error:", e$message), type = "error")
               enable("runPCA")
            }
         )
      })
   })
   
   output$downloadbarPlot <- downloadHandler(
      filename = function() {
         paste0("bar_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
         png(file, width = 800, height = 800, res = 300)
         graphics::barplot(
            PCAResults()$percent,
            ylab = "Genetic variance explained by eigenvectors (%)",
            ylim = c(0, 25),
            names.arg = round(PCAResults()$percent, 1)
         )
         dev.off()
      },
      contentType = "image/png"
   )
   
   output$downloadPCAPlot <- downloadHandler(
      filename = function() {
         paste0("pca_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
         plot <- plot_pca(
            ind_coords = PCAResults()$ind_coords,
            centroid = PCAResults()$centroid,
            percent = PCAResults()$percent,
            labels_colors = LabelColors(),
            pc_x = input$pcX,
            pc_y = input$pcY
         )
         
         ggsave(filename = file, plot = plot, width = 8, height = 8, dpi = 600)
      },
      contentType = "image/png"
   )
   
}