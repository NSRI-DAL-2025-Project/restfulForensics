plot_structure_runs <- function() {
   
   tabItem(
      tabName = "PlotStructure",
      fluidRow(
         box(
            fileInput("zippedMatrices", "Upload STRUCTURE results (zipped matrices)", accept = c("zip", "tar")),
            numericInput("kChoice", "Target K Value", value = 2, min = 1),
            selectInput("greedyAlgo", "Choose Algorithm",
                        choices = c("Full Search" = "fullSearch", "Greedy" = "greedy", "Large K" = "largeK"),
                        selected = "greedy"),
            radioButtons("simStat", "Pairwise matrix similarity statistics",
                         choices = c("g" = "g", "g.prime" = "g.prime")
            ),
            conditionalPanel(
               condition = "input.greedyAlgo %in% c('greedy', 'largeK')",
               radioButtons("greedyOption", "Input order of the runs to be tested",
                            choices = c("All" = "all", "ran order" = "ranOrder")
               ),
            ),
            numericInput("orderRun", "Permute clusters according to cluster order?", value = 0),
            actionButton("plotStructureResults", "Run CLUMPP", icon = icon("play"))
            
         ), # end of box
         tabBox(
            tabPanel(
               "Instructions",
               p(
                  "This runs the basic Windows implementation of STRUCTURE v2.3.4 without a front-end and allows immediate
                                   visualization of results using revised functions from the ",
                  tags$a("starmie",
                         href = "https://github.com/sa-lee/starmie",
                         target = "_blank"
                  ), "R package. This generates STRUCTURE input files and qmatrices files compatible for
                                   other visualization programs such as pong (Behr et al., 2016)."
               ),
               p("Some functions were revised and adapted from the strataG and dartR packages such as 'gl.run.structure', '.structureParseQmat', 'structureRead', and 'utils.structure.evanno'"),
               # h4("Generate STRUCTURE input files and pong compatible files. Visualize the possible results"),
               p(strong("Input file:"), "CSV or XLSX file"),
               p(strong("Expected output file:"), "Zipped qmatrices, individual files, and PNG plots"),
               p(
                  "See ",
                  tags$a("STRUCTURE v2.3.4 Documentation",
                         href = "https://web.stanford.edu/group/pritchardlab/structure_software/release_versions/v2.3.4/structure_doc.pdf",
                         target = "_blank"
                  )
               )
            ),
            tabPanel(
               "Sample Input File",
               DT::dataTableOutput("examplePop_STR2UI")
            ),
            tabPanel(
               "Download Sample File",
               h4("Download Sample File"),
               tags$ul(
                  tags$a("Sample CSV file", href = "sample.csv", download = "sample.csv")
               )
            )
         )
      ), # end of fluid row
      fluidRow(
         tabBox(
            width = 12,
            tabPanel(
               "Prediction Table",
               verbatimTextOutput("predictionTableResult")
            ),
            tabPanel(
               "Statistics by Population",
               verbatimTextOutput("statbyClassResult")
            ),
            tabPanel(
               "Overall Statistics",
               verbatimTextOutput("overallStatResult")
            )
         )
      )
   ) # end of tab item
   
}