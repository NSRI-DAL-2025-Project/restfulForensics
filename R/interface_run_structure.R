structure_runs <- function() {
   
   # POPULATION STRUCTURE MODULE ===========================================================================
   tabItem(
      tabName = "StructureRun",
      fluidRow(
         box(
            fileInput("structureFile", "Upload Input File (CSV/XLSX)", accept = c(".csv", ".xlsx")),
            helpText("Input file should be similar to the output of the 'Convert to CSV' tab under 'File Conversion'"),
            numericInput("kMin", "Min K", value = 2, min = 1),
            numericInput("kMax", "Max K", value = 5, min = 1),
            numericInput("numKRep", "Replicates per K", value = 10, min = 1),
            numericInput("burnin", "Burn-in Period", value = 100000),
            numericInput("numreps", "MCMC Reps After Burn-in", value = 100000),
            checkboxInput("noadmix", "Use 'No Admixture' Model", value = TRUE),
            checkboxInput("freqScore", "Use 'Correlated Frequencies' Model?", value = FALSE),
            checkboxInput("advancedStructure", "See Advanced Parameters", value = FALSE),
            conditionalPanel(
              condition = "input.advancedStructure == 'true' && input.noadmix == 'false'",
              checkboxInput("inferAlpha", "Infer the value of model parameters", value = FALSE),
              numericInput("alphaValStructure", "Alpha Value", value = 0.05, max = 1)
            ),
            actionButton("runStructure", "Run STRUCTURE", icon = icon("play")),
            uiOutput("downloadButtons")
         ),
         tabBox(
            tabPanel(
               title = "Instructions",
               p(
                  "This runs the basic Windows implementation of STRUCTURE v2.3.4 without a front-end and allows immediate
                                   visualization of results using the ",
                  tags$a("strataG",
                         href = "https://github.com/EricArcher/strataG/tree/master",
                         target = "_blank"
                  ), "R package. This generates STRUCTURE input files and qmatrices files compatible for
                                   other visualization programs such as pong (Behr et al., 2016)."
               ),
               p(strong("Input file:"), "CSV or XLSX file"),
               p(strong("Expected output file:"), "Zipped qmatrices and individual files"),
               p(
                  "See ",
                  tags$a("STRUCTURE v2.3.4 Documentation",
                         href = "https://web.stanford.edu/group/pritchardlab/structure_software/release_versions/v2.3.4/structure_doc.pdf",
                         target = "_blank"
                  )
               )
            ),
            tabPanel(
               title = "Sample Input File",
               DT::dataTableOutput("examplePop_STR2UI")
            ),
            tabPanel(
               title = "Download Sample File",
               h4("Download Sample File"),
               tags$ul(
                  tags$a("Sample CSV file", href = "sample.csv", download = "sample.csv")
               )
            )
         )
      )#,
      #fluidRow(
      #   tabBox(
      #      width = 12,
      #      tabPanel(
      #         "Status",
      #         verbatimTextOutput("structure_log")
      #      )
      #   )
      #)
   )
   
}