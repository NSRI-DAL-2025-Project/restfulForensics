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
            numericInput("ploidy", "Ploidy Level", value = 2),
            checkboxInput("noadmix", "Use 'No Admixture' Model", value = FALSE),
            checkboxInput("phased", "Phased Genotype", value = FALSE),
            checkboxInput("linkage", "Use Linkage Model", value = FALSE),
            checkboxInput("useAlpha", "Use default alpha value", value = TRUE),
            conditionalPanel(
               condition = "!input.useAlpha",
               textInput("alphaval", "Alpha Value (integers only)", value = 1)
            ),
            actionButton("runStructure", "Run STRUCTURE", icon = icon("play")),
            uiOutput("downloadButtons")
         ),
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
      )
   )
   
}