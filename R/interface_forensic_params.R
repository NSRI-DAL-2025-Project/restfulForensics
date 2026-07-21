forensic_params_tab <- function() {
   
   # CALCULATION OF FORENSIC PARAMETERS MODULE =======================================================
   tabItem(
      tabName = "ForensicParams",
      tabsetPanel(
         tabPanel(
            "individual identity SNPs",
            fluidRow(
               box(
                  fileInput("iisnpsFile", "Upload Reference File", accept = c(".csv", ".xlsx")),
                  helpText("See 'Sample Input File' for accepted formats. Frequency table and genotype files are accepted."),
                  checkboxInput("floorCeiling", "Use 5/2n rule in calculating genotype frequency?", FALSE),
                  conditionalPanel(
                     condition = "input.floorCeiling",
                     numericInput("totalPop", "Total individuals/samples", value = 100, min = 10, )
                  ),
                  checkboxInput("matchProfile", "Calculate RMP for a profile?", FALSE),
                  conditionalPanel(
                     condition = "input.matchProfile",
                     fileInput("fileProfile", "Upload Profile", accept = c(".csv", ".xlsx", ".txt")),
                     numericInput("thetaValue", "Theta Value", min = 0, value = 0.01, max = 1)
                  ),
                  actionButton("calcIISNPs", "Calculate", icon = icon("calculator")),
                  uiOutput("downloadMetrics_UI"),
                  uiOutput("downloadRMP_UI")
               ),
               tabBox(
                  tabPanel(
                     "Instructions",
                     h4("Calculate forensic parameters specific for individual identity SNPs"),
                     tags$ul(
                        tags$li("Random match probability (PM)"),
                        tags$li("Power of discrimination (PD)"),
                        tags$li("Polymorphism Information Content (PIC)"),
                        tags$li("Power of Exclusion (PE)"),
                        tags$li("Typical Paternity Index (TPI)")
                     ),
                     p(strong("Input file:"), "CSV or XLSX file in genotype format or as an allele frequency table"),
                     p(strong("Expected output file:"), "CSV file"),
                     br(),
                     p(
                        "Guidelines on statistical calculations for casework: ",
                        tags$a("Guidelines (for STR):",
                               href = "https://dfs.dc.gov/sites/default/files/dc/sites/dfs/page_content/attachments/FBS22%20-%20STR%20Statistical%20Calculations%20Guidelines.pdf",
                               target = "_blank"
                        )
                     ),
                     p(
                        "Guidelines and interpretations: ",
                        tags$a("Based on the STRAF book",
                               href = "https://agouy.github.io/straf_book/forensic-parameters.html",
                               target = "_blank"
                        )
                     ),
                  ),
                  tabPanel(
                     "Sample Input File",
                     h4("Acceptable file inputs: genotype files or an allele frequency table:"),
                     p("Genotype file"),
                     DT::dataTableOutput("referenceData_UI"),
                     p("Allele frequency table"),
                     DT::dataTableOutput("afSample_UI"),
                     h4("Sample profile to match"),
                     DT::dataTableOutput("profileSample_UI")
                  ),
                  tabPanel(
                     "Download Sample Files",
                     h4("Download Sample File"),
                     tags$ul(
                        tags$a("Sample CSV file", href = "sample.csv", download = "sample.csv")
                     ),
                     tags$ul(
                        tags$a("Sample Allele Frequency Table", href = "pop_stat.xlsx", download = "pop_stat.xlsx")
                     )
                  )
               )
            ),
            fluidRow(
               tabBox(
                  width = 12,
                  tabPanel(
                     "Overall Forensic Params",
                     selectInput("selected_pop", "Select Population", choices = NULL),
                     div(
                        style = "overflow-x: auto;",
                        DT::dataTableOutput("popTable")
                     )
                  ),
                  tabPanel(
                     "Genotype Frequencies",
                     selectInput("selected_pop_gt", "Select Population", choices = NULL),
                     div(
                        style = "overflow-x: auto;",
                        DT::dataTableOutput("genotypeFreqs_UI")
                     )
                  )
               )
            )
         )
      )
   )
   
}