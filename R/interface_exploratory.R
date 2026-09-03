exploratory_tab <- function() {
   
   # PRINCIPAL COMPONENT ANALYSIS MODULE =============================================================
   tabItem(
      tabName = "PCAtab",
      fluidRow(
         box(
            fileInput("pcaFile", "Upload SNP Data (in CSV or XLSX) for PCA", accept = c(".csv", ".txt", ".xlsx")),
            checkboxInput("useDefaultColors", "Use Default Colors and Labels", TRUE),
            conditionalPanel(
               condition = "!input.useDefaultColors",
               fileInput("pcaStyleFile", "Customize population colors and shapes.", accept = c(".csv", ".xlsx", ".txt")),
               helpText("Columns should contain: [1] Population name, [2] Color (name or hex code), [3] Shapes"),
               p("The order of the colors would match the order of PCA labels")
            ),
            br(),
            numericInput("pcX", "PC Axis X", value = 1, min = 1),
            numericInput("pcY", "PC Axis Y", value = 2, min = 1),
            uiOutput("selectedPopulation"),
            
            actionButton("runPCA", "Run PCA Analysis", icon = icon("play")),
            actionButton("recalcPCA", "Recalculate PCA for Selected Populations", icon = icon("filter"))
         ),
         tabBox(
            tabPanel(
               title = "Instructions",
               h4("Run principal component analysis using the ade4 (Dray and Dufour, 2007) package in R"),
               p(strong("Input file:"), "CSV or XLSX file and color labels (optional)"),
               p(strong("Optional additional input file/s:")),
               tags$ul(
                  tags$li("PCA labels (.txt with one population per line)"),
                  tags$li("Color palette (.txt with one hex code per line)"),
                  tags$li(
                     "Desired point shapes (.txt with one number/name indicating the",
                     tags$a("shapes",
                            href = "https://ggplot2.tidyverse.org/reference/scale_shape.html",
                            target = "_blank"
                     ), " per line)"
                  )
               ),
               p(strong("Expected output file:"), "PNG plots")
            ),
            tabPanel(
               title = "Sample Input Format/s",
               h4("Example Input Format"),
               tableOutput("examplePCA")
            ),
            tabPanel(
               title = "Download sample files",
               tags$ul(
                  tags$a("Sample file", href = "sample.csv", download = "sample.csv")
               )
            )
         )
      ),
      fluidRow(
         tabBox(
            title = "PCA Results",
            width = 12,
            tabPanel(
               title = "Plots",
               plotly::plotlyOutput("pcaPlot")#,
               #downloadButton("downloadPCAPlot", "Download PCA Plot")
            ),
            tabPanel(
               title = "Bar Plot",
               plotOutput("barPlot"),
               uiOutput("downloadbarPlot_UI")
            )
         )
      )
   )
   
}