popstats_tab <- function() {
   
   # POPULATION STATISTICS MODULE=====================================================================
   tabItem(
      tabName = "PopStatistics",
      fluidRow(
         box(
            fileInput("popStatsFile", "Upload CSV or XLSX Dataset", accept = c(".zip", ".tar")),
            actionButton("runPopStats", "Analyze", icon = icon("magnifying-glass-chart")),
            selectInput("correctionModel", "Select Correction Model", choices = c("Bonferroni" = "Bonferroni", "FDR" = "FDR")),
            numericInput("alphaValue", "Set Alpha Value", value = 0.05, min = 0.00, max = 1),
            uiOutput("downloadStatsXLSX_UI")
         ),
         tabBox(
            tabPanel(
               "Instructions",
               div(
                  style = "height: 40vh; overflow-y:scroll;",
                  uiOutput("popstatRef")
               )
            ),
            tabPanel(
               "Sample Input Format/s",
               h4("Example: Population File Format"),
               DT::dataTableOutput("examplePop_UI")
            ),
            tabPanel(
               "Download Sample Files",
               h4("Sample File"),
               tags$ul(
                  tags$a("Sample CSV file", href = "sample.csv", download = "sample.csv")
               )
            )
         )
      ),
      fluidRow(
         tabBox(
            width = 12,
            tabPanel(
               "1. Private Alleles",
               h4("Private Alleles Summary"),
               DT::dataTableOutput("privateAlleleTable")
            ),
            tabPanel(
               "2. Mean Allelic Richness",
               h4("Mean Allelic Richness per site"),
               DT::dataTableOutput("meanallelic")
            ),
            tabPanel(
               "3. Heterozygosity",
               h4("Observed vs Expected Heterozygosity"),
               DT::dataTableOutput("heterozygosity_table"),
               br(),
               h4("Heterozygosity Plot"),
               imageOutput("heterozygosity_plot"),
               uiOutput("downloadHeterozygosityPlot_UI")
            ),
            tabPanel(
               "4. Inbreeding Coefficients",
               h4("Inbreeding Coefficient by Population"),
               DT::dataTableOutput("inbreeding_table")
            ),
            tabPanel(
               "5. Allele Frequencies",
               h4("Allele Frequency Table"),
               DT::dataTableOutput("allele_freq_table")
            ),
            tabPanel(
               "6. Hardy-Weinberg Equilibrium",
               h4("HWE P-value Summary"),
               DT::dataTableOutput("hwe_summary_text"),
               h4("Population-wise HWE Chi-Square Table"),
               DT::dataTableOutput("hwe_chisq_table"),
               h4("Loci out of HWE"),
               DT::dataTableOutput("hwe_loci"),
               h4("Populations out of HWE"),
               DT::dataTableOutput("hwe_pop")
            ),
            tabPanel(
               "7. Fst Values",
               h4("Pairwise Fst Matrix"),
               uiOutput("fstMatrixUI"),
               h4("Tidy Pairwise Fst Data"),
               DT::dataTableOutput("fstDfTable"),
               br(),
               h4("Fst Heatmap"),
               uiOutput("downloadFstHeatmap_UI"),
               imageOutput("fst_heatmap_plot", width = "100%")
            )
         )
      )
   )
   
}