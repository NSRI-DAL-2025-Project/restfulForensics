restful_sidebar <- function() {
   
   dashboardSidebar(
      width = 300,
      sidebarMenu(
         id = "tabs",
         tags$head(tags$style("img {max-width: 100%; height: auto; }")),
         tags$img(src = "restful-www/readme/full.png", height = "auto", width = "300px"),
         menuItem("Homepage", tabName = "dashboard", icon = icon("dashboard")),
         menuItem("File Conversion", tabName = "FileConv", icon = icon("arrows-rotate")),
         menuItem("SNP Data Extraction", tabName = "markerExtract", icon = icon("dna")),
         menuItem("Filtering", tabName = "FilterTab", icon = icon("filter")),
         menuItem("Exploratory Analysis", tabName = "PCAtab", icon = icon("magnifying-glass-location")),
         menuItem("Population Summary Statistics", tabName = "PopStatistics", icon = icon("users-gear")),
         menuItem("Population Structure Analysis", tabName = "PopStructure", icon = icon("square-poll-vertical")),
         menuItem("Forensic Parameters", tabName = "ForensicParams", icon = icon("magnifying-glass")), # addition 12 March 2026
         menuItem("Forensic DNA Inference", tabName = "Classification", icon = icon("diagram-project")),
         menuItem("DNA Barcoding",
                  icon = icon("chart-bar"),
                  menuSubItem("Multiple Sequence Alignment", tabName = "MSAtab"),
                  menuSubItem("Phylogenetic Tree Analysis", tabName = "PhylogenAnalysis"),
                  menuSubItem("Barcoding", tabName = "BarcodingTab")
         ),
         menuItem("References", tabName = "AppRef", icon = icon("book-bookmark")),
         menuItem("About", tabName = "About", icon = icon("building-user"))
      ),
      h5("© 2025 DNA Analysis Laboratory, Natural Sciences Research Institute, University of the Philippines Diliman. All rights reserved.")
   )
   
}