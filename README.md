![](www/readme/fulllogo.png)
# restful-forensics
```restful forensics``` is an open-source platform for forensic genetics research
workflow and method validation. This tool is built as a Shiny app in R and
incorporates commonly used packages, functions, and software for genetic data preparation,
pre-processing, and exploratory data analysis.

restful forensics is developed at the Natural Sciences Research Institute,
University of the Philippines Diliman, Quezon City.

## Installation

A. Prerequisites
- [R (R version 4.6.1 and above)](https://cran.r-project.org/bin/windows/base/)
- [Rtools (compatible with R >= 4.6.1)](https://cran.r-project.org/bin/windows/Rtools/)
- (optional) Any Integrated Development Environment. Suggestion is to use the [RStudio](https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads)

B. Install the app
Clone the repository first
```git clone https://github.com/NSRI-DAL-2025-Project/restfulForensics.git --branch alpha-test```

Then run the application:
```
source("install.R")
shiny::runApp()
```
