![](www/readme/fulllogo.png)
# restful-forensics
```restful forensics``` is an open-source platform for forensic genetics research
workflow and method validation. This tool is built as a Shiny app in R and
incorporates commonly used packages, functions, and software for genetic data preparation,
pre-processing, and exploratory data analysis.

restful forensics is developed at the Natural Sciences Research Institute,
University of the Philippines Diliman, Quezon City.

## Table of Contents
1. [About restful forensics](#About)  
2. [Software Architecture]()  -- similar with features so 
3. [Features](#Features)  
4. [Installation]()  
   a. [Prerequisites]()
   b. [Dependency List]()
5. [Usage Guide]()  
6. [Example Workflow]()  
7. [Limitations and Works-in-Progress]()
   a. [Known Limitations]()  
   b. [Planned Enhancements]() 
8. [Citation Guide]()  
9. [License]()  


## About
The restful (Reproducible and Efficient Sequence Toolkit) forensics app is a free R-based tool developed to assist forensic genetics
researchers in navigating genomic data analysis for forensic applications. It consolidates
genetic data preprocessing and method validation into one interactive platform using
widely used R packages and the incorporation of external software/executables into R
for a unified workflow.

## Features
restful forensics has 9 distinct modules.  
Module 1: File Conversion  
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |
| 1 | Convert Files | Interconvert single or zipped VCF, BCF, CSV, or PLINK-associated files | PLINK 2.0 |
| 2 | Add Metadata | Merge genotype data with metadata based on shared keywords (sample IDs) | |
| 3 | Widen SNP calls | Convert long-formatted SNP calls to a wide format | |
| 4 | Convert to SNIPPER analysis-ready file | 


Module 2: 
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |

Module 3: 
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |
## Installation

A. Prerequisites
- [R (R version 4.6.1 and above)](https://cran.r-project.org/bin/windows/base/)
- [Rtools (compatible with R >= 4.6.1)](https://cran.r-project.org/bin/windows/Rtools/)
- (optional) Any Integrated Development Environment. Suggestion is to use the [RStudio](https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads)

B. Install the app  
B.1 Clone the repository  
```git clone https://github.com/NSRI-DAL-2025-Project/restfulForensics.git --branch alpha-test```

B.2 Run the application
```
source("install.R")
shiny::runApp()
```

##