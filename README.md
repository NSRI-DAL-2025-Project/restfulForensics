<img src = "www/readme/fulllogo.png" width = "300" height = "200">
# restful forensics
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
   a. [Prerequisites](#A.-Prerequisites)
   b. [Dependency List](#B.-Dependencies)
   c. [Installation Guide](#C.-Installation-Guide)
5. [Usage Guide](#Usage-Guide)  
6. [Example Workflow](#Example-Workflow)  
7. [Limitations](#Limitations)
   a. [Known Limitations](#Known-Limitations)   
   b. [Planned Enhancements](#Planned-Enhancements) 
8. [Citation Guide](#Citation-Guide)  
9. [License](#License)  

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
| 4 | To SNIPPER file | Convert .csv or .xlsx files into a SNIPPER-compatible file for individual classification using ancestry-informative markers | |
| 5 | To STRUCTURE file | Converts .csv or .xlsx files to a STRUCTURE v2.3.4-compatible file | |
| 6 | To Arlequin file | Converts .csv or .xlsx files to an Arlequin-compatible file | |

Module 2: SNP Data Extraction
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |
| 1 | SNP Data Extraction | Extract specific SNP calls from genome-wide data (VCF or PLINK associated files) based on rsID or position | PLINK 2.0 |
| 2 | Concordance Analysis | Check concordance of calls between datasets with overlapping samples | |

Module 3: Filtering
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |
| 1 | Filtering | Perform quality control/filtering of samples and variants using standard options in PLINK 2.0 | PLINK 2.0 |

Module 4: Exploratory Analysis
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |
| 1 | Exploratory Analysis | Perform Principal Components Analysis using multivariate SNP data | Ade4 |

Module 5: Population Summary Statistics
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |
| 1 | R-based Calculations | |
| 2 | Arlecore | | Arlecore |

Module 6: Population Structure Analysis
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |
| 1 | Run STRUCTURE v2.3.4 | | STRUCTURE v2.3.4 |
| 2 | Plot STRUCTURE results | CLUMPP |

Module 7: Forensic Parameters
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |
| 1 | 

Module 8: Forensic DNA Inference
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |
| 1 | 

Module 9: DNA Barcoding
| # | Feature | Description | Related Tools |
| :---: | :--- | :--- | :---: |
| 1 | Multiple Sequence Alignment | | |
| 2 | Phylogenetic Tree Analysis | | |
| 3 | Barcoding | | DECIPHER |


## Installation

### A. Prerequisites
- [R (R version 4.6.1 and above)](https://cran.r-project.org/bin/windows/base/)
- [Rtools (compatible with R >= 4.6.1)](https://cran.r-project.org/bin/windows/Rtools/)
- (optional) Any Integrated Development Environment. Suggestion is to use [RStudio.](https://docs.posit.co/ide/user/#rstudio-ide-oss-downloads)

### B. Dependencies
The list of package dependencies are listed under "R/packages.R" and are installed upon running ```source("install.R")```

### C. Installation Guide

1 Clone the repository  
```git clone https://github.com/NSRI-DAL-2025-Project/restfulForensics.git --branch alpha-test```  

2 Run the application  
```
source("install.R")
shiny::runApp()
```  
  
## Usage Guide  

## Example Workflow

## Limitations

### Known Limitations

### Planned Enhancements

## Citation Guide

## License
