# CDI Umbrella Review

This repository contains the data and R scripts used for the supplementary analyses in the manuscript:

**Factors associated with mortality in Clostridioides difficile infection: an umbrella review of systematic reviews**

## Folders

- `data/`: cleaned datasets used for supplementary analyses
- `src/`: R scripts used to reproduce CCA, ACE, Ioannidis classification, and figures

## Analyses

The repository includes code and data for:

1. Corrected covered area (CCA) analysis
2. ACE credibility framework
3. ACE annotated forest plot
4. Ioannidis classification of evidence credibility

## How to run

Run the scripts in order:

```r
source("src/01_cca_analysis.R")
source("src/02_ace_classification.R")
source("src/03_ace_forest_plot.R")
source("src/04_ioannidis_classification.R")
