# CDI Umbrella Review

This repository contains the data and R scripts used for supplementary analyses in the manuscript:

“Factors associated with mortality in Clostridioides difficile infection: an umbrella review of systematic reviews”

## Contents

This repository contains:

- `cdi_umb_data.xlsx`  
  Excel workbook containing the data used for the analyses.

- `01_cca_analysr.r`  
  R script for corrected covered area (CCA) analysis.

- `02_ace_classification.r`  
  R script for ACE credibility classification.

- `03_ace_forest_plot.R`  
  R script for generating the ACE annotated forest plot.

- `04_ioannidis_classification.r`  
  R script for Ioannidis-style credibility classification.

## How to run the scripts

If you already use GitHub, you can clone or download this repository.

If GitHub is not already set up on your computer, simply download the repository as a ZIP file:

1. Click the green **Code** button on this GitHub page.
2. Click **Download ZIP**.
3. Extract/unzip the downloaded folder on your computer.
4. Open RStudio.
5. Open the extracted folder in RStudio.

Make sure all files are in the same folder:

- `cdi_umb_data.xlsx`
- `01_cca_analysr.r`
- `02_ace_classification.r`
- `03_ace_forest_plot.R`
- `04_ioannidis_classification.r`

Install the required R packages:

```r
install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "knitr"))
```

Run the scripts in this order:

```r
source("01_cca_analysr.r")
source("02_ace_classification.r")
source("03_ace_forest_plot.R")
source("04_ioannidis_classification.r")
```

The scripts will read the Excel workbook and reproduce the supplementary analyses.

## Data availability

The extracted data and R scripts are available in this repository. Full-text articles are not included because of copyright restrictions.

