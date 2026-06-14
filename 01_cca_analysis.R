# =============================================================================
# Script: 01_cca_analysis.R
# Purpose: Estimate Corrected Covered Area (CCA)  =============================================================================
# Load packages
Pacman::p_load(ccaR, rio, ggplot2, pak)
# -----------------------------------------------------------------------------
# Load data (from Github repo)
# -----------------------------------------------------------------------------
url <- "https://raw.githubusercontent.com/drrubesh/cdi-umbrella-review/main/cdi_umb_data.xlsx"
dat <- rio::import(url, which = "ccaR")
# -----------------------------------------------------------------------------
# Run CCA analysis
# -----------------------------------------------------------------------------
cca_results <- cca(dat)
# -----------------------------------------------------------------------------
# Generate heatmap
# -----------------------------------------------------------------------------
cca_map <- cca_heatmap(dat, fontsize = 2.5, fontsize_diag = 2.5)
# -----------------------------------------------------------------------------
# Save heatmap and export CCA table
# -----------------------------------------------------------------------------
ggsave(filename = "outputs/cca_heatmap.tiff", plot = cca_map,
       width = 180, height = 160, units = "mm", dpi = 300, bg = "white")
cca_tab <- cca_table(dat)
rio::export(cca_tab, file = "outputs/cca_table.csv", overwrite = TRUE)
# -----------------------------------------------------------------------------
# End of CCA analysis