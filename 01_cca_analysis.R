# =============================================================================
# Script: 01_cca_analysis.R
# Purpose: Estimate Corrected Covered Area (CCA)  =============================================================================
# Load packages
pacman::p_load(ccaR, rio, ggplot2, pak)
# -----------------------------------------------------------------------------
# Load data (from Github repo)
# -----------------------------------------------------------------------------
url <- "https://raw.githubusercontent.com/drrubesh/cdi-umbrella-review/main/cdi_umb_data.xlsx"

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
# -----------------------------------------------------------------------------
# Sensitivity analysis: exclude Eeuwijk 2024
# -----------------------------------------------------------------------------

dat_sensitivity <- dat |>
  dplyr::select(-`Eeuwijk  2024`)

# Run CCA
cca_results_sensitivity <- cca(dat_sensitivity)

cca_results_sensitivity

# Generate sensitivity heatmap if required
cca_map_sensitivity <- cca_heatmap(
  dat_sensitivity,
  fontsize = 2.5,
  fontsize_diag = 2.5
)

cca_map_sensitivity
# End of CCA analysis
