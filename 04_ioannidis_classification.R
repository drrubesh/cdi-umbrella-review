# =============================================================================
# Script: 04_ioannidis_classification.R
# Purpose: Apply Ioannidis credibility classification
# =============================================================================

library(dplyr)
library(stringr)
library(rio)

# -----------------------------------------------------------------------------
# Import dataset
# -----------------------------------------------------------------------------
# Expected file: data/ioannidis_input.xlsx
# You can replace this with a GitHub raw URL if needed.
url <- "https://raw.githubusercontent.com/drrubesh/cdi-umbrella-review/main/cdi_umb_data.xlsx"
dat= rio::import(url, which = "Ionnidis input")
# -----------------------------------------------------------------------------
# Helper: nominal statistical significance
# -----------------------------------------------------------------------------

is_nominally_significant <- function(effect_measure, ci_low, ci_high) {
  effect_measure <- toupper(trimws(effect_measure))
  
  case_when(
    effect_measure %in% c("RR", "OR", "HR") &
      !(ci_low <= 1 & ci_high >= 1) ~ "Yes",
    
    effect_measure == "RD" &
      !(ci_low <= 0 & ci_high >= 0) ~ "Yes",
    
    effect_measure %in% c("RR", "OR", "HR") &
      ci_low <= 1 & ci_high >= 1 ~ "No",
    
    effect_measure == "RD" &
      ci_low <= 0 & ci_high >= 0 ~ "No",
    
    TRUE ~ NA_character_
  )
}

# -----------------------------------------------------------------------------
# Ioannidis classification
# -----------------------------------------------------------------------------
# Standard umbrella-review credibility logic:
#
# Class I / Convincing evidence:
# - >1000 cases
# - p < 1e-6
# - largest study significant
# - PI excludes null
# - no large heterogeneity
# - no small-study effects
# - no excess significance bias
#
# Class II / Highly suggestive:
# - >1000 cases
# - p < 1e-6
# - largest study significant
#
# Class III / Suggestive:
# - >1000 cases
# - p < 1e-3
#
# Class IV / Weak:
# - nominally significant pooled association
#
# Not significant:
# - pooled CI includes null
#
# In this review, no association had >1000 deaths.
# Therefore, statistically significant associations can only reach Class IV / Weak.
# -----------------------------------------------------------------------------

ioannidis_classified <- dat %>%
  mutate(
    across(where(is.character), str_trim),
    effect_measure = toupper(effect_measure),
    
    # -------------------------------------------------------------------------
    # Two-sided nominal significance
    # -------------------------------------------------------------------------
    nominal_significance = case_when(
      
      effect_measure %in% c("RR", "OR", "HR") &
        !(ci_low <= 1 & ci_high >= 1) ~ "Yes",
      
      effect_measure == "RD" &
        !(ci_low <= 0 & ci_high >= 0) ~ "Yes",
      
      effect_measure %in% c("RR", "OR", "HR") &
        (ci_low <= 1 & ci_high >= 1) ~ "No",
      
      effect_measure == "RD" &
        (ci_low <= 0 & ci_high >= 0) ~ "No",
      
      TRUE ~ NA_character_
    ),
    
    # -------------------------------------------------------------------------
    # Standardise inputs
    # -------------------------------------------------------------------------
    deaths_gt_1000 = case_when(
      is.na(deaths) ~ "NR",
      deaths %in% c("Yes", "yes", "Y") ~ "Yes",
      deaths %in% c("No", "no", "N") ~ "No",
      TRUE ~ "NR"
    ),
    
    largest_study_significance = ifelse(
      is.na(largest_study_significance), "NR", largest_study_significance
    ),
    
    prediction_interval_excluded_null = ifelse(
      is.na(prediction_interval_excluded_null), "NR", prediction_interval_excluded_null
    ),
    
    small_study_effects = ifelse(
      is.na(small_study_effects), "NR", small_study_effects
    ),
    
    excess_significance = ifelse(
      is.na(excess_significance), "NR", excess_significance
    ),
    
    # -------------------------------------------------------------------------
    # Ioannidis classification (two-sided logic)
    # -------------------------------------------------------------------------
    ioannidis_class = case_when(
      
      # Not significant (two-sided)
      nominal_significance == "No" ~ "Not significant",
      
      # Class I — Convincing
      nominal_significance == "Yes" &
        deaths_gt_1000 == "Yes" &
        p_value < 1e-6 &
        largest_study_significance == "Yes" &
        prediction_interval_excluded_null == "Yes" &
        small_study_effects == "No" &
        excess_significance == "No" ~ "Class I/Convincing",
      
      # Class II — Highly suggestive
      nominal_significance == "Yes" &
        deaths_gt_1000 == "Yes" &
        p_value < 1e-6 &
        largest_study_significance == "Yes" ~ "Class II/Highly suggestive",
      
      # Class III — Suggestive
      nominal_significance == "Yes" &
        deaths_gt_1000 == "Yes" &
        p_value < 1e-3 ~ "Class III/Suggestive",
      
      # Class IV — Weak (two-sided, includes protective effects)
      nominal_significance == "Yes" ~ "Class IV/Weak",
      
      TRUE ~ "Not classifiable"
    ),
    
    # -------------------------------------------------------------------------
    # Transparent reasoning column
    # -------------------------------------------------------------------------
    ioannidis_reason = case_when(
      
      ioannidis_class == "Not significant" ~
        "Two-sided CI includes the null value",
      
      ioannidis_class == "Class IV/Weak" &
        deaths_gt_1000 %in% c("No", "NR") ~
        "Nominally significant (two-sided) but <=1000 deaths or not reported",
      
      ioannidis_class == "Class IV/Weak" ~
        "Nominally significant but does not meet higher credibility thresholds",
      
      ioannidis_class == "Class I/Convincing" ~
        "Meets all Ioannidis criteria including PI exclusion and no bias signals",
      
      ioannidis_class == "Class II/Highly suggestive" ~
        "Strong statistical signal with >1000 deaths and largest study significant",
      
      ioannidis_class == "Class III/Suggestive" ~
        "Moderate statistical signal with >1000 deaths",
      
      TRUE ~ "Insufficient information"
    )
  )
# -----------------------------------------------------------------------------
# Export results
# -----------------------------------------------------------------------------

rio::export(
  ioannidis_classified,
  file = "outputs/ioannidis_classified_output.xlsx",
  overwrite = TRUE
)

rio::export(
  ioannidis_classified,
  file = "outputs/ioannidis_classified_output.csv",
  overwrite = TRUE
)

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

ioannidis_summary <- ioannidis_classified %>%
  count(ioannidis_class, name = "n")

rio::export(
  ioannidis_summary,
  file = "outputs/ioannidis_summary.csv",
  overwrite = TRUE
)

ioannidis_summary