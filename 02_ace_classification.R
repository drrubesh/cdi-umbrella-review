# =============================================================================
# Script: 02_ace_classification.R
# Purpose: Apply ACE framework classification
# =============================================================================

library(dplyr)
library(stringr)
library(rio)

# -----------------------------------------------------------------------------
# Import ACE dataset directly from GitHub
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
url <- "https://raw.githubusercontent.com/drrubesh/cdi-umbrella-review/main/cdi_umb_data.xlsx"
ace <- rio::import(url, which = "ACE input")

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

downgrade_one <- function(x) {
  dplyr::case_when(
    x == "Strong" ~ "Moderate",
    x == "Moderate" ~ "Limited",
    x == "Limited" ~ "Inconclusive",
    TRUE ~ x
  )
}

cap_limited <- function(x) {
  dplyr::case_when(
    x %in% c("Strong", "Moderate") ~ "Limited",
    TRUE ~ x
  )
}

cap_moderate <- function(x) {
  dplyr::case_when(
    x == "Strong" ~ "Moderate",
    TRUE ~ x
  )
}

# -----------------------------------------------------------------------------
# ACE classification
# Required input columns:
# effect_measure, pooled_effect, ci_low, ci_high, i2,
# direction_alignment, d1_temporality, d4_dose_response,
# d5_methodology, d6_publication_bias
# -----------------------------------------------------------------------------

ace_classified <- ace %>%
  mutate(
    across(where(is.character), str_trim),
    
    effect_measure = toupper(effect_measure),
    
    # -------------------------------------------------------------------------
    # D2: Strength of association
    # GRADE-aligned large effect:
    # RR/OR/HR >= 2.0 or <= 0.50, CI excludes null
    # RD: absolute RD >= 5%, CI excludes 0
    # -------------------------------------------------------------------------
    d2_strength = case_when(
      effect_measure %in% c("RR", "OR", "HR") &
        ci_low <= 1 & ci_high >= 1 ~ "NS",
      
      effect_measure %in% c("RR", "OR", "HR") &
        !(ci_low <= 1 & ci_high >= 1) &
        (pooled_effect >= 2 | pooled_effect <= 0.5) ~ "S",
      
      effect_measure %in% c("RR", "OR", "HR") &
        !(ci_low <= 1 & ci_high >= 1) ~ "PS",
      
      effect_measure == "RD" &
        ci_low <= 0 & ci_high >= 0 ~ "NS",
      
      effect_measure == "RD" &
        !(ci_low <= 0 & ci_high >= 0) &
        abs(pooled_effect) >= 5 ~ "S",
      
      effect_measure == "RD" &
        !(ci_low <= 0 & ci_high >= 0) ~ "PS",
      
      TRUE ~ NA_character_
    ),
    
    # -------------------------------------------------------------------------
    # D3: Consistency
    # Direction alignment is primary.
    # If direction alignment is not assessable but heterogeneity is available,
    # classify conservatively as PS.
    # -------------------------------------------------------------------------
    d3_consistency = case_when(
      str_detect(direction_alignment, "≥80|>=80") &
        (is.na(i2) | i2 <= 50) ~ "S",
      
      str_detect(direction_alignment, "≥80|>=80") &
        !is.na(i2) & i2 > 50 ~ "PS",
      
      str_detect(direction_alignment, "60-79") ~ "PS",
      
      str_detect(direction_alignment, "<60") ~ "NS",
      
      str_detect(str_to_lower(direction_alignment), "not assessable") &
        !is.na(i2) ~ "PS",
      
      str_detect(str_to_lower(direction_alignment), "not assessable") &
        is.na(i2) ~ "NA",
      
      TRUE ~ NA_character_
    ),
    
    # -------------------------------------------------------------------------
    # Stage 1: Preliminary ACE assessment
    # Apply rules sequentially
    # -------------------------------------------------------------------------
    preliminary_ace = case_when(
      d1_temporality == "NS" ~ "Inconclusive",
      d2_strength == "NS" ~ "Inconclusive",
      d1_temporality == "PS" ~ "Limited",
      d3_consistency == "NS" ~ "Limited",
      
      d1_temporality == "S" &
        d2_strength == "S" &
        d3_consistency == "S" &
        d4_dose_response != "NS" ~ "Strong",
      
      d1_temporality == "S" &
        d2_strength == "S" &
        d3_consistency == "S" &
        d4_dose_response == "NS" ~ "Moderate",
      
      TRUE ~ "Moderate"
    ),
    
    # -------------------------------------------------------------------------
    # Stage 2a: Apply D5 — methodological quality
    # Robust = no change
    # Some concerns = downgrade one level
    # Major limitations = cap at Limited
    # -------------------------------------------------------------------------
    after_d5 = case_when(
      d5_methodology == "Robust" ~ preliminary_ace,
      d5_methodology == "Some concerns" ~ downgrade_one(preliminary_ace),
      d5_methodology == "Major limitations" ~ cap_limited(preliminary_ace),
      TRUE ~ preliminary_ace
    ),
    
    # -------------------------------------------------------------------------
    # Stage 2b: Apply D6 — publication bias
    # Bias detected = cap at Moderate
    # No bias / Not assessable = no change
    # -------------------------------------------------------------------------
    final_ace = case_when(
      d6_publication_bias == "Bias detected" ~ cap_moderate(after_d5),
      TRUE ~ after_d5
    )
  )

# -----------------------------------------------------------------------------
# Export classified dataset
# -----------------------------------------------------------------------------

rio::export(
  ace_classified,
  file = "outputs/ace_classified_output.xlsx",
  overwrite = TRUE
)

rio::export(
  ace_classified,
  file = "outputs/ace_classified_output.csv",
  overwrite = TRUE
)

# -----------------------------------------------------------------------------
# Summary table
# -----------------------------------------------------------------------------

ace_summary <- ace_classified %>%
  count(final_ace, name = "n") %>%
  arrange(match(final_ace, c("Strong", "Moderate", "Limited", "Inconclusive")))

rio::export(
  ace_summary,
  file = "outputs/ace_summary.csv",
  overwrite = TRUE
)

ace_summary