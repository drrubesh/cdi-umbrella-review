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

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------
# This script classifies one pooled meta-analytic estimate per
# exposure-outcome association. It does not require individual-study estimates.
# D1 and D4-D6 are human-entered judgements. D2 is derived from the pooled
# estimate and 95% CI; D3 may be derived from direction alignment and I-squared.

ace_downgrade_one <- function(x) {
  dplyr::case_when(
    x == "Strong" ~ "Moderate",
    x == "Moderate" ~ "Limited",
    x == "Limited" ~ "Inconclusive",
    TRUE ~ x
  )
}

ace_d5_major_limitations <- function(x) {
  dplyr::case_when(
    x %in% c("Strong", "Moderate") ~ "Limited",
    TRUE ~ x
  )
}

ace_d6_bias_cap <- function(x) {
  dplyr::case_when(
    x == "Strong" ~ "Moderate",
    TRUE ~ x
  )
}

ace_normalise_text <- function(x) {
  stringr::str_squish(as.character(x))
}

classify_ace_v1 <- function(ace) {
  required <- c(
    "effect_measure", "pooled_effect", "ci_low", "ci_high", "i2",
    "direction_alignment", "d1_temporality", "d4_dose_response",
    "d5_methodology", "d6_publication_bias"
  )
  missing_columns <- setdiff(required, names(ace))
  if (length(missing_columns)) {
    stop("Missing required column(s): ", paste(missing_columns, collapse = ", "))
  }
  
  # RD-specific fields are optional at import. Absence means that no defensible
  # independently prespecified threshold has been supplied; a non-null RD is PS.
  if (!"rd_unit" %in% names(ace)) ace$rd_unit <- NA_character_
  if (!"important_effect_threshold_available" %in% names(ace)) {
    ace$important_effect_threshold_available <- FALSE
  }
  if (!"important_effect_threshold_percentage_points" %in% names(ace)) {
    ace$important_effect_threshold_percentage_points <- NA_real_
  }
  if (!"threshold_source_or_justification" %in% names(ace)) {
    ace$threshold_source_or_justification <- NA_character_
  }
  
  result <- ace |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.character), ace_normalise_text),
      effect_measure = toupper(effect_measure),
      rd_unit = toupper(rd_unit),
      d1_temporality = toupper(d1_temporality),
      d4_dose_response = toupper(d4_dose_response),
      d5_methodology = toupper(d5_methodology),
      d6_publication_bias = toupper(d6_publication_bias),
      .rd_threshold_available = dplyr::case_when(
        is.logical(important_effect_threshold_available) ~ important_effect_threshold_available,
        toupper(as.character(important_effect_threshold_available)) %in% c("TRUE", "YES", "1") ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  unsupported <- unique(result$effect_measure[!is.na(result$effect_measure) &
                                                !result$effect_measure %in% c("RR", "OR", "HR", "RD")])
  if (length(unsupported)) {
    stop("Unsupported effect measure(s): ", paste(unsupported, collapse = ", "),
         ". ACE v1.0 supports RR, OR, HR and RD; SMD is outside scope.")
  }
  if (any(result$effect_measure == "RD" & result$rd_unit != "PERCENTAGE_POINTS", na.rm = TRUE)) {
    stop("Every RD row must explicitly use rd_unit = 'PERCENTAGE_POINTS'.")
  }
  if (any(result$effect_measure == "RD" & result$.rd_threshold_available &
          (is.na(result$important_effect_threshold_percentage_points) |
           result$important_effect_threshold_percentage_points <= 0), na.rm = TRUE)) {
    stop("An available RD importance threshold must be a positive number of percentage points.")
  }
  if (any(result$effect_measure == "RD" & result$.rd_threshold_available &
          (is.na(result$threshold_source_or_justification) |
           result$threshold_source_or_justification == ""), na.rm = TRUE)) {
    stop("Each available RD importance threshold requires an independent source or justification.")
  }
  
  result |>
    dplyr::mutate(
      # D2 — Strength of association
      # A CI that includes or touches the null is NS.
      d2_strength = dplyr::case_when(
        effect_measure %in% c("RR", "OR", "HR") & ci_low <= 1 & ci_high >= 1 ~ "NS",
        effect_measure %in% c("RR", "OR", "HR") &
          (pooled_effect >= 2 | pooled_effect <= 0.5) ~ "S",
        effect_measure %in% c("RR", "OR", "HR") ~ "PS",
        
        effect_measure == "RD" & ci_low <= 0 & ci_high >= 0 ~ "NS",
        effect_measure == "RD" & .rd_threshold_available &
          abs(pooled_effect) >= important_effect_threshold_percentage_points ~ "S",
        effect_measure == "RD" ~ "PS",
        TRUE ~ NA_character_
      ),
      
      # D3 — Consistency
      # Direction is primary. When direction cannot be assessed but I-squared is
      # reported, frozen v1.0 applies the conservative PS operational rule.
      d3_consistency = dplyr::case_when(
        stringr::str_detect(direction_alignment, "≥80|>=80") &
          (is.na(i2) | i2 <= 50) ~ "S",
        stringr::str_detect(direction_alignment, "≥80|>=80") &
          !is.na(i2) & i2 > 50 ~ "PS",
        stringr::str_detect(direction_alignment, "60[–-]79") ~ "PS",
        stringr::str_detect(direction_alignment, "<60") ~ "NS",
        stringr::str_detect(stringr::str_to_lower(direction_alignment), "not assessable") &
          !is.na(i2) ~ "PS",
        stringr::str_detect(stringr::str_to_lower(direction_alignment), "not assessable") &
          is.na(i2) ~ "NA",
        TRUE ~ NA_character_
      ),
      
      # Preliminary ACE: D1-D4
      preliminary_ace = dplyr::case_when(
        d1_temporality == "NS" | d2_strength == "NS" ~ "Inconclusive",
        d1_temporality == "PS" | d3_consistency == "NS" ~ "Limited",
        d1_temporality == "S" & d2_strength == "S" &
          d3_consistency == "S" & d4_dose_response == "NS" ~ "Moderate",
        d1_temporality == "S" & d2_strength == "S" &
          d3_consistency == "S" & d4_dose_response != "NS" ~ "Strong",
        TRUE ~ "Moderate"
      ),
      
      # D5 — Review methodological quality
      after_d5 = dplyr::case_when(
        d5_methodology == "ROBUST" ~ preliminary_ace,
        d5_methodology %in% c("SOME CONCERNS", "NOT ASSESSED") ~
          ace_downgrade_one(preliminary_ace),
        d5_methodology == "MAJOR LIMITATIONS" ~
          ace_d5_major_limitations(preliminary_ace),
        TRUE ~ NA_character_
      ),
      
      # D6 — Small-study effects/publication bias
      # Bias detected prevents a final Strong classification; other recognised
      # frozen states do not change the classification after D5.
      final_ace = dplyr::case_when(
        d6_publication_bias == "BIAS DETECTED" ~ ace_d6_bias_cap(after_d5),
        d6_publication_bias %in% c("NO BIAS", "NO BIAS DETECTED",
                                   "NOT ASSESSABLE", "NA") ~ after_d5,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(-.rd_threshold_available)
}
# -----------------------------------------------------------------------------
# Export classified dataset
# -----------------------------------------------------------------------------
ace_classified <- classify_ace_v1(ace)
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
  file = "outputs/ace_summary.csv", overwrite = TRUE)

ace_summary
