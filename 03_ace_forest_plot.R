library(ggplot2)
library(dplyr)
library(patchwork)
library(stringr)

# =============================================================================
# DATA: Moderate and Limited ACE classifications only
# =============================================================================

df <- data.frame(
  exposure = c(
    "Inflammatory bowel disease",
    "Ulcerative colitis",
    "Chronic kidney disease",
    "End-stage renal disease",
    "Absolute eosinopenia",
    "Immunosuppressive therapy",
    "R014/020",
    "R027/NAP1",
    "R027/NAP1",
    "Cirrhosis",
    "Emergent colectomy",
    "Age ≥75 years",
    "Treated vs untreated NAAT+/Toxin−",
    "Treated vs untreated NAAT+/Toxin−"
  ),
  outcome = c(
    "Mortality",
    "Mortality",
    "Mortality",
    "Mortality",
    "All-cause mortality",
    "All-cause mortality",
    "30-day attributable mortality",
    "30-day attributable mortality",
    "30-day all-cause mortality",
    "In-hospital mortality",
    "In-hospital / 30-day mortality",
    "Short-term mortality",
    "30-day all-cause mortality",
    "60-day all-cause mortality"
  ),
  est = c(
    4.39, 4.39, 1.76, 1.58,
    2.35, 1.77, 0.46, 1.96, 1.57,
    1.68, 0.70, 2.31,
    -7.45, -7.37
  ),
  lo = c(
    3.56, 3.44, 1.26, 1.37,
    1.84, 1.38, 0.29, 1.23, 1.15,
    1.29, 0.49, 1.27,
    -12.29, -13.05
  ),
  hi = c(
    5.42, 5.61, 2.47, 1.83,
    2.99, 2.28, 0.75, 3.13, 2.16,
    1.85, 0.99, 4.20,
    -2.60, -1.70
  ),
  em = c(
    "OR", "OR", "RR", "RR",
    "RR", "OR", "RR", "RR", "RR",
    "OR", "OR", "OR",
    "RD", "RD"
  ),
  k = c(
    13, 9, 10, 2,
    3, 12, 3, 8, 21,
    9, 6, 7,
    3, 2
  ),
  classification = c(
    "Moderate", "Moderate", "Limited", "Limited",
    "Limited", "Limited", "Limited", "Limited", "Limited",
    "Limited", "Limited", "Limited",
    "Limited", "Limited"
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# STYLE
# =============================================================================

tier_num <- c("Moderate" = 2, "Limited" = 1)

col_tier <- c(
  "Moderate" = "#B8860B",
  "Limited"  = "#C0522A"
)

bg_tier <- c(
  "Moderate" = "#FFF3CD",
  "Limited"  = "#FDE8C8"
)

bg_fig <- "#F8FBFF"

# =============================================================================
# HELPER FUNCTION
# =============================================================================
make_ace_panel <- function(dat, panel_title, log_scale = TRUE) {
  
  dat <- dat %>%
    mutate(
      em = trimws(em),
      tier_num = tier_num[classification],
      est_fmt = ifelse(
        em == "RD",
        sprintf("%.2f (%.2f to %.2f)", est, lo, hi),
        sprintf("%.2f (%.2f–%.2f)", est, lo, hi)
      )
    ) %>%
    arrange(desc(tier_num), desc(abs(est))) %>%
    mutate(y = rev(seq_len(n())) * 1.18)
  
  # Safety: log scale can only plot positive ratio measures
  if (log_scale) {
    dat <- dat %>% filter(est > 0, lo > 0, hi > 0)
  }
  
  y_max <- max(dat$y)
  y_hdr <- y_max + 0.9
  
  x_min <- if (log_scale) 0.25 else -14
  x_max <- if (log_scale) 6.8 else 4
  
  seps <- dat %>%
    arrange(desc(y)) %>%
    mutate(next_class = lead(classification),
           sep_y = y - 0.59) %>%
    filter(!is.na(next_class), classification != next_class) %>%
    pull(sep_y)
  
  # -------------------------
  # LEFT PANEL
  # -------------------------
  p_left <- ggplot(dat, aes(y = y)) +
    geom_rect(
      aes(
        xmin = 0.35, xmax = 1,
        ymin = y - 0.52, ymax = y + 0.52,
        fill = classification
      ),
      alpha = 0.22
    ) +
    scale_fill_manual(values = bg_tier, guide = "none") +
    geom_text(
      aes(x = 0.98, label = exposure),
      hjust = 1,
      size = 3.0,
      colour = "#111111",
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 0.98, y = y - 0.34, label = outcome),
      hjust = 1,
      size = 2.5,
      colour = "#666666",
      fontface = "italic"
    ) +
    annotate(
      "text",
      x = 1, y = y_hdr,
      label = "Exposure / Outcome",
      hjust = 1,
      size = 3.2,
      colour = "#1F3864",
      fontface = "bold"
    ) +
    scale_x_continuous(limits = c(0.35, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.3, y_max + 1.3), expand = c(0, 0)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = bg_fig, colour = NA),
      plot.margin = margin(15, 0, 15, 2)
    )
  
  for (s in seps) {
    p_left <- p_left +
      annotate("segment", x = 0.35, xend = 1, y = s, yend = s,
               colour = "#BBBBBB", linewidth = 0.4)
  }
  
  # -------------------------
  # FOREST PANEL
  # -------------------------
  p_forest <- ggplot(dat, aes(y = y)) +
    geom_rect(
      aes(
        xmin = x_min, xmax = x_max,
        ymin = y - 0.52, ymax = y + 0.52,
        fill = classification
      ),
      alpha = 0.22
    ) +
    scale_fill_manual(values = bg_tier, guide = "none") +
    geom_vline(
      xintercept = ifelse(log_scale, 1, 0),
      linetype = "dashed",
      colour = "#777777",
      linewidth = 0.55
    ) +
    geom_segment(
      aes(x = lo, xend = hi, y = y, yend = y, colour = classification),
      linewidth = 1.6
    ) +
    geom_point(
      aes(x = est, colour = classification),
      shape = 18,
      size = 4.8
    ) +
    scale_colour_manual(values = col_tier, guide = "none") +
    annotate(
      "text",
      x = ifelse(log_scale, 1.6, mean(c(x_min, x_max))),
      y = y_hdr,
      label = panel_title,
      size = 3.2,
      colour = "#1F3864",
      fontface = "bold"
    ) +
    scale_y_continuous(limits = c(0.3, y_max + 1.3), expand = c(0, 0)) +
    labs(
      x = ifelse(log_scale, "Effect estimate", "Risk difference"),
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.background = element_rect(fill = bg_fig, colour = NA),
      panel.background = element_rect(fill = bg_fig, colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E0E0E0", linewidth = 0.3),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(size = 9, colour = "#444444"),
      plot.margin = margin(15, 4, 15, 4)
    )
  
  for (s in seps) {
    p_forest <- p_forest +
      annotate("segment", x = x_min, xend = x_max, y = s, yend = s,
               colour = "#BBBBBB", linewidth = 0.4)
  }
  
  if (log_scale) {
    p_forest <- p_forest +
      scale_x_log10(
        breaks = c(0.5, 1, 2, 5),
        labels = c("0.5", "1.0", "2.0", "5.0")
      ) +
      coord_cartesian(xlim = c(x_min, x_max))
  } else {
    p_forest <- p_forest +
      scale_x_continuous(
        breaks = c(-12, -8, -4, 0, 4)
      ) +
      coord_cartesian(xlim = c(x_min, x_max))
  }
  
  # -------------------------
  # RIGHT PANEL
  # -------------------------
  p_right <- ggplot(dat, aes(y = y)) +
    geom_rect(
      aes(
        xmin = 0, xmax = 5.6,
        ymin = y - 0.52, ymax = y + 0.52,
        fill = classification
      ),
      alpha = 0.22
    ) +
    scale_fill_manual(values = bg_tier, guide = "none") +
    geom_text(aes(x = 0.5, label = em), size = 3.1, colour = "#333333") +
    geom_text(
      aes(x = 2.1, label = est_fmt),
      size = 2.9,
      colour = "#111111",
      family = "mono"
    ) +
    geom_text(aes(x = 3.55, label = k), size = 3.1, colour = "#333333") +
    geom_label(
      aes(x = 4.8, label = classification, colour = classification),
      fill = "white",
      size = 3.0,
      fontface = "bold",
      label.padding = unit(0.25, "lines"),
      label.r = unit(0.22, "lines"),
      label.size = 0.5,
      show.legend = FALSE
    ) +
    scale_colour_manual(values = col_tier, guide = "none") +
    annotate("text", x = 0.5, y = y_hdr, label = "Measure",
             size = 3.0, colour = "#1F3864", fontface = "bold") +
    annotate("text", x = 2.1, y = y_hdr, label = "Estimate (95% CI)",
             size = 3.0, colour = "#1F3864", fontface = "bold") +
    annotate("text", x = 3.55, y = y_hdr, label = "k",
             size = 3.0, colour = "#1F3864", fontface = "bold") +
    annotate("text", x = 4.8, y = y_hdr, label = "ACE",
             size = 3.0, colour = "#1F3864", fontface = "bold") +
    scale_x_continuous(limits = c(0, 5.6), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.3, y_max + 1.3), expand = c(0, 0)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = bg_fig, colour = NA),
      plot.margin = margin(15, 8, 15, 0)
    )
  
  for (s in seps) {
    p_right <- p_right +
      annotate("segment", x = 0, xend = 5.6, y = s, yend = s,
               colour = "#BBBBBB", linewidth = 0.4)
  }
  
  p_left + p_forest + p_right +
    plot_layout(widths = c(1.7, 3.7, 3.6))
}

# =============================================================================
# SPLIT DATA
# =============================================================================

df_ratio <- df %>% filter(em %in% c("OR", "RR"))
df_rd    <- df %>% filter(em == "RD")

ratio_panel <- make_ace_panel(
  df_ratio,
  panel_title = "Effect estimates (95% CI)",
  log_scale = TRUE
)

rd_panel <- make_ace_panel(
  df_rd,
  panel_title = "Risk differences (95% CI)",
  log_scale = FALSE
)

# =============================================================================
# COMBINE
# =============================================================================

combined <- ratio_panel / rd_panel +
  plot_layout(heights = c(4.8, 1.4)) +
  plot_annotation(
    title = "ACE framework: Credibility-annotated effect estimates",
    subtitle = "Risk factors for CDI mortality | Selected Exposure-Outcome associations with Moderate or Limited ACE classification",
    caption = paste0(
      "ACE = Assessing Credibility of Exposure–Outcome Associations; ",
      "OR = odds ratio; RR = risk ratio; RD = risk difference; ",
      "k = number of primary studies in the anchor meta-analysis.\n",
      "Effect estimates are shown on a log scale; risk differences are shown separately on a linear scale.\n",
      "Emergency colectomy OR < 1.0: protective direction (non-colectomy patients had higher mortality).\n",
      "CKD capped at Moderate: funnel plot asymmetry detected.\n",
      "Moderate = consistent evidence with limitations; Limited = suggestive evidence with important uncertainty"
    ),
    theme = theme(
      plot.background = element_rect(fill = bg_fig, colour = NA),
      plot.title = element_text(
        size = 13,
        face = "bold",
        colour = "#1F3864",
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 9.5,
        colour = "#555555",
        margin = margin(b = 4)
      ),
      plot.caption = element_text(
        size = 7.5,
        colour = "#666666",
        hjust = 0,
        lineheight = 1.35,
        margin = margin(t = 10)
      ),
      plot.margin = margin(12, 12, 12, 12)
    )
  )
combined
# =============================================================================
# SAVE
# =============================================================================

ggsave(
  "ACE_credibility_annotated_forest_20260503.png",
  combined,
  width = 20,
  height = 10,
  dpi = 300,
  bg = bg_fig
)

ggsave(
  "ACE_credibility_annotated_forest_20260503.tiff",
  combined,
  width = 20,
  height = 10,
  dpi = 300,
  compression = "lzw",
  bg = bg_fig
)

# =============================================================================
# ACE vs Ioannidis 
# =============================================================================


library(ggplot2)
library(dplyr)
library(patchwork)

# =============================================================================
# DATA: Moderate and Limited ACE classifications only
# =============================================================================

df <- data.frame(
  exposure = c(
    "Inflammatory bowel disease",
    "Ulcerative colitis",
    "Chronic kidney disease",
    "End-stage renal disease",
    "Absolute eosinopenia",
    "Immunosuppressive therapy",
    "R014/020",
    "R027/NAP1",
    "R027/NAP1",
    "Cirrhosis",
    "Emergent colectomy",
    "Age >=75 years",
    "Treated vs untreated NAAT+/Toxin-",
    "Treated vs untreated NAAT+/Toxin-"
  ),
  outcome = c(
    "Mortality",
    "Mortality",
    "Mortality",
    "Mortality",
    "All-cause mortality",
    "All-cause mortality",
    "30-day attributable mortality",
    "30-day attributable mortality",
    "30-day all-cause mortality",
    "In-hospital mortality",
    "In-hospital / 30-day mortality",
    "Short-term mortality",
    "30-day all-cause mortality",
    "60-day all-cause mortality"
  ),
  est = c(
    4.39, 4.39, 1.76, 1.58,
    2.35, 1.77, 0.46, 1.96, 1.57,
    1.68, 0.70, 2.31,
    -7.45, -7.37
  ),
  lo = c(
    3.56, 3.44, 1.26, 1.37,
    1.84, 1.38, 0.29, 1.23, 1.15,
    1.29, 0.49, 1.27,
    -12.29, -13.05
  ),
  hi = c(
    5.42, 5.61, 2.47, 1.83,
    2.99, 2.28, 0.75, 3.13, 2.16,
    1.85, 0.99, 4.20,
    -2.60, -1.70
  ),
  em = c(
    "OR", "OR", "RR", "RR",
    "RR", "OR", "RR", "RR", "RR",
    "OR", "OR", "OR",
    "RD", "RD"
  ),
  k = c(
    13, 9, 10, 2,
    3, 12, 3, 8, 21,
    9, 6, 7,
    3, 2
  ),
  classification = c(
    "Moderate", "Moderate", "Limited", "Limited",
    "Limited", "Limited", "Limited", "Limited", "Limited",
    "Limited", "Limited", "Limited",
    "Limited", "Limited"
  ),
  ioannidis = c(
    "Class IV/Weak", "Class IV/Weak", "Class IV/Weak", "Class IV/Weak",
    "Class IV/Weak", "Class IV/Weak", "Class IV/Weak", "Class IV/Weak", "Class IV/Weak",
    "Class IV/Weak", "Class IV/Weak", "Class IV/Weak",
    "Class IV/Weak", "Class IV/Weak"
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# STYLE
# =============================================================================

tier_num <- c("Moderate" = 2, "Limited" = 1)

col_tier <- c(
  "Moderate" = "#B8860B",
  "Limited"  = "#C0522A"
)

bg_tier <- c(
  "Moderate" = "#FFF3CD",
  "Limited"  = "#FDE8C8"
)

bg_fig <- "#F8FBFF"

# =============================================================================
# HELPER FUNCTION
# =============================================================================

make_ace_panel <- function(dat, panel_title, log_scale = TRUE) {
  
  dat <- dat %>%
    mutate(
      em = trimws(em),
      tier_num = tier_num[classification],
      est_fmt = ifelse(
        em == "RD",
        sprintf("%.2f (%.2f to %.2f)", est, lo, hi),
        sprintf("%.2f (%.2f–%.2f)", est, lo, hi)
      )
    ) %>%
    arrange(desc(tier_num), desc(abs(est))) %>%
    mutate(y = rev(seq_len(n())) * 1.18)
  
  if (log_scale) {
    dat <- dat %>% filter(est > 0, lo > 0, hi > 0)
  }
  
  y_max <- max(dat$y)
  y_hdr <- y_max + 0.9
  
  x_min <- if (log_scale) 0.25 else -14
  x_max <- if (log_scale) 6.8 else 4
  
  seps <- dat %>%
    arrange(desc(y)) %>%
    mutate(
      next_class = lead(classification),
      sep_y = y - 0.59
    ) %>%
    filter(!is.na(next_class), classification != next_class) %>%
    pull(sep_y)
  
  # -------------------------
  # LEFT PANEL
  # -------------------------
  p_left <- ggplot(dat, aes(y = y)) +
    geom_rect(
      aes(
        xmin = 0.38, xmax = 1,
        ymin = y - 0.52, ymax = y + 0.52,
        fill = classification
      ),
      alpha = 0.22
    ) +
    scale_fill_manual(values = bg_tier, guide = "none") +
    geom_text(
      aes(x = 0.98, label = exposure),
      hjust = 1,
      size = 3.15,
      colour = "#111111",
      fontface = "bold"
    ) +
    geom_text(
      aes(x = 0.98, y = y - 0.35, label = outcome),
      hjust = 1,
      size = 2.65,
      colour = "#666666",
      fontface = "italic"
    ) +
    annotate(
      "text",
      x = 1, y = y_hdr,
      label = "Exposure / Outcome",
      hjust = 1,
      size = 3.35,
      colour = "#1F3864",
      fontface = "bold"
    ) +
    scale_x_continuous(limits = c(0.38, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.3, y_max + 1.3), expand = c(0, 0)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = bg_fig, colour = NA),
      plot.margin = margin(15, 0, 15, 2)
    )
  
  for (s in seps) {
    p_left <- p_left +
      annotate(
        "segment",
        x = 0.38, xend = 1,
        y = s, yend = s,
        colour = "#BBBBBB",
        linewidth = 0.4
      )
  }
  
  # -------------------------
  # FOREST PANEL
  # -------------------------
  p_forest <- ggplot(dat, aes(y = y)) +
    geom_rect(
      aes(
        xmin = x_min, xmax = x_max,
        ymin = y - 0.52, ymax = y + 0.52,
        fill = classification
      ),
      alpha = 0.22
    ) +
    scale_fill_manual(values = bg_tier, guide = "none") +
    geom_vline(
      xintercept = ifelse(log_scale, 1, 0),
      linetype = "dashed",
      colour = "#777777",
      linewidth = 0.55
    ) +
    geom_segment(
      aes(
        x = lo, xend = hi,
        y = y, yend = y,
        colour = classification
      ),
      linewidth = 1.65
    ) +
    geom_point(
      aes(x = est, colour = classification),
      shape = 18,
      size = 5.0
    ) +
    scale_colour_manual(values = col_tier, guide = "none") +
    annotate(
      "text",
      x = ifelse(log_scale, 1.6, mean(c(x_min, x_max))),
      y = y_hdr,
      label = panel_title,
      size = 3.35,
      colour = "#1F3864",
      fontface = "bold"
    ) +
    scale_y_continuous(limits = c(0.3, y_max + 1.3), expand = c(0, 0)) +
    labs(
      x = ifelse(log_scale, "Effect estimate", "Risk difference"),
      y = NULL
    ) +
    theme_minimal(base_size = 10.5) +
    theme(
      plot.background = element_rect(fill = bg_fig, colour = NA),
      panel.background = element_rect(fill = bg_fig, colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E0E0E0", linewidth = 0.3),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(size = 9.5, colour = "#444444"),
      axis.text.x = element_text(size = 9.5, colour = "#444444"),
      plot.margin = margin(15, 3, 15, 3)
    )
  
  for (s in seps) {
    p_forest <- p_forest +
      annotate(
        "segment",
        x = x_min, xend = x_max,
        y = s, yend = s,
        colour = "#BBBBBB",
        linewidth = 0.4
      )
  }
  
  if (log_scale) {
    p_forest <- p_forest +
      scale_x_log10(
        breaks = c(0.5, 1, 2, 5),
        labels = c("0.5", "1.0", "2.0", "5.0")
      ) +
      coord_cartesian(xlim = c(x_min, x_max))
  } else {
    p_forest <- p_forest +
      scale_x_continuous(
        breaks = c(-12, -8, -4, 0, 4)
      ) +
      coord_cartesian(xlim = c(x_min, x_max))
  }
  
  # -------------------------
  # RIGHT PANEL
  # Reduced column spacing:
  # Measure | Estimate | k | ACE | Ioannidis
  # -------------------------
  x_em  <- 0.45
  x_est <- 1.75
  x_k   <- 3.05
  x_ace <- 4.05
  x_io  <- 5.20
  
  p_right <- ggplot(dat, aes(y = y)) +
    geom_rect(
      aes(
        xmin = 0, xmax = 5.85,
        ymin = y - 0.52, ymax = y + 0.52,
        fill = classification
      ),
      alpha = 0.22
    ) +
    scale_fill_manual(values = bg_tier, guide = "none") +
    geom_text(
      aes(x = x_em, label = em),
      size = 3.25,
      colour = "#333333"
    ) +
    geom_text(
      aes(x = x_est, label = est_fmt),
      size = 3.25,
      colour = "#111111",
      family = "mono"
    ) +
    geom_text(
      aes(x = x_k, label = k),
      size = 3.25,
      colour = "#333333"
    ) +
    geom_label(
      aes(x = x_ace, label = classification, colour = classification),
      fill = "white",
      size = 3.05,
      fontface = "bold",
      label.padding = unit(0.23, "lines"),
      label.r = unit(0.20, "lines"),
      label.size = 0.48,
      show.legend = FALSE
    ) +
    geom_text(
      aes(x = x_io, label = ioannidis),
      size = 3.1,
      colour = "#444444",
      fontface = "plain"
    ) +
    scale_colour_manual(values = col_tier, guide = "none") +
    annotate(
      "text",
      x = x_em, y = y_hdr,
      label = "Measure",
      size = 3.15,
      colour = "#1F3864",
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = x_est, y = y_hdr,
      label = "Estimate (95% CI)",
      size = 3.15,
      colour = "#1F3864",
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = x_k, y = y_hdr,
      label = "k",
      size = 3.15,
      colour = "#1F3864",
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = x_ace, y = y_hdr,
      label = "ACE class",
      size = 3.15,
      colour = "#1F3864",
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = x_io, y = y_hdr,
      label = "Ioannidis",
      size = 3.15,
      colour = "#1F3864",
      fontface = "bold"
    ) +
    scale_x_continuous(limits = c(0, 5.85), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.3, y_max + 1.3), expand = c(0, 0)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = bg_fig, colour = NA),
      plot.margin = margin(15, 8, 15, 0)
    )
  
  for (s in seps) {
    p_right <- p_right +
      annotate(
        "segment",
        x = 0, xend = 5.85,
        y = s, yend = s,
        colour = "#BBBBBB",
        linewidth = 0.4
      )
  }
  
  p_left + p_forest + p_right +
    plot_layout(widths = c(1.55, 3.75, 4.05))
}

# =============================================================================
# SPLIT DATA
# =============================================================================

df <- df %>% mutate(em = trimws(em))

df_ratio <- df %>% filter(em %in% c("OR", "RR"))
df_rd    <- df %>% filter(em == "RD")

ratio_panel <- make_ace_panel(
  df_ratio,
  panel_title = "Effect estimate (95% CI)",
  log_scale = TRUE
)

rd_panel <- make_ace_panel(
  df_rd,
  panel_title = "Risk differences (95% CI)",
  log_scale = FALSE
)

# =============================================================================
# COMBINE
# =============================================================================

combined <- ratio_panel / rd_panel +
  plot_layout(heights = c(4.8, 1.4)) +
  plot_annotation(
    title = "ACE framework: credibility-annotated effect estimates",
    subtitle = paste0(
      "Risk factors for CDI mortality | ",
      "Selected exposure-outcome associations with Moderate or Limited ACE classification"
    ),
    caption = paste0(
      "ACE = Assessing Credibility of Exposure–Outcome Associations; ",
      "OR = odds ratio; RR = risk ratio; RD = risk difference; ",
      "k = number of primary studies in the anchor meta-analysis.\n",
      "Effect estimates are shown on a log scale; risk differences are shown separately on a linear scale. ",
      "Ioannidis classifications are shown for comparison; all statistically significant associations were Class IV/Weak because none included >1000 deaths.\n",
      "Emergency colectomy OR <1.0 indicates lower mortality in the colectomy group. ",
      "Chronic kidney disease and End-stage renal disease was downgraded to Limited due to major limitations in methodological quality of included anchor review.\n",
      "Moderate = consistent evidence with limitations; Limited = suggestive evidence with important uncertainty."
    ),
    theme = theme(
      plot.background = element_rect(fill = bg_fig, colour = NA),
      plot.title = element_text(
        size = 13.5,
        face = "bold",
        colour = "#1F3864",
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 10,
        colour = "#555555",
        margin = margin(b = 4)
      ),
      plot.caption = element_text(
        size = 8,
        colour = "#666666",
        hjust = 0,
        lineheight = 1.35,
        margin = margin(t = 10)
      ),
      plot.margin = margin(12, 12, 12, 12)
    )
  )

combined

# =============================================================================
# SAVE
# =============================================================================

ggsave(
  "ACE_credibility_annotated_forest_ace_ioannidis_20260503.png",
  combined,
  width = 20,
  height = 10.5,
  dpi = 300,
  bg = bg_fig
)

ggsave(
  "ACE_credibility_annotated_forest_ace_Ioannidis_20260503.tiff",
  combined,
  width = 20,
  height = 10.5,
  dpi = 300,
  compression = "lzw",
  bg = bg_fig
)
