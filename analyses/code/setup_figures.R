# ==========================================================================
# Setup for all figures
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(ggplot2, patchwork, ggsci)
pacman::p_load_gh("janajarecki/themejj")
theme_set(themejj(base_size = 14))

.w <- 7 # width of all plots
.h <- 7 # height of all plots

# Color-blind friendly colors --------------------------------------------
colors <- c(
  perc_risk = "#373F51",
  seek_risk = "#008DD5",
  social = "#DFBBB1"
)

# from lightest to darkest, darkest is the value in colors
color_schemes <- list(
  perc_risk=c("#d7d8dc", "#c3c5ca", "#afb2b9", "#878b96", "#5e6573","#373f51"),
  seek_risk=c("#cce8f6", "#99d1ee", "#66bae5", "#32a3dd", "#1998d9","#008DD5"),
  social =c("#f2e3df", "#efddd8", "#ebd6d0", "#e5c8c0", "#DFBBB1", "#c8a89f"),
  other = tolower(color_scheme_get("teal"))
)

# Define labels for the predictors for all plots --------------------------
variable_labels <- c(
  perc_risk_health = "Health Risk Perception",
  perc_risk_data = "Data-Security Risk Perception",
  perc_risk_econ = "Economic Risk Perception",
  seek_risk_general = "General Risk Seeking",
  seek_risk_health = "Health Risk Seeking",
  seek_risk_data = "Data-Security Risk Seeking",
  seek_risk_economy = "Economic Risk Seeking",
  # econ is correct, need to re-run models.
  seek_risk_econ = "Economic Risk Seeking",
  honhum_score = "Honesty-Humility Personality",
  svo_angle = "Social-Value-Orientation",
  iwah_community = "Identification-Community",
  iwah_world = "Identification-World",
  compreh_score = "Comprehension",
  policy_score = "Policy-Support",
  tech_score = "Technology-Affinity",
  age = "Age"
)

message("Defined vector 'colors', `color_schemes` and 'variable_labels'.")