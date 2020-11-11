# ==========================================================================
# Statistical Analyses of the models
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(data.table, brms, logspline, see, ggdist, kableExtra, magrittr)
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }

# Setup --------------------------------------------------------------------
fit_accept <- readRDS("fitted_models/accept_index_fit_reduced_all.rds")
fit_comply <- readRDS("fitted_models/comply_index_fit_reduced_all.rds")

# Test hypothesis
h <- c("perc_risk_health > 0", "perc_risk_data < 0", "perc_risk_econ > 0", "seek_risk_general < 0", "seek_risk_health < 0", "seek_risk_data > 0", "honhum_score > 0", "svo_angle > 0")


# Table 1 ---------------------------------------------------------------------
tab <- merge(
  hypothesis(fit_accept, c(h, "seek_risk_econ < 0", "iwah_diff_score > 0"))[[1]],
  hypothesis(fit_comply, h)[[1]],
  by = "Hypothesis",
  suffix = c("_a", "_c"),
  all = T)


# Fomat table for print -------------------------------------------------------
source("setup_figures.R") # generates variable labels
tab <- as.data.table(tab)
tab[, predictorf := factor(gsub("\\(|\\).*", "", Hypothesis), levels = names(variable_labels), labels = variable_labels, ordered = TRUE)]
tab <- tab[order(predictorf)]
tab[, predictorf := gsub("-", " ", tstrsplit(predictorf, " ")[[1]])]
tab[, predictionf := paste("$", predictorf, "$")]
tab[, group := fcase(
  grepl("perc", Hypothesis), "Risk Perception",
  grepl("seek", Hypothesis), "Risk-seeking Preference",
  grepl("svo|iwa|honh", Hypothesis), "Social Preferences",
  default = "Other"
)]

# If we must we do tidyverse magrittr style
options(knitr.kable.NA = '--')
tab[, c("group", "predictorf", "Estimate_a", "CI.Lower_a", "CI.Upper_a", "Estimate_c", "CI.Lower_c", "CI.Upper_c")] %>%
  kable(
    format = "latex",
    digits = 2,
    align = c("l","l", "r", "r","l", "r", "r","l"),
    col.names = c("Group", "Predictor","Median", "2.5\\%", "97.5\\%", "Median", "2.5\\%", "97.5\\%"),
    booktabs = TRUE,
    escape = FALSE,
    format.args = list(scientific = F),
    label = "tab_results_1",
    caption = "Results of the Bayesian regression: Effects on the acceptance of and compliance with digital contact tracing."
    ) %>%
  column_spec(1, width = "1.8cm") %>%
  column_spec(2, width = "4.5cm") %>%
  column_spec(3, width = "1cm") %>%
  collapse_rows(1, latex_hline = "major") %>% 
  add_header_above(c(" " = 2, "Acceptance " = 3, "Compliance " = 3)) %>% 
  kable_styling() %>%
  add_footnote(label = "Note. CIs are Bayesian credibility intervalls.", notation="none") %>%
  writeLines(con = paste0("../tables/fullmodel_coef_estimates.tex"))

