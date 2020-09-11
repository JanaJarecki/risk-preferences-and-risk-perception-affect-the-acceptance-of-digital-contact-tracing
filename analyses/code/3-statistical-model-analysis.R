# ==========================================================================
# Statistical Analyses of the models
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(data.table, bayestestR, projpred, logspline, see, ggdist, kable, kableExtra, magrittr)
# set working directory to THIS file location (if rstudio)
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }

# Setup --------------------------------------------------------------------
dep_var <- "accept_index" # or "comply_index"
cvs <- readRDS(paste0("fitted_models/", dep_var, "_variable_selection.rds"))
# Generate the projected reduced model
proj <- project(cvs, nv = nvar, ns = 1000)

# Hypothesis tests ---------------------------------------------------------
# Normal prior
m <- as.matrix(proj)
prior <- distribution_normal(nrow(m), mean = 0, sd = .1)
prior <- as.matrix(prior)[, rep(1,ncol(m))]
colnames(prior) <- colnames(m)

# Test Hypothesis about positive, negative, null effect by BF
hyp_pos <- c("perc_risk_health", "perc_risk_econ", "seek_risk_data")
  #"honhum_score", "svo_angle", "iwah_community", "iwah_world")
bf_pos <- bayesfactor_parameters(m[, hyp_pos], prior[, hyp_pos], direction=">")

hyp_neg <- c("perc_risk_data", "seek_risk_general", "seek_risk_health", "seek_risk_econ")
bf_neg <- bayesfactor_parameters(m[, hyp_neg], prior[, hyp_neg], direction="<")

hyp_null <- setdiff(colnames(m)[-c(1, ncol(m))], c(hyp_pos, hyp_neg))
bf_null <- bayesfactor_parameters(m[, hyp_null], prior[, hyp_null], direction = "=")


# Table 1 ---------------------------------------------------------------------
tab <- rbindlist(apply(m, 2, median_hdi), id="predictor")
BFs <- data.table(
  predictor = c(hyp_pos, hyp_neg, hyp_null),
  prediction = c(rep(">", length(hyp_pos)), rep("<", length(hyp_neg)), rep("-", length(hyp_null))),
  BF = c(bf_pos$BF, bf_neg$BF, bf_null$BF))
tab <- tab[BFs, on = "predictor"]

# Fomat table for print -------------------------------------------------------
source("setup_figures.R") # generates variable labels
tab[, predictorf := factor(predictor, levels = names(variable_labels), labels = variable_labels, ordered = TRUE)]
tab <- tab[order(predictorf)]
tab[, predictorf := gsub("-", " ", tstrsplit(predictorf, " ")[[1]])]
tab[, predictionf := paste("$", prediction, "$")]
tab[, group := fcase(
  grepl("^perc", predictor), "Risk Perception",
  grepl("^seek", predictor), "Risk-seeking Preference",
  grepl("svo|iwa|honh", predictor), "Social Preferences",
  default = "Other"
)]
tab[, BF_f := fcase(
  BF > 100, "$>$ 1000",
  BF < 1/100, "$<$ 1/1000",
  BF > 10, sprintf("%f", BF),
  BF < 1/10, paste(1, round(1/BF), sep="/"),
  BF < 10 & BF > 1/10, sprintf("%.2f", BF))]

# If we must we do tidyverse magrittr style
tab[, c("group", "predictorf", "y", "ymin", "ymax", "predictionf", "BF_f")] %>%
  kable(
    format = "latex",
    digits = 2,
    align = c("l","l", "r", "r","l","c","r"),
    col.names = c("", "Predictor","Median", "2.5\\%", "97.5\\%","Prediction","BF"),
    booktabs = TRUE,
    escape = FALSE,
    format.args = list(scientific = F)) %>%
  column_spec(1, width = "2cm") %>%
  column_spec(3, width = "1cm") %>%
  collapse_rows(1, latex_hline = "major") %>% 
  add_header_above(c(" " = 2, "Estimates " = 3)) %>% 
  kable_styling() %>%
  writeLines(con = paste0("../tables/", dep_var, "_estimates_no_social.tex"))

