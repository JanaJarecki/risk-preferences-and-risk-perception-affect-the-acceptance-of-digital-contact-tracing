# ==========================================================================
# Visualize variable selection
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(tidybayes, bayesplot, data.table, ggdist, bayestestR, brms)
source("setup_figures.R")


# Setup --------------------------------------------------------------------
dep_var <- "accept_index" # or: "comply_index"

# Load files ---------------------------------------------------------------
# File is not on github, it is too large
fit_reduced_no_social <- readRDS(paste0("fitted_models/", dep_var, "_fit_reduced_no_social.rds"))


# Number of variables --------------------------------------------------------
# With Cross-validation - ELPD and RMSE in relation to full model
# Sum of log predictive densities (ELPD) & RMSE  as function of number of variables added.
# varsel_plot(cvs, stats = c('elpd', 'rmse'), deltas=TRUE) +
#   labs(title = "Variable numbers from cross-validation",
#   subtitle = "Sum of log predictive densities (ELPD) and RMSE relative to best-fitting model")

# Preprocess: get BF ---------------------------------------------------------
# Hypothesis tests
# proj <- project(cvs, nv = nvar, ns = 1000)
# m <- as.matrix(proj)
# prior <- distribution_normal(nrow(m), mean = 0, sd = .1)
# prior <- as.matrix(prior)[, rep(1,ncol(m))]
# colnames(prior) <- colnames(m)
set.seed(42)
# Test Hypothesis about positive, negative, null effect by BF
hyp_pos <- c("perc_risk_health", "perc_risk_econ", "seek_risk_data")
  #"honhum_score", "svo_angle", "iwah_community", "iwah_world")
bf_pos <- hypothesis(fit_reduced_no_social, paste(hyp_pos, "> 0"))
hyp_neg <- c("perc_risk_data", "seek_risk_general", "seek_risk_health", "seek_risk_econ")
bf_neg <- hypothesis(fit_reduced_no_social, paste(hyp_neg, "< 0"))
hyp_null <- setdiff(
  gsub("b_", "", grep("^b_", parnames(fit_reduced_no_social), value=T)), c(hyp_pos, hyp_neg))[-1]
bf_null <- hypothesis(fit_reduced_no_social, paste(hyp_null, "=0"))
bfs <- data.table(variable = c(hyp_pos, hyp_neg, hyp_null), rbind(bf_pos[[1]], bf_neg[[1]], bf_null[[1]]))
setnames(bfs, "Evid.Ratio", "BF")
setnames(bfs, "variable", ".variable")
bfs[grepl("= 0", Hypothesis), BF := 1/BF]
bfs[, BF := ifelse(BF > 100, ">100", ifelse(BF < 1/100, "<1/100", ifelse(BF < 10, sprintf("%.2f",BF), sprintf("%.0f",BF))))]





# Magnitude of most-relevant predictors ---------------------------------------
# Generate the projected reduced model
# Fun for all plots



# Plot IVs --------------------------------------------------------------------
risk_perc <- plot_group(vars = "perc", title = "Risk Perceptions")
risk_seek <- plot_group(vars = "seek", title = "Risk-Seeking Preferences")
other     <- plot_group(vars = paste(gsub("b_", "", grep("per|see|Inter|lp|sigma|prior", parnames(fit_reduced_no_social),invert=TRUE, value=TRUE)), collapse="|"),
  title = "Major Predictors")

# Plot Regression lines -------------------------------------------------------
plot_lines <- function(var, xlab, ylab = NULL, title) {
  p <- plot(conditional_effects(fit_reduced_no_social, var))[[1]] + scale_x_continuous(xlab)  +ylim(-.5,.5) +theme(axis.title.y = element_text(vjust = 0, lineheight = 1), axis.ticks.y = element_blank(), aspect.ratio = 1) + geom_line(color="black", size=1)
  if (is.null(ylab)) {
    p <- p +theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), aspect.ratio = 1) +ylim(-.5,.5)
  } else {
    p <- p + ylab(ylab)
  }
  return(p + ggtitle(title))
}
health_perc <- plot_lines("perc_risk_health", "Health Risk Perception", "\n\nAcceptance of\nContact Tracing", "Health Risk Effects")
health_seek <- plot_lines("seek_risk_health", "Health Risk Seeking", NULL, "")
data_perc <- plot_lines("perc_risk_data", "Data Risk Perception", "\n\nAcceptance of\nContact Tracing", "\nData Risk Effects")
data_seek <- plot_lines("seek_risk_data", "Data Risk Seeking", NULL, "\n")

lines  <- health_perc + (health_seek + plot_layout(tag_level="new")) + (data_perc + plot_layout(tag_level="new")) + (data_seek + plot_layout(tag_level="new")) +plot_layout(nrow=2) &
  theme(
    axis.title = element_text(size = rel(.6)),
    plot.margin=margin(-0.5, unit="lines"))


# usig ?patchwork magic
pp <- other + risk_perc + risk_seek + lines +
  plot_layout(nrow=2, heights = c(.49,.51), widths=c(.5,.5)) +
  plot_annotation(
    title = paste("Predictors of the", ifelse(dep_var == "comply_index", "Compliance with", "Acceptance of"), "Contact Tracing"),
    tag_levels = "A") & 
  theme(
    plot.tag = element_text(size = rel(.8), margin = margin(l=1, r=-2, b=-.6, unit="lines")),
    plot.title = element_text(size = rel(.8)),
    axis.text = element_text(size = rel(.6)),
    plot.margin=margin(0.1, unit="lines"))

ggsave(plot = pp,
  file =paste0("../figures/fig_", dep_var, "_mcmc_areas_no_social.pdf"),
  width = .w*1.2,
  height = .h)
