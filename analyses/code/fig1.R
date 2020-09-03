# ==========================================================================
# Figure 1
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(data.table, ggdist)
source("setup_figures.R")

# Load data ---------------------------------------------------------------
d <- fread("../../data/processed/data.csv")

variables <- c("compreh_score", "policy_score", "tech_score", "svo_angle", "perc_risk_econ")


plot_variable <- function(var, d) {
  label <- variable_labels[var]
  var <- ensym(var)
  ggplot(data = d,
    aes(x = factor(cut_number(!!var, 3)),
        y = accept_index,
        color = ..y..)) +
    stat_dots(
      position = "dodge",
      quantiles = 40,
      side = "both",
      alpha = .50,
      shape = 16) +
    geom_pointrange(
      stat = "summary",
      fun.data = "mean_sdl",
      fun.args = list(mult = 1),
      size = .6) +
    themejj(facet=TRUE) +
    theme(aspect.ratio = 1) +
    xlab(tstrsplit(label, " ")[[1]]) + # use first word as x-label
    ylab("Acceptance (M, SD)")  +
    ggtitle(sub(" ", "\n", label)) +
    guides(color = "none") +
    scale_colour_steps(low = "black", high = "turquoise")
}

plots <- lapply(variables, plot_variable, d = d)
plots[2:length(plots)] <- lapply(plots[2:length(plots)], function(p) p + theme(axis.title.y = element_blank()))

wrap_plots(plots, nrow=1)