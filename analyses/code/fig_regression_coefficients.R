# ==========================================================================
# Visualize variable selection
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(projpred, bayesplot, data.table, ggdist, bayestestR)
source("setup_figures.R")


# Setup --------------------------------------------------------------------
dep_var <- "accept_index" # or: "comply_index"

# Load files ---------------------------------------------------------------
# File is not on github, it is too large
cvs <- readRDS(paste0("fitted_models/", dep_var, "_variable_selection_no_social.rds"))
nvar <- suggest_size(cvs)



# Number of variables --------------------------------------------------------
# With Cross-validation - ELPD and RMSE in relation to full model
# Sum of log predictive densities (ELPD) & RMSE  as function of number of variables added.
# varsel_plot(cvs, stats = c('elpd', 'rmse'), deltas=TRUE) +
#   labs(title = "Variable numbers from cross-validation",
#   subtitle = "Sum of log predictive densities (ELPD) and RMSE relative to best-fitting model")

# Preprocess: get BF ---------------------------------------------------------
# Hypothesis tests
proj <- project(cvs, nv = nvar, ns = 1000)
m <- as.matrix(proj)
prior <- distribution_normal(nrow(m), mean = 0, sd = .1)
prior <- as.matrix(prior)[, rep(1,ncol(m))]
colnames(prior) <- colnames(m)
set.seed(42)
# Test Hypothesis about positive, negative, null effect by BF
hyp_pos <- c("perc_risk_health", "perc_risk_econ", "seek_risk_data")
  #"honhum_score", "svo_angle", "iwah_community", "iwah_world")
bf_pos <- bayesfactor_parameters(m[, hyp_pos], prior[, hyp_pos], direction=">")
hyp_neg <- c("perc_risk_data", "seek_risk_general", "seek_risk_health", "seek_risk_econ")
bf_neg <- bayesfactor_parameters(m[, hyp_neg], prior[, hyp_neg], direction="<")
hyp_null <- setdiff(colnames(m)[-c(1, ncol(m))], c(hyp_pos, hyp_neg))
bf_null <- bayesfactor_parameters(m[, hyp_null], prior[, hyp_null], direction = "=")
bfs <- data.table(variable = c(hyp_pos, hyp_neg, hyp_null), rbind(bf_pos, bf_neg, bf_null))
bfs[, BF := ifelse(BF > 100, ">100", ifelse(BF < 1/100, "<1/100", sprintf("%.2f",BF)))]




# Magnitude of most-relevant predictors ---------------------------------------
# Generate the projected reduced model
proj <- project(cvs, nv = nvar, ns = 1000)
# Fun for all plots
plot_group <- function(vars, title, ylabels = variable_labels) {
  names <- grep(paste(vars, collapse="|"), colnames(as.matrix(proj)), value=T)
  # get color scheme based on vars
  if (any(grepl("hon", vars)))    { scheme <- "social"
    } else if (any(grepl("compr", vars))) { scheme <- "other"
      } else { scheme <- grep(vars, names(color_schemes), value=T) }
  color_scheme_set(color_schemes[[scheme]]) # set color scheme
  ylabels <- ylabels[names] # y-axis labels
  ylabels <- tstrsplit(ylabels, " ")[[1]] # only first word
  ylabels <- gsub("-", " ", ylabels) # no hyphens
  xlim <- if (grepl("Other", title)) c(-0.9,0.9) else c(-0.3,0.3)

  proj_long <- melt(as.data.table(as.matrix(proj)), measure.vars = patterns(vars))
  mqi <- proj_long[, median_qi(value), by = variable]
  mqi[, label := paste0(
    sprintf("%.2f", y),
    "\n[", paste0(sprintf("%.2f", ymin)),
      "; ", paste0(sprintf("%.2f", ymax)),
      "]")]

  ggplot(proj_long, aes(x = value, y = variable)) +
    geom_blank(data = data.table(variable = c(0.5,4.5), value = range(proj_long$value))) +
    geom_vline(xintercept = 0, color = "black", alpha = .5, linetype = 2) +
    stat_halfeye(
      .width = c(.95, .89, .5),
      slab_fill = color_schemes[[scheme]][1],
      interval_colour = color_schemes[[scheme]][5],
      slab_alpha = .7,
      point_colour = color_schemes[[scheme]][6],
      point_fill = color_schemes[[scheme]][6],
      point_size = 2,
    ) +
    geom_text(data=mqi, aes(x = y, label = label),
      nudge_y = .3,
      family = .default_font,
      size = 2.8,
      lineheight = .9,
      color = color_schemes[[scheme]][6])+
    geom_label(data=bfs[grepl(vars, variable)], aes(x = max(xlim), label = BF),
        family = .default_font,
        nudge_x = -0.02,
        fill = "white",
        label.size = 0,
        label.padding  = unit(0.13, "lines")) +
    geom_text(aes(label = "BF[H]", x= max(xlim), y = nrow(mqi) + .5),
      parse=TRUE,
      family = .default_font,
      inherit.aes=FALSE) +
    scale_y_discrete(labels = ylabels) +
    scale_x_continuous(breaks = round(seq(xlim[1], xlim[2], .2), 1)) +
    coord_cartesian(xlim = force(xlim)) +
    ggtitle(title) +
    theme(
      plot.margin = unit(c(.5,.5,.5,.5), "lines"),
      axis.title = element_blank(),
      panel.grid.major.y = element_line(),
      aspect.ratio = .9)
}


# Plot IVs --------------------------------------------------------------------
risk_perc <- plot_group(vars = "^perc", title = "Risk Perceptions")
risk_seek <- plot_group(vars = "^seek", title = "Risk-Seeking Preferences")
# social    <- plot_group(vars = "^honhum|^iwah|^svo",title="Social Preferences")
other     <- plot_group(vars = paste(grep("^per|^see|^ho|^iw|^sv",names(cvs$vind[1:nvar]),invert=TRUE, value=TRUE), collapse="|"),
  title = "Other Variables")

# usig ?patchwork magic
pp <- risk_perc + risk_seek + other +
  plot_layout(nrow=2) +
  plot_annotation(
    title = paste("Effects on the", ifelse(dep_var == "comply_index", "Compliance with", "Acceptance of"), "Contact Tracing"),
    tag_levels = "A")

ggsave(plot = pp,
  file =paste0("../figures/fig_", dep_var, "_mcmc_areas_no_social.pdf"),
  width = .w*1.2,
  height = .h)