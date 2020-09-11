# ==========================================================================
# Visualize variable selection
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(projpred, bayesplot, data.table)
source("setup_figures.R")


# Setup --------------------------------------------------------------------
dep_var <- "accept_index" # or: "comply_index"

# Load files ---------------------------------------------------------------
# File is not on github, it is too large
cvs <- readRDS(paste0("fitted_models/", dep_var, "_variable_selection_no_.rds"))
nvar <- suggest_size(cvs)



# Number of variables --------------------------------------------------------
# With Cross-validation - ELPD and RMSE in relation to full model
# Sum of log predictive densities (ELPD) & RMSE  as function of number of variables added.
# varsel_plot(cvs, stats = c('elpd', 'rmse'), deltas=TRUE) +
#   labs(title = "Variable numbers from cross-validation",
#   subtitle = "Sum of log predictive densities (ELPD) and RMSE relative to best-fitting model")




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
  xlim <- if (scheme == "other") c(-.6,.6) else c(-.15,.15)

  suppressMessages(
    mcmc_areas(x = as.matrix(proj), regex_pars = vars,
      prob = 0.89, prob_outer = 0.95) +
    scale_y_discrete(labels = ylabels) +
    theme(axis.text.y = element_text(face = "plain")) +
    vline_0(color = "black", alpha = .6) +
    xlim(xlim) +
    ggtitle(title)
  )
}

# Plot IVs --------------------------------------------------------------------
risk_perc <- plot_group(vars = "^perc", title = "Risk Perceptions")
risk_seek <- plot_group(vars = "^seek", title = "Risk-Seeking Preferences")

social    <- plot_group(vars = "^honhum|^iwah|^svo",title="Social Preferences")

other     <- plot_group(
  vars=grep("^per|^see|^ho|^iw|^sv",names(cvs$vind[1:nvar]),invert=T, value=T),
  title = "Other Variables")
# usig ?patchwork magic
risk_perc + risk_seek + other +
  plot_annotation(
    title = paste("Effects on", ifelse(dep_var == "comply_index", "Compliance with", "Acceptance of"), "Contact Tracing"),
    tag_levels = "A")

ggsave(paste0("../figures/fig_", dep_var, "_mcmc_areas.pdf"), width = .w*1.3, height = .h)