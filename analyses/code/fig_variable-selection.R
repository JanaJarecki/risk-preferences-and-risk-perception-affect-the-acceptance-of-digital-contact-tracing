# ==========================================================================
# Visualize variable selection
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(projpred, bayesplot, data.table)
source("setup_figures.R")

# Load variable selection result
cvs <- readRDS("variable_selection.rds")
nvar <- suggest_size(cvs)


# Sum of log predictive densities (ELPD)
# root mean squared error (RMSE)
# as function of number of variables added.


# Number of variables --------------------------------------------------------
# With Cross-validation - ELPD and RMSE in relation to full model
varsel_plot(cvs, stats = c('elpd', 'rmse'), deltas=TRUE) +
  labs(title = "Variable numbers without cross-validation",
  subtitle = "Sum of log predictive densities (ELPD) and RMSE relative to best-fitting model")
ggsave("../figures/variable-selection-nvariables-after-cv.png", width = .w, height = .h)




# Magnitude of most-relevant predictors ---------------------------------------
# Generate the projected reduced model
proj <- project(cvs, nv = nvar, ns = 1000)
# Fun for all plots
plot_group <- function(vars, title, ylabels = variable_labels) {
  # get color scheme based on vars
  if (any(grepl("hon", vars)))    { scheme <- "social"
    } else if (any(grepl("compr", vars))) { scheme <- "other"
      } else { scheme <- grep(vars, names(color_schemes), value=T) }
  color_scheme_set(color_schemes[[scheme]]) # set color scheme
  ylabels <- ylabels[grepl(paste(vars, collapse="|"), names(ylabels))] # y-axis labels
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

risk_perc <- plot_group(vars = "^perc", title = "Risk Perceptions")
risk_seek <- plot_group(vars = "^seek", title = "Risk-Seeking Preferences")
social    <- plot_group(vars = "^honhum|^iwah|^svo",title="Social Preferences")
other     <- plot_group(
  vars=grep("^per|^see|^ho|^iw|^sv",names(cvs$vind[1:nvar]),invert=T, value=T),
  title = "Other Variables")

risk_perc + risk_seek + social + other
ggsave("../figures/fig_mcmc_areas.pdf", width = .w, height = .h)