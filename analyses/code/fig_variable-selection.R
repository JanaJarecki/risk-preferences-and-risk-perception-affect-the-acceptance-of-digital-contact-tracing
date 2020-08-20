# ==========================================================================
# Visualize variable selection
# ==========================================================================
source("1-statistical-analysis.R")
source("setup_figures.R")

# Sum of log predictive densities (ELPD)
# root mean squared error (RMSE)
# as function of number of variables added.


# Number of variables --------------------------------------------------------
# Without Cross-validation - ELPD and RMSE on absolute scale
varsel_plot(vs, stats=c('elpd', 'rmse')) +
  labs(title = "Variable numbers without cross-validation",
  subtitle = "Sum of log predictive densities (ELPD) and RMSE")
ggsave("../figures/variable-selection-nvariables-no-cv.png")

# With Cross-validation - ELPD and RMSE in relation to full model
varsel_plot(cvs, stats = c('elpd', 'rmse'), deltas=TRUE) +
  labs(title = "Variable numbers without cross-validation",
  subtitle = "Sum of log predictive densities (ELPD) and RMSE relative to best-fitting model")
ggsave("../figures/variable-selection-nvariables-after-cv.png")


# The most-relevant predictors ----------------------------------------------
proj <- project(vs, nv = nvar, ns = 500)
mcmc_areas(as.matrix(proj)) +
  coord_cartesian(xlim = c(-.6, .6)) +
  labs(
    title="Most relevant variables from cross-validation",
    subtitle = paste("Cross-validation suggests top", nvar, "of variables"))
ggsave("../figures/variable-selection-after-cv.png")


ind <- 1:15 # n most relevant variables
mcmc_areas(as.matrix(fit), 
           pars = paste0("b_", c('Intercept', names(vs$vind[ind])))) +
  coord_cartesian(xlim = c(-.6, .6)) +
  labs(
    title="More variables without the n from cross-validation",
    subtitle = paste("Top", max(ind), "of variables"))
ggsave("../figures/variable-selection.png", height = 10)
