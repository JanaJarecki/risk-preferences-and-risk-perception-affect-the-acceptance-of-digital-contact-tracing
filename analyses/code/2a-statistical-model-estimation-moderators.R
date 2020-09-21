# ==========================================================================
# Statistical Modeling
# Author: Rebecca Albrecht
# ==========================================================================
if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, brms, projpred, bayesplot, standardize, mice)
# set working directory to THIS file location (if rstudio)
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }


# Setup: which depenent var -----------------------------------------------
dep_var <- "accept_index"
# dep_var <- "comply_index"


# Load data ---------------------------------------------------------------
d <- fread("../../data/processed/data.csv")

# Preprocess ----------------------------------------------------------------
# Factors
fac <- c("has_work", "income_loss", "is_infected", "was_infected")
d[, (fac) := lapply(.SD, factor, labels=c("no", "yes")), .SDcols = fac]
d[, homeoffice := factor(homeoffice, labels = c("no", "yes", "partially"))]
d[, female := factor(female, labels = c("male", "female", "no response"))]
# Impute missing income and wealth
# d[, summary(.SD), .SDcols=c("income_imputed", "wealth_imputed")] #140,175 NA
d[, income_imputed := fifelse(is.na(income_imputed), median(income_imputed, na.rm=T), income_imputed)]
d[, wealth_imputed := fifelse(is.na(wealth_imputed), median(wealth_imputed,na.rm=T), income_imputed)]
# @todo use 'mic' package to do imputation based on better stats technique

# Model predictor selection --------------------------------------------------
indep_vars <- c("perc_risk_health", "perc_risk_data", "perc_risk_econ", "seek_risk_general", "seek_risk_health", "seek_risk_data", "seek_risk_econ", "honhum_score", "svo_angle", "iwah_community", "iwah_world")
# Note: using only 'has_work', not 'had_work', because they correlate too much
contr_vars <- c("safebehavior_score", "know_health_score", "know_econ", "female", "age", "education", "community_imputed", "household", "was_infected", "is_infected", "has_symptoms", "income_imputed", "wealth_imputed", "has_work", "income_loss", "homeoffice", "policy_score", "mhealth_score", "tech_score", "compreh_score")

# Moderator selection --------------------------------------------------
mod_vars <- c("belief_efficiency", "belief_local")

d <- d[, .SD, .SDcols = c(dep_var, indep_vars, contr_vars, mod_vars)]


### Moderators Variant 1: Fit all models -> Do variable selection.
# Fit full model
formula <- reformulate(
  termlabels = c(indep_vars, contr_vars),
  response = dep_var)
sobj <- standardize(formula = formula, d)
fit_all <- brm(formula = formula, family = gaussian(), data = sobj$data,
               prior = prior_coeff,
               save_all_pars = TRUE, sample_prior = "yes",
               iter = 8000,
               seed = 42,
               control = list(
                 adapt_delta = 0.9 # because horseshoe prior prone to divergent transitions
               ),
               file = paste0("fitted_models/", dep_var, "_fit_full"))

# Fit full model with local moderator
predictors_mod_local <- paste(indep_vars[1:3], mod_vars[2], sep=":")
formula <- reformulate(
  termlabels = c(indep_vars, contr_vars, predictors_mod_local),
  response = dep_var)
sobj <- standardize(formula = formula, d)
fit_all_mod_local <- brm(formula = formula, family = gaussian(), data = sobj$data,
                         prior = prior_coeff,
                         save_all_pars = TRUE, sample_prior = "yes",
                         iter = 8000,
                         seed = 42,
                         control = list(
                           adapt_delta = 0.9 # because horseshoe prior prone to divergent transitions
                         ),
                         file = paste0("fitted_models/", dep_var, "_fit_full_mod_local"))

# Fit full model with effectiveness moderator
predictors_mod_effective <- paste(indep_vars[1:11], mod_vars[1], sep=":")
formula <- reformulate(
  termlabels = c(indep_vars, contr_vars, predictors_mod_effective),
  response = dep_var)
sobj <- standardize(formula = formula, d)
fit_all_mod_effective <- brm(formula = formula, family = gaussian(), data = sobj$data,
                             prior = prior_coeff,
                             save_all_pars = TRUE, sample_prior = "yes",
                             iter = 8000,
                             seed = 42,
                             control = list(
                               adapt_delta = 0.9 # because horseshoe prior prone to divergent transitions
                             ),
                             file = paste0("fitted_models/", dep_var, "_fit_full_mod_effective"))


### Variable selection for moderator comparison
## Done in 2-statistical-model-estimate.R
cvs_full <- readRDS(paste0("fitted_models/", dep_var, "_variable_selection.rds"))

# Model with moderator local
betas <- grep("^b", parnames(fit_all_mod_local), value=TRUE)[-1]
penalty <- rep(1, length(betas))
penalty[match(indep_vars, gsub("b_", "", betas))] <- 0
cvs_local <- cv_varsel(fit_all_mod_local, method = "L1", penalty = penalty)
saveRDS(cvs_local,
        paste0("fitted_models/", dep_var, "_variable_selection_mod_local.rds"))
nvar_local <- suggest_size(cvs_local)

# Model with moderator effectiveness
betas <- grep("^b", parnames(fit_all_mod_effective), value=TRUE)[-1]
penalty <- rep(1, length(betas))
penalty[match(indep_vars, gsub("b_", "", betas))] <- 0
cvs_effective <- cv_varsel(fit_all_mod_effective, method = "L1", penalty = penalty)
saveRDS(cvs_effective,
        paste0("fitted_models/", dep_var, "_variable_selection_mod_effective.rds"))
nvar_effective <- suggest_size(cvs_effective)

### Select model
## Calculate model with suggested variables
# Model with all predictors
cvs_all = readRDS(paste0("fitted_models/", dep_var, "_variable_selection.rds"))
nvar <- suggest_size(cvs_all)
predictors_selected <- names(cvs_all$vind[1:nvar])
formula <- reformulate(
  termlabels = predictors_selected,
  response = dep_var)
sobj <- standardize(formula = formula, d)
fit_reduced_all <- brm(formula = formula, family = gaussian(), data = sobj$data,
                       prior = prior_coeff,
                       save_all_pars = TRUE, sample_prior = "yes",
                       iter = 8000,
                       seed = 42,
                       control = list(
                         adapt_delta = 0.9 # because horseshoe prior prone to divergent transitions
                       ),
                       file = paste0("fitted_models/", dep_var, "_fit_reduced_all"))

# Model with local as a moderator
cvs_local = readRDS(paste0("fitted_models/", dep_var, "_variable_selection_mod_local.rds"))
nvar <- suggest_size(cvs_local)
predictors_selected <- names(cvs_local$vind[1:nvar])
formula <- reformulate(
  termlabels = predictors_selected,
  response = dep_var)
sobj <- standardize(formula = formula, d)
fit_reduced_local <- brm(formula = formula, family = gaussian(), data = sobj$data,
                         prior = prior_coeff,
                         save_all_pars = TRUE, sample_prior = "yes",
                         iter = 8000,
                         seed = 42,
                         control = list(
                           adapt_delta = 0.9 # because horseshoe prior prone to divergent transitions
                         ),
                         file = paste0("fitted_models/", dep_var, "_fit_reduced_local"))


# Model with effectiveness as a moderator
cvs_effective = readRDS(paste0("fitted_models/", dep_var, "_variable_selection_mod_effective.rds"))
nvar <- suggest_size(cvs_effective)
predictors_selected <- names(cvs_effective$vind[1:nvar])
formula <- reformulate(
  termlabels = predictors_selected,
  response = dep_var)
sobj <- standardize(formula = formula, d)
fit_reduced_effective <- brm(formula = formula, family = gaussian(), data = sobj$data,
                             prior = prior_coeff,
                             save_all_pars = TRUE, sample_prior = "yes",
                             iter = 8000,
                             seed = 42,
                             control = list(
                               adapt_delta = 0.9 # because horseshoe prior prone to divergent transitions
                             ),
                             file = paste0("fitted_models/", dep_var, "_fit_reduced_effective"))

BF_all_eff <- bayes_factor(fit_reduced_all, fit_reduced_local)
BF_all_loc <- bayes_factor(fit_reduced_all, fit_reduced_effective)
BF_loc_eff <- bayes_factor(fit_reduced_local, fit_reduced_effective)


### Moderators Variant 2: Fit fill model and perform variable selection -> Reduce model -> Fit reduced moderator models
## All predictors
cvs_all <- readRDS(paste0("fitted_models/", dep_var, "_variable_selection.rds"))
nvar_all <- suggest_size(cvs_all)
proj_all <- project(cvs_all, nv = nvar_all, ns = 1000)
predictors_all <- names(proj_all$vind)

# Moderator Local
predictors_mod <- paste(predictors_all[1:3], mod_vars[2], sep=":")

formula <- reformulate(
  termlabels = c(predictors_all, predictors_mod),
  response = dep_var)
sobj <- standardize(formula = formula, d)

#file.remove(paste0("fitted_models/", dep_var, "_fit_full_mod_local.rds"))
fit_all_mod_local <- brm(formula = formula, family = gaussian(), data = sobj$data,
                         prior = prior_coeff,
                         save_all_pars = TRUE, sample_prior = "yes",
                         iter = 8000,
                         seed = 42,
                         control = list(
                           adapt_delta = 0.9 # because horseshoe prior prone to divergent transitions
                         ),
                         file = paste0("fitted_models/", dep_var, "_fit_full_mod_local"))





# Moderator Effectiveness
predictors_mod <- paste(predictors_all[1:11], mod_vars[1], sep=":")

formula <- reformulate(
  termlabels = c(predictors_all, predictors_mod),
  response = dep_var)
sobj <- standardize(formula = formula, d)

#file.remove(paste0("fitted_models/", dep_var, "_fit_full_mod_effective.rds"))
fit_all_mod_effective <- brm(formula = formula, family = gaussian(), data = sobj$data,
                             prior = prior_coeff,
                             save_all_pars = TRUE, sample_prior = "yes",
                             iter = 8000,
                             seed = 42,
                             control = list(
                               adapt_delta = 0.9 # because horseshoe prior prone to divergent transitions
                             ),
                             file = paste0("fitted_models/", dep_var, "_fit_full_mod_effective"))

# Compare BFs
BF_eff_all <- bayes_factor(fit_reduced_all, fit_all_mod_effective)
BF_all_loc <- bayes_factor(fit_reduced_all, fit_all_mod_local)
BF_eff_loc <- bayes_factor(fit_all_mod_effective, fit_all_mod_local)
