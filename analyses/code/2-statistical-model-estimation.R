# ==========================================================================
# Statistical Modeling
# Author: Jana B. Jarecki
# ==========================================================================
rm(list=ls())
if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, brms, projpred, bayesplot, standardize, mice)
# set working directory to THIS file location (if rstudio)
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }


# Setup: which depenent var -----------------------------------------------
dep_var <- "accept_index"
dep_var <- "comply_index"


# Load data ---------------------------------------------------------------
d <- fread("../../data/processed/data.csv")

# Preprocess ----------------------------------------------------------------
# IWAH difference score
d[, iwah_diff_score := iwah_community - iwah_world]
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
perc_risk_vars <- paste0("perc_risk_", c("health","data","econ"))
seek_risk_vars <- paste0("seek_risk_", c("general","health","data","econ"))
social_vars <- c("honhum_score", "svo_angle", "iwah_diff_score")
indep_vars <- c(perc_risk_vars, seek_risk_vars, social_vars)
# Note: using only 'has_work', not 'had_work', because they correlate too much
contr_vars <- c("safebehavior_score", "know_health_score", "know_econ", "female", "age", "education", "community_imputed", "household", "was_infected", "is_infected", "has_symptoms", "income_imputed", "wealth_imputed", "has_work", "income_loss", "homeoffice", "policy_score", "mhealth_score", "tech_score", "compreh_score")
d <- d[, .SD, .SDcols = c(dep_var, indep_vars, contr_vars)]


# Standardize variables -------------------------------------------------------
formula <- reformulate(
  termlabels = c(indep_vars, contr_vars),
  response = dep_var)
sobj <- standardize(formula = formula, d)
# Important:
# 'sobj$data' must be used as data from here on


# Variable selection procedure -----------------------------------------------
# using leave-one-out cross-validation and Lasso (L1) penalization
# 1. Setup
n <- nrow(sobj$data) # 757
nc <- ncol(sobj$data) # 31
# Piironen and Vehtari (2017): the prior for the global shrinkage parameter is defined from the prior guess for the number of variables that matter
p0 <- length(indep_vars) # prior guess: number of relevant variables
tau0 <- p0/(nc-p0) * 1/sqrt(n) # scale for tau (stan_glm scales this by sigma)
prior_coeff <- set_prior(horseshoe(scale_global = tau0, scale_slab = 1)) # regularized horseshoe prior

# 2. Fit full model
# If you want to re-fit the model, run next line
# file.remove(paste0("fitted_models/", dep_var, "_fit_full.rds"))
formula <- reformulate(
  termlabels = c(indep_vars, contr_vars),
  response = dep_var)
sobj <- standardize(formula = formula, d)
fit <- brm(formula = formula, family = gaussian(), data = sobj$data,
  prior = prior_coeff,
  save_all_pars = TRUE, sample_prior = "yes",
  iter = 8000,
  seed = 42,
  control = list(
    adapt_delta = 0.99 # because horseshoe prior prone to divergent transitions
  ),
  file = paste0("fitted_models/", dep_var, "_fit_full"))

# Variable selection ----------------------------------------------------------
# Function that does variable selection given specification in x
select_vars <- function(x) {
  if (x == "all") indep_vars <- indep_vars
  if (x == "no_social") indep_vars <- c(perc_risk_vars, seek_risk_vars)
  if (x == "no_riskperc") indep_vars <- c(seek_risk_vars, social_vars)
  if (x == "no_riskseek") indep_vars <- c(perc_risk_vars, social_vars)

  # Penalty of 0 = var is selected first, Inf = var is never selected
  #   * 0 for independent vars that are theoretically motivated
  #   * 1 for control vars
  betas <- grep("^b", parnames(fit), value=TRUE)[-1]
  penalty <- rep(1, length(betas))
  penalty[match(c(indep_vars), gsub("b_", "", betas))] <- 0
  # betas[penalty==0] # check var penalty: ok

  fn <- paste0("fitted_models/",dep_var,"_variable_selection_",x,".rds")
  if (file.exists(fn)) {
    cvs <- readRDS(fn) 
  } else {
    cvs <- cv_varsel(fit, method = "L1", penalty = penalty)
    saveRDS(cvs, fn)
  }

  # vs$vind # variables ordered as they enter during the search  
  nvar <- suggest_size(cvs)
  predictors_selected <- names(cvs$vind[1:nvar])

  return(reformulate(
    termlabels = predictors_selected,
    response = dep_var))
}


# Fit reduced model -----------------------------------------------------------
#file.remove(paste0("fitted_models/", dep_var, "_fit_reduced_all.rds"))
formula <- select_vars("all")
sobj <- standardize(formula = formula, d)
fit_reduced_all <- update(fit,
  formula=formula, newdata=sobj$data, prior=prior(normal(0,10), class="b"),
  file = paste0("fitted_models/", dep_var, "_fit_reduced_all"))


# Variable selection no social ------------------------------------------------
#file.remove(paste0("fitted_models/", dep_var, "_fit_reduced_no_social.rds"))
formula <- select_vars("no_social")
sobj <- standardize(formula = formula, d)
fit_reduced_no_social <- update(fit,
  formula=formula, newdata=sobj$data, prior=prior(normal(0,10), class="b"),
  file = paste0("fitted_models/", dep_var, "_fit_reduced_no_social"))

# Variable selection no risk seek ---------------------------------------------
#file.remove(paste0("fitted_models/", dep_var, "_fit_reduced_no_riskseek.rds"))
formula <- select_vars("no_riskseek")
sobj <- standardize(formula = formula, d)
fit_reduced_no_riskseek <- update(fit,
  formula=formula, newdata=sobj$data, prior=prior(normal(0,10), class="b"),
  file = paste0("fitted_models/", dep_var, "_fit_reduced_no_riskseek"))


# Variable selection no risk perception --------------------------------------
#file.remove(paste0("fitted_models/", dep_var, "_fit_reduced_no_riskperc.rds"))
formula <- select_vars("no_riskperc")
sobj <- standardize(formula = formula, d)
fit_reduced_no_riskperc <- update(fit,
  formula=formula, newdata=sobj$data, prior=prior(normal(0,10), class=b),
  file = paste0("fitted_models/", dep_var, "_fit_reduced_no_riskperc"))


# Compare models -------------------------------------------------------------
BF_no_social_all = bayes_factor(fit_reduced_no_social, fit_reduced_all)
BF_no_riskseek_all = bayes_factor(fit_reduced_no_riskseek, fit_reduced_all)
BF_no_riskperc_all = bayes_factor(fit_reduced_no_riskperc, fit_reduced_all)
BF_no_social_no_riskseek = bayes_factor(fit_reduced_no_social, fit_reduced_no_riskseek)
BF_no_social_no_riskperc = bayes_factor(fit_reduced_no_social, fit_reduced_no_riskperc)


