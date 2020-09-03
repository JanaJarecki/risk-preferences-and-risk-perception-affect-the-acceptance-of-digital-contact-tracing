# ==========================================================================
# Statistical Modeling
# Author: Jana B. Jarecki
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


# Factors -------------------------------------------------------------------
fac <- c("has_work", "income_loss", "is_infected", "was_infected")
d[, (fac) := lapply(.SD, factor, labels=c("no", "yes")), .SDcols = fac]
d[, homeoffice := factor(homeoffice, labels = c("no", "yes", "partially"))]
d[, female := factor(female, labels = c("male", "female", "no response"))]


# Model predictor selection --------------------------------------------------
indep_vars <- c("perc_risk_health", "perc_risk_data", "perc_risk_econ", "seek_risk_general", "seek_risk_health", "seek_risk_data", "seek_risk_econ", "honhum_score", "svo_angle", "iwah_community", "iwah_world")
# Note: using only 'has_work', not 'had_work', because they correlate too much
contr_vars <- c("safebehavior_score", "know_health_score", "know_econ", "female", "age", "education", "community_imputed", "household", "was_infected", "is_infected", "has_symptoms", "income_imputed", "wealth_imputed", "has_work", "income_loss", "homeoffice", "policy_score", "mhealth_score", "tech_score", "compreh_score")
d <- d[, .SD, .SDcols = c(dep_var, indep_vars, contr_vars)]


# Impute missing income and wealth variables ----------------------------------
# d[, summary(.SD), .SDcols=c("income_imputed", "wealth_imputed")] #140,175 NA
d[, income_imputed := fifelse(is.na(income_imputed), median(income_imputed, na.rm=T), income_imputed)]
d[, wealth_imputed := fifelse(is.na(wealth_imputed), median(wealth_imputed,na.rm=T), income_imputed)]
# @todo use 'mic' package to do imputation based on better stats technique


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
nc <- ncol(sobj$data) # 33
# Piironen and Vehtari (2017): the prior for the global shrinkage parameter is defined from the prior guess for the number of variables that matter
p0 <- length(indep_vars) # prior guess: number of relevant variables
tau0 <- p0/(nc-p0) * 1/sqrt(n) # scale for tau (stan_glm scales this by sigma)
prior_coeff <- set_prior(horseshoe(scale_global = tau0, scale_slab = 1)) # regularized horseshoe prior


# 2. Fit full model
# If you want to re-fit the model, run next line
# file.remove(paste0("fitted_models/", dep_var, "_fit_full.rds"))
fit <- brm(formula = formula, family = gaussian(), data = sobj$data,
  prior = prior_coeff,
  save_all_pars = TRUE, sample_prior = "yes",
  iter = 8000,
  seed = 42,
  control = list(
    adapt_delta = 0.9 # because horseshoe prior prone to divergent transitions
  ),
  file = paste0("fitted_models/", dep_var, "_fit_full"))
# @todo update model fitting because of errors!
# @body
# Warning messages:     
# 1: In system(paste(CXX, ARGS), ignore.stdout = TRUE, ignore.stderr = TRUE):
#   'C:/rtools40/usr/mingw_/bin/g++' not found
# 2: There were 101 divergent transitions after warmup. See                                               
# http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# to find out why this is a problem and how to eliminate them.                                            
# 3: Examine the pairs() plot to diagnose sampling problems


# 3.a) Perform variable selection on training data
# Penalty of 0 = var is selected first, Inf = var is never selected
#   * 0 for independent vars that are theoretically motivated
#   * 1 for control vars
betas <- grep("^b", parnames(fit), value=TRUE)[-1]
penalty <- rep(1, length(betas))
penalty[match(indep_vars, gsub("b_", "", betas))] <- 0
# betas[penalty==0] # check var penalty: ok

# Optional
# Forward search & Lasso L1-penalty to find variable order (Tran et al., 2012)
# vs <- varsel(fit,
#   method = "L1",
#   penalty = penalty)
# vs$vind # variables ordered as they enter during the search


# 3.b) Perform variable selection using cross-validation
cvs <- cv_varsel(fit,
  method = "L1",
  penalty = penalty)
saveRDS(cvs, paste0("fitted_models/", dep_var, "_variable_selection.rds"))

# model size suggested by the program
nvar <- suggest_size(cvs)









# ==========================================================================
# Old code starts here
# ==========================================================================

# # Regression, dependent variable: acceptance_index ----------------------------
# priors <- prior_string("normal(0,10)")

# fit_acc_all_main <- brm(formula = ,
#                         data = d, 
#                         prior = priors, 
#                         save_all_pars=T)

# fit_acc_all_mod <- brm(formula = accept_index ~ 
#                           #IV 
#                           perc_health +
#                           perc_data +
#                           perc_moneyloss +
#                           risk_general + 
#                           risk_domain_health + 
#                           risk_domain_data + 
#                           risk_domain_economic + 
#                           iwah_community +
#                           iwah_world +
#                           svo_angle +
#                           honhum +
#                           perc_health:belief_efficiency +
#                           perc_data:belief_efficiency +
#                           perc_moneyloss:belief_efficiency +
#                           risk_general:belief_efficiency + 
#                           risk_domain_health:belief_efficiency + 
#                           risk_domain_data:belief_efficiency + 
#                           risk_domain_economic:belief_efficiency + 
#                           iwah_community:belief_efficiency +
#                           iwah_world:belief_efficiency +
#                           svo_angle:belief_efficiency +
#                           honhum:belief_efficiency +
#                           #Mod
#                           belief_efficiency +
#                           belief_local +
#                           belief_global +
#                           #Control
#                           know_health +
#                           know_econ +
#                           female +
#                           age +
#                           education +
#                           household +
#                           household_kids +
#                           was_infected +
#                           is_infected +
#                           has_symptoms +
#                           had_work +
#                           has_work +
#                           income_loss +
#                           policy +
#                           mhealth +
#                           tech +
#                           comprehension,
#                         data = d, 
#                         prior = priors, 
#                         save_all_pars=T)

# bayes_factor(fit_acc_all_main, fit_acc_all_mod)

# hdi(fit_acc_all_main)

# fit_com_all_main <- brm(formula = comply_index ~ 
#                          #IV 
#                          perc_health +
#                          perc_data +
#                          perc_moneyloss +
#                          risk_general + 
#                          risk_domain_health + 
#                          risk_domain_data + 
#                          risk_domain_economic + 
#                          iwah_community +
#                          iwah_world +
#                          svo_angle +
#                          honhum +
#                          #Mod
#                          belief_efficiency +
#                          belief_local +
#                          belief_global +
#                          #Control
#                          know_health +
#                          know_econ +
#                          female +
#                          age +
#                          education +
#                          household +
#                          household_kids +
#                          was_infected +
#                          is_infected +
#                          has_symptoms +
#                          had_work +
#                          has_work +
#                          income_loss +
#                          policy +
#                          mhealth +
#                          tech +
#                          comprehension,
#                        data = d, 
#                        prior = priors, 
#                        save_all_pars=T)

# fit_vacc_all_main <- brm(formula = vaccine_index ~ 
#                          #IV 
#                          perc_health +
#                          perc_data +
#                          perc_moneyloss +
#                          risk_general + 
#                          risk_domain_health + 
#                          risk_domain_data + 
#                          risk_domain_economic + 
#                          iwah_community +
#                          iwah_world +
#                          svo_angle +
#                          honhum +
#                          #Mod
#                          belief_efficiency +
#                          belief_local +
#                          belief_global +
#                          #Control
#                          know_health +
#                          know_econ +
#                          female +
#                          age +
#                          education +
#                          household +
#                          household_kids +
#                          was_infected +
#                          is_infected +
#                          has_symptoms +
#                          had_work +
#                          has_work +
#                          income_loss +
#                          policy +
#                          mhealth +
#                          tech +
#                          comprehension,
#                        data = d, 
#                        prior = priors, 
#                        save_all_pars=T)

# library(magrittr)
# library(dplyr)
# library(purrr)
# library(forcats)
# library(tidyr)
# library(modelr)
# library(ggdist)
# library(tidybayes)
# library(ggplot2)
# library(cowplot)
# library(rstan)
# library(brms)
# library(ggrepel)
# library(RColorBrewer)
# library(gganimate)

# get_variables(fit_acc_all_main)


# #bayes_factor(fit_acc, fit_acc_eff_mod)
# #fit2 <- update(fit, . ~ . - know_econ)
# #summary(fit2)


