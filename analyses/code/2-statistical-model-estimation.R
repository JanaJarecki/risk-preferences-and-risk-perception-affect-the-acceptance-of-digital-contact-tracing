# ==========================================================================
# File: statistical-model-estimation.R
# Estimates the Bayesian regression models
# Author: Jana B. Jarecki, Rebecca Albrecht
# ==========================================================================
rm(list=ls())
if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, brms, projpred, bayesplot, standardize, mice)
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }


# Setup: which depenent variable to model? ------------------------------------
# dep_var <- "accept_index"
dep_var <- "comply_index"
# dep_var <- "safebehavior_score"
# dep_var <- "policy_score"
# dep_var <- "iwah_diff_score
# dep_var <- "compreh_score"


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


# Model predictor selection --------------------------------------------------
perc_risk_vars <- paste0("perc_risk_", c("health","data","econ"))
seek_risk_vars <- paste0("seek_risk_", c("general","health","data","econ"))
social_vars <- c("honhum_score", "svo_angle", "iwah_diff_score")
indep_vars <- c(perc_risk_vars, seek_risk_vars, social_vars)
indep_vars <- indep_vars[indep_vars!=dep_var]
# Note: using only 'has_work', not 'had_work', because they correlate too much
contr_vars <- c("safebehavior_score", "know_health_score", "know_econ", "female", "age", "education", "community_imputed", "household", "was_infected", "is_infected", "has_symptoms", "income_imputed", "wealth_imputed", "has_work", "income_loss", "homeoffice", "policy_score", "mhealth_score", "tech_score", "compreh_score")
contr_vars <- contr_vars[contr_vars!=dep_var]
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
select_vars <- function(x, use_IV = TRUE) {
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
    if (use_IV == TRUE){
      cvs <- cv_varsel(fit, method = "L1", penalty = penalty)  
    } else {
      cvs <- cv_varsel(fit, method = "L1")  
    }
    
    saveRDS(cvs, fn)
  }

  # vs$vind # variables ordered as they enter during the search  
  nvar <- suggest_size(cvs)
  predictors_selected <- names(cvs$vind[1:nvar])
  for (i in contr_vars) {
    j <- grep(i, predictors_selected)
    predictors_selected[j] <- i
  }

  return(reformulate(
    termlabels = predictors_selected,
    response = dep_var))
}


# Fit reduced model -----------------------------------------------------------
file.remove(paste0("fitted_models/",dep_var,"_variable_selection_",x,".rds"))
file.remove(paste0("fitted_models/", dep_var, "_fit_reduced_all.rds"))
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




# Fit reduced model for other interesting DVs without penalties -----------------------------------------------------------
#file.remove(paste0("fitted_models/", dep_var, "_fit_reduced_all.rds"))
formula <- select_vars("all", use_IV=F)
sobj <- standardize(formula = formula, d)
fit_reduced_all <- update(fit,
                          formula=formula, newdata=sobj$data, prior=prior(normal(0,10), class="b"),
                          file = paste0("fitted_models/", dep_var, "_fit_reduced_all"))


#iwah_diff_score (World -> community) ==>
#                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept              0.20      0.19    -0.16     0.57 1.00    10853    10707
#age                    0.15      0.04     0.07     0.23 1.00    16093    12864
#community_imputed     -0.13      0.03    -0.20    -0.06 1.00    20369    12240
#svo_angle             -0.12      0.03    -0.19    -0.05 1.00    20412    11016
#household              0.10      0.04     0.03     0.17 1.00    18791    12303
#mhealth_score         -0.07      0.04    -0.14    -0.00 1.00    18484    12183
#safebehavior_score    -0.08      0.04    -0.14    -0.01 1.00    20303    12443
#wealth_imputed         0.08      0.04     0.01     0.15 1.00    20020    11617
#seek_risk_econ         0.07      0.04    -0.00     0.14 1.00    22078    12072
#femalefemale          -0.35      0.19    -0.72     0.01 1.00    11036    10214
#femalemale            -0.20      0.19    -0.56     0.17 1.00    10764    10045
#has_workyes            0.13      0.04     0.05     0.22 1.00    18672    11839

#comprehension_score ==> 
#                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept               0.12      0.05     0.02     0.21 1.00    14967    12343
#policy_score            0.40      0.03     0.33     0.46 1.00    14008    11812
#perc_risk_data         -0.14      0.03    -0.21    -0.08 1.00    13960    11579
#seek_risk_data          0.11      0.03     0.05     0.17 1.00    15929    11657
#tech_score              0.10      0.03     0.03     0.16 1.00    15282    12015
#homeofficeno           -0.18      0.05    -0.28    -0.08 1.00    14628    11557
#homeofficepartially     0.00      0.06    -0.12     0.13 1.00    15520    12121


#policy_score ==> 
#                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept             -0.09      0.16    -0.41     0.23 1.00    11304    10478
#compreh_score          0.37      0.03     0.31     0.43 1.00    17108    11085
#perc_risk_data        -0.14      0.03    -0.20    -0.07 1.00    16451    12344
#perc_risk_econ        -0.11      0.03    -0.17    -0.04 1.00    18161    11249
#seek_risk_general     -0.12      0.04    -0.19    -0.04 1.00    15132    12175
#mhealth_score         -0.16      0.03    -0.22    -0.10 1.00    19047    10448
#seek_risk_health      -0.06      0.04    -0.13     0.01 1.00    15037    11930
#safebehavior_score     0.09      0.03     0.03     0.15 1.00    19111    11637
#femalefemale           0.20      0.16    -0.13     0.52 1.00    11382    10639
#femalemale            -0.01      0.16    -0.33     0.31 1.00    11287    10987