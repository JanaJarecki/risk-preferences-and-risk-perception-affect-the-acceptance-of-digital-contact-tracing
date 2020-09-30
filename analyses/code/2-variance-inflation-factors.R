# ==========================================================================
# Statistical Modeling
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(data.table, standardize, car)
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }


# Setup: which depenent var -----------------------------------------------
dep_var <- "accept_index"
# dep_var <- "comply_index"


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
indep_vars <- c("perc_risk_health", "perc_risk_data", "perc_risk_econ", "seek_risk_general", "seek_risk_health", "seek_risk_data", "seek_risk_econ", "honhum_score", "svo_angle", "iwah_diff_score")
# Note: using only 'has_work', not 'had_work', because they correlate too much
contr_vars <- c("safebehavior_score", "know_health_score", "know_econ", "female", "age", "education", "community_imputed", "household", "was_infected", "is_infected", "has_symptoms", "income_imputed", "wealth_imputed", "has_work", "income_loss", "homeoffice", "policy_score", "mhealth_score", "tech_score", "compreh_score")
d <- d[, .SD, .SDcols = c(dep_var, indep_vars, contr_vars)]

# Frequentistic model
fit <- lm(formula = formula, data = sobj$data)
vif(fit)