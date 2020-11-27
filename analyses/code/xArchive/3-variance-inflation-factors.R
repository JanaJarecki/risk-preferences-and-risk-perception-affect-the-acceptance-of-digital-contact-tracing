# ==========================================================================
# Statistical Modeling
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(data.table, standardize, car, projpred)
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
cv <- readRDS(paste0("fitted_models/", dep_var, "_variable_selection.rds"))
var <- names(cv$vind)[1:suggest_size(cv)]
d <- d[, .SD, .SDcols = c(dep_var, var)]

# Standardize variables -------------------------------------------------------
formula <- reformulate(
  termlabels = var,
  response = dep_var)
sobj <- standardize(formula = formula, d)
# Important:
# 'sobj$data' must be used as data from here on

# VIF -------------------------------------------------------------------------
fit <- lm(formula = formula, data = sobj$data)
vif(fit)