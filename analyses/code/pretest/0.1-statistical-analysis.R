if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, brms)

# set working directory to THIS file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Load preprocessed data
d <- fread("../../data/processed/pretest.csv")

# Inter-correlations of regressors --------------------------------------------
# Risk-related variables:
d[, cor(.SD), .SDcols = grep("^risk", names(d))]
d[, cor(.SD), .SDcols = grep("^iwah", names(d))]

# Regression, dependent variable: acceptance_index ----------------------------
priors <- priors(b = "normal(0,10)")
fit <- brm(formula = accept_index ~ risk_general_1 + risk_domain_1 + risk_domain_2 + risk_domain_3 + know_data + know_econ + iwah_community + iwah_world + svo_index,
  data = d)

fit2 <- update(fit, . ~ . - know_econ)
summary(fit2)



