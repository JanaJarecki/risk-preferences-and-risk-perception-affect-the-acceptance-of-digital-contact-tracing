# Setup --------------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, psych)
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }


# Load data ---------------------------------------------------------------
d <- fread("../../data/processed/data.csv")
d$excl <- ""
n_excluded <- 91  # see exclusion report in pre-process

d$age <- as.numeric(d$age)
d$gender <- factor(d$female, levels = c(0,1,2), labels = c("male", "female", "NA"))

# Print demographics ---------------------------------------------------------
cognitiveutils::participants(d,
  id="id",
  age="age",
  gender="gender",
  excl=91,
  date = "enddate",
  recruitedfrom ="professional panel provider",
  approvedby = "the ethics committee of the faculty of Psychology at the University of Basel")

# Summarize demographics ------------------------------------------------------
d[, describe(age)]
d[, describe(belief_efficiency)]
d[, prop.table(table(is_infected))]
d[, prop.table(table(was_infected))]
d[is_infected==0 & was_infected==0, any(has_symptoms_fever, has_symptoms_feverfeel, has_symptoms_sorethroat, has_symptoms_drycough, has_symptoms_shortbreath, has_symptoms_muscleache, has_symptoms_nosense), by=id][, prop.table(table(V1))]
d[, print(prop.table(table(homeoffice)), digits=1)]
d[, print(prop.table(table(income_loss)), digits=2)]
d[, income_imputed := fifelse(is.na(income_imputed), median(income_imputed, na.rm=T), income_imputed)]
d[, wealth_imputed := fifelse(is.na(wealth_imputed), median(wealth_imputed,na.rm=T), income_imputed)]
d[, print(prop.table(table(income_loss, cut(income_imputed, breaks = quantile(income_imputed)))), digits=1)]

d[, range(as.IDate(startdate))]

d[, describe(accept_index)]
d[, describe(comply_index)]
d[, describe(accept_index), by=cut(age,6,ordered_result=T)][order(cut)]
d[, describe(policy_score), by=cut(age,6,ordered_result=T)][order(cut)]
d[, describe(policy_score), by=gender]

grep("ef", names(d), value=T)

# Table demographics ----------------------------------------------------------
digits <- 2
d[, female_n := ifelse(female==2, NA, female)]
d[, high_school := education >= 4]
tab <- d[, .(
  Variable = c("Female", "Age", "At least high-school degree", "Household size", "Number of children", "Monthly net income (sFr)"),
  Mdn = sapply(.SD, median, na.rm=T),
  Mean = sapply(.SD, mean, na.rm=T),
  SD = sapply(.SD, sd, na.rm=T),
  Nonresponse = sapply(.SD, function(x) sum(is.na(x)))
  ),
  .SDcols = c("female_n", "age", "high_school", "household", "household_kids", "income_imputed")]

knitr::kable(tab,
  format = "latex",
  booktabs = TRUE,
  digits = c(2, 2, 2, 0))
