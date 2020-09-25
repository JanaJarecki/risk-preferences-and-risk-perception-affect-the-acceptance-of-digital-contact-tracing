# Setup --------------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table)
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
