# Setup --------------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
# Load packages and install missing packages in one step
pacman::p_load(data.table)
# set working directory to THIS file location if rstudio
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }


# Load data ---------------------------------------------------------------
d <- fread("../../data/processed/data.csv")
d$excl <- ""
n_excluded <- 91  # see exclusion report in pre-process

d$age <- as.numeric(d$age)
d$gender <- factor(d$female, levels = c(0,1,2), labels = c("male", "female", "NA"))

cognitiveutils::participants(d,
  id="id",
  age="age",
  gender="gender",
  excl=91,
  date = "enddate",
  recruitedfrom ="professional panel provider",
  approvedby = "the ethics committee of the faculty of Psychology at the University of Basel")
