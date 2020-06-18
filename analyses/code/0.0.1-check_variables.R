if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table)

# set working directory to THIS file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Load preprocessed data
d <- fread("../../data/processed/pretest.csv")

# Risk perception
d[, cor(.SD), ]