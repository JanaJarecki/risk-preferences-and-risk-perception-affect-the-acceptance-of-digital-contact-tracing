if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, dplyr, rstudioapi)

# set working directory to THIS file location if rstudio
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }
source("utilities.R")

# ---------------------------------------------------------------------
# Git does NOT sync the raw data (data protection!), therefore
# export the data and save as `data/raw/pretest.csv" as follows:
#   * format: csv
#   * Use numeric values (not 'Use choice text')
#   * Recode unseen values as -99 and 0
# ---------------------------------------------------------------------


# Load raw data and set column names to lower-case names
d <- setnames(
  fread("../../data/raw/pretest.csv", skip = 2),
  tolower(names(fread("../../data/raw/pretest.csv", nrows = 0))))
# Exclude preview data
d <- d[status != "Survey Preview" & status != 1]
# Columns to drop
drop <- c("status", "ipaddress", "recordeddate", "recipientlastname", "recipientfirstname", "recipientemail", "externalreference", "locationlatitude", "locationlongitude", "distributionchannel", "id", grep("^qid", names(d), value = T))
d <- d[, !drop, with = FALSE]

# Rename variables
setnames(d, c("responseid", "duration (in seconds)"), c("id", "duration_seconds"))
setnames(d, gsub("wealtch", "wealth", names(d)))
setnames(d, c("risk_domain_1", "risk_domain_2", "risk_domain_3", "risk_general_1"), paste0("risk_", c("health", "data", "economy", "general")))
setcolorder(d, "id")



# Exclude participants --------------------------------------------------------
ex <- list(progress = NULL, dataquality = NULL)
ex$progress    <- d[progress < 95]$id
ex$dataquality <- d[data_quality < 4]$id
# @todo in preprocess line 42: add exclusion for failing attention check
# ex$attention <- d[, sum(.SD == c(3, 1, 5) < 2), .SDcols = patterns("^check"), by = id][V1 == FALSE]$id
d <- d[!id %in% unlist(ex)]



# Change polarity of variables ------------------------------------------------
neg <- c("honhum_makemoney", "honhum_celebrity", "honhum_special")
d[, c(neg) := lapply(.SD, function(x) 6-x), .SDcols = neg]
d$understanding_self = 6 - d$understanding_self 
d$attitudes_civilrights = 6 - d$attitudes_civilrights 
# Normalize some variables
normalize <- function(vec) {
  sapply(vec, function(x) (x - min(vec))/(max(vec)-min(x)))
}
d[, normalize(.SD), .SDcols = patterns("^safety_")]
apply(d[, .SD, .SDcols = patterns("^safety_")], 2, normalize)
normalize(d[, .SD, .SDcols = patterns("^safety_")][,2])



# Make dependent variables ----------------------------------------------------
# accept_index & comply_index
# .SDcols = patterns():
#            makes a Sub Dataframe (.SD) from columns matching the pattern
#        :=
#            is like <- but inside a data.table
d[, accept_index := rowMeans(.SD), .SDcols = patterns("^acc_")]
d[, comply_index := rowMeans(.SD), .SDcols = patterns("^com_")]



# Make independent variables --------------------------------------------------
# Form indices/scores from variables
# Social preferences
d[, honhum_score   := rowMeans(.SD), .SDcols = patterns("^honhum_")]
d[, iwah_community := rowMeans(.SD), .SDcols = patterns("^iwah_.*_1")]
d[, iwah_swiss     := rowMeans(.SD), .SDcols = patterns("^iwah_.*_2")]
d[, iwah_world     := rowMeans(.SD), .SDcols = patterns("^iwah_.*_3")]
d[, iwah_diff_score := iwah_community - iwah_world]
d[, svo_angle      := calc_svo_angle(d)] # see utilities.R
# @todo Preprocess, line 72: Make risk perception score from new know data
d[, policy_score   := rowMeans(.SD), .SDcols = patterns("^attitudes_")]
d[, mhealth_score  := rowMeans(.SD), .SDcols = patterns("^mhealth_")]
d[, tech_score     := rowMeans(.SD), .SDcols = patterns("^tech_")]
d[, compreh_score := rowMeans(.SD), .SDcols =patterns("^understanding_")]
# The risk score is just for testing purposes
# @@todo Preprocess: remove risk `score` calculation
d[, risk_score := rowMeans(.SD), .SDcols = patterns("^risk_")]



# Delete columns that were used to create the variables --------------------
d[, grep("^mhealth_|^iwah_|^svo_|^acc_|^com_", colnames(d)[1:100]) := NULL]
d[, grep("(^svo_)(.*)([0-9])", colnames(d)):=NULL]



# Save data ------------------------------------------------------------------
fwrite(d, "../../data/processed/pretest.csv")



# Exclusion report -----------------------------------------------------------
cat("\n---------------------------",
   "Exclusion Report",
   paste(" ", names(ex), sapply(ex, length), sep = "\t"), sep = "\n")


