if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table)

# ---------------------------------------------------------------------
# Git does NOT sync the raw data (data protection!), therefore
# export the data and save as `data/raw/pretest.csv" as follows:
#   * format: csv
#   * Use numeric values (not 'Use choice text')
#   * Recode unseen values as -99 and 0
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------
# TODO in preprocess, control for subjective data quality
# ---------------------------------------------------------------------



# set working directory to THIS file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load raw data and set column names to lower-case names
d <- setnames(
  fread("../../data/raw/pretest.csv", skip = 2),
  tolower(names(fread("../../data/raw/pretest.csv", nrows = 0))))
# Exclude preview data
d <- d[status != "Survey Preview" & status != 1]
# columns to drop
drop <- c("status", "ipaddress", "recordeddate", "recipientlastname", "recipientfirstname", "recipientemail", "externalreference", "locationlatitude", "locationlongitude", "distributionchannel", "id", grep("^qid", names(d), value = T))
d <- d[, !drop, with = FALSE]
# rename id variable
setnames(d, c("responseid", "duration (in seconds)"), c("id", "duration_seconds"))
setcolorder(d, "id")

# Make dependent variables ----------------------------------------------------
# accept_index & comply_index
d[, accept_index := rowMeans(.SD), .SDcols = grep("^acc_", colnames(d))]
d[, comply_index := rowMeans(.SD), .SDcols = grep("^com_", colnames(d))]


# Make independent variables --------------------------------------------------
# mhealth_index
d[, mhealth_index := rowMeans(.SD), .SDcols = grep("^mhealth_", colnames(d))]
d[, iwah_community := rowMeans(.SD), .SDcols = grep("^iwah_.*_1", colnames(d))]
d[, iwah_swiss := rowMeans(.SD), .SDcols = grep("^iwah_.*_2", colnames(d))]
d[, iwah_world := rowMeans(.SD), .SDcols = grep("^iwah_.*_3", colnames(d))]
d[, svo_index := rowMeans(.SD), .SDcols = grep("^svo_", colnames(d))]

# Delete the columns that were used to create the variables ------------------
drop <- grep("^mhealth_|^iwah_|^svo_|^acc_|^com_", colnames(d)[1:100], value=T)
d <- d[, !drop, with = FALSE]

# Save data
fwrite(d, "../../data/processed/pretest.csv")