if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, dplyr)

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
drop <- c("status", "ipaddress", "recordeddate", "recipientlastname", "recipientfirstname", "recipientemail", "externalreference", "locationlatitude", "locationlongitude", "distributionchannel", "id", grep("^qid", names(d), value = T), "know_infected_last14_4", "know_infected_last14_5", "know_infected_last14_6", "know_infected_last14_7")
d <- d[, !drop, with = FALSE]
# rename id variable
setnames(d, c("responseid", "duration (in seconds)"), c("id", "duration_seconds"))
setcolorder(d, "id")
# spelling error in wealth variable names
setnames(d, gsub("wealtch", "wealth", names(d)))

# Make dependent variables ----------------------------------------------------
# accept_index & comply_index
# .SDcols = patterns():
#            makes a Sub Dataframe (.SD) from columns matching the pattern
#        :=
#            is like <- but inside a data.table
d[, accept_index := rowMeans(.SD), .SDcols = patterns("^acc_")]
d[, comply_index := rowMeans(.SD), .SDcols = patterns("^com_")]

# Make independent variables --------------------------------------------------
# mhealth_index
d[, mhealth_index := rowMeans(.SD), .SDcols = patterns("^mhealth_")]
d[, iwah_community := rowMeans(.SD), .SDcols = patterns("^iwah_.*_1")]
d[, iwah_swiss := rowMeans(.SD), .SDcols = patterns("^iwah_.*_2")]
d[, iwah_world := rowMeans(.SD), .SDcols = patterns("^iwah_.*_3")]

# polarity
neg <- c("honhum_makemoney", "honhum_celebrity", "honhum_special")
d[, c(neg) := lapply(.SD, function(x) 6-x), .SDcols = neg ]

#using case_when because fcase not on CRAN yet
d <- d %>% 
  mutate(svo_kept_1 = 85,
         svo_given_1 = case_when(svo_1 == 1 ~ 85,
                                 svo_1 == 2 ~ 76,
                                 svo_1 == 3 ~ 68,
                                 svo_1 == 4 ~ 59,
                                 svo_1 == 5 ~ 50,
                                 svo_1 == 6 ~ 41,
                                 svo_1 == 7 ~ 33,
                                 svo_1 == 8 ~ 24,
                                 svo_1 == 9 ~ 15),
         svo_kept_2 =  case_when(svo_2 == 1 ~ 85,
                                 svo_2 == 2 ~ 87,
                                 svo_2 == 3 ~ 89,
                                 svo_2 == 4 ~ 91,
                                 svo_2 == 5 ~ 93,
                                 svo_2 == 6 ~ 94,
                                 svo_2 == 7 ~ 96,
                                 svo_2 == 8 ~ 98,
                                 svo_2 == 9 ~ 100),
         svo_given_2 = case_when(svo_2 == 1 ~ 15,
                                 svo_2 == 2 ~ 19,
                                 svo_2 == 3 ~ 24,
                                 svo_2 == 4 ~ 28,
                                 svo_2 == 5 ~ 33,
                                 svo_2 == 6 ~ 37,
                                 svo_2 == 7 ~ 41,
                                 svo_2 == 8 ~ 46,
                                 svo_2 == 9 ~ 50),
         svo_kept_3 =  case_when(svo_3 == 1 ~ 50,
                                 svo_3 == 2 ~ 54,
                                 svo_3 == 3 ~ 59,
                                 svo_3 == 4 ~ 63,
                                 svo_3 == 5 ~ 68,
                                 svo_3 == 6 ~ 72,
                                 svo_3 == 7 ~ 76,
                                 svo_3 == 8 ~ 81,
                                 svo_3 == 9 ~ 85),
         svo_given_3 = case_when(svo_3 == 1 ~ 100,
                                 svo_3 == 2 ~ 98,
                                 svo_3 == 3 ~ 96,
                                 svo_3 == 4 ~ 94,
                                 svo_3 == 5 ~ 93,
                                 svo_3 == 6 ~ 91,
                                 svo_3 == 7 ~ 89,
                                 svo_3 == 8 ~ 87,
                                 svo_3 == 9 ~ 85),
         svo_kept_4 =  case_when(svo_4 == 1 ~ 50,
                                 svo_4 == 2 ~ 54,
                                 svo_4 == 3 ~ 59,
                                 svo_4 == 4 ~ 63,
                                 svo_4 == 5 ~ 68,
                                 svo_4 == 6 ~ 72,
                                 svo_4 == 7 ~ 76,
                                 svo_4 == 8 ~ 81,
                                 svo_4 == 9 ~ 85),
         svo_given_4 = case_when(svo_4 == 1 ~ 100,
                                 svo_4 == 2 ~ 89,
                                 svo_4 == 3 ~ 79,
                                 svo_4 == 4 ~ 68,
                                 svo_4 == 5 ~ 58,
                                 svo_4 == 6 ~ 47,
                                 svo_4 == 7 ~ 36,
                                 svo_4 == 8 ~ 26,
                                 svo_4 == 9 ~ 15),
         svo_kept_5 =  case_when(svo_5 == 1 ~ 100,
                                 svo_5 == 2 ~ 94,
                                 svo_5 == 3 ~ 88,
                                 svo_5 == 4 ~ 81,
                                 svo_5 == 5 ~ 75,
                                 svo_5 == 6 ~ 69,
                                 svo_5 == 7 ~ 63,
                                 svo_5 == 8 ~ 56,
                                 svo_5 == 9 ~ 50),
         svo_given_5 = case_when(svo_5 == 1 ~ 50,
                                 svo_5 == 2 ~ 56,
                                 svo_5 == 3 ~ 63,
                                 svo_5 == 4 ~ 69,
                                 svo_5 == 5 ~ 75,
                                 svo_5 == 6 ~ 81,
                                 svo_5 == 7 ~ 88,
                                 svo_5 == 8 ~ 94,
                                 svo_5 == 9 ~ 100),
         svo_kept_6 =  case_when(svo_6 == 1 ~ 100,
                                 svo_6 == 2 ~ 98,
                                 svo_6 == 3 ~ 96,
                                 svo_6 == 4 ~ 94,
                                 svo_6 == 5 ~ 93,
                                 svo_6 == 6 ~ 91,
                                 svo_6 == 7 ~ 89,
                                 svo_6 == 8 ~ 87,
                                 svo_6 == 9 ~ 85),
         svo_given_6 = case_when(svo_6 == 1 ~ 50,
                                 svo_6 == 2 ~ 54,
                                 svo_6 == 3 ~ 59,
                                 svo_6 == 4 ~ 63,
                                 svo_6 == 5 ~ 68,
                                 svo_6 == 6 ~ 72,
                                 svo_6 == 7 ~ 76,
                                 svo_6 == 8 ~ 81,
                                 svo_6 == 9 ~ 85))

d[, svo_kept := rowMeans(.SD) - 50, .SDcols = grep("svo_kept", colnames(d))]
d[, svo_given := rowMeans(.SD) - 50, .SDcols = grep("svo_given", colnames(d))]
d[, svo_angle := atan(svo_given/svo_kept) * 180 / pi]

d$understanding_self = 6-d$understanding_self 
d[, understanding_correct := rowMeans(.SD), .SDcols = patterns("^understanding_")]

# Delete the columns that were used to create the variables ------------------
d[, grep("^mhealth_|^iwah_|^svo_|^acc_|^com_", colnames(d)[1:100]):=NULL]
d[, grep("(^svo_)(.*)([0-9])", colnames(d)):=NULL]


# Save data
fwrite(d, "../../data/processed/pretest.csv")
