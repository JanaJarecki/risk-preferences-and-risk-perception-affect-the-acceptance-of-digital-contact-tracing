# ==========================================================================
# Preprocessing of the main BCRQ study
# ==========================================================================
if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, dplyr, rstudioapi)

# set working directory to THIS file location if rstudio
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }
source("utilities.R")

# ---------------------------------------------------------------------
# Git does NOT sync the raw data (data protection!), therefore
# export the data and save as `data/raw/data.csv" as follows:
#   * format: csv
#   * Use numeric values (not 'Use choice text')
#   * Recode unseen values as -99 and 0
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------
# TODO in preprocess, control for subjective data quality
# ---------------------------------------------------------------------


# Load raw data and set column names to lower-case names
d <- setnames(
  fread("../../data/raw/data.csv", skip = 2),
  tolower(names(fread("../../data/raw/data.csv", nrows = 0))))
# Exclude preview data
d <- d[status != "Survey Preview" & status != 1]
# columns to drop
drop <- c("status", "ipaddress", "recordeddate", "recipientlastname", "recipientfirstname", "recipientemail", "externalreference", "locationlatitude", "locationlongitude", "distributionchannel", "id", grep("^qid", names(d), value = T))
d <- d[, !drop, with = FALSE]
# rename id variable
setnames(d, c("responseid", "duration (in seconds)"), c("id", "duration_seconds"))
setcolorder(d, "id")
# spelling error in wealth variable names
setnames(d, gsub("wealtch", "wealth", names(d)))

#remove poeple who did not finish
unfinished=sum(d$finished==0)
d=d[finished==1,]
nrow(d)
# Make dependent variables ----------------------------------------------------
# accept_index & comply_index ( & vacc_index)
# .SDcols = patterns():
#            makes a Sub Dataframe (.SD) from columns matching the pattern
#        :=
#            is like <- but inside a data.table
d[, accept_index := rowMeans(.SD), .SDcols = patterns("^acc_")]
d[, comply_index := rowMeans(.SD), .SDcols = patterns("^com_")]
d[, vaccine_index := rowMeans(.SD), .SDcols = patterns("^vacc_")]




# Make/rename independent variables --------------------------------------------------
## Risk perception
# Recode perc_severe
recode_dict=data.table(old=c(11:20), new=c(2,8,13,18,23,28,33,38,43,46))
d$perc_severe = sapply(d$perc_severe, function(x){recode_dict[old==x, new]})
# Health

d[, perc__health := rowMeans(.SD), .SDcols = patterns("^perc_infected|perc_severe")]
# Recode perc_data
d$perc__data = sapply(d$perc_data, function(x){recode_dict[old==x, new]})
# Recode perc_moneyloss
d$perc__moneyloss = sapply(d$perc_moneyloss, function(x){recode_dict[x==(old-10), new]})
## Risk preference
setnames(d, gsub("risk_general_1", "risk_general", names(d)))
setnames(d, gsub("risk_domain_1", "risk_domain_health", names(d)))
setnames(d, gsub("risk_domain_2", "risk_domain_data", names(d)))
setnames(d, gsub("risk_domain_3", "risk_domain_economic", names(d)))
## Social preference
# IWAH
d[, iwah__community := rowMeans(.SD), .SDcols = patterns("^iwah_.*_1")]
d[, iwah__swiss := rowMeans(.SD), .SDcols = patterns("^iwah_.*_2")]
d[, iwah__world := rowMeans(.SD), .SDcols = patterns("^iwah_.*_3")]
#Delete participants who did not complete IWAH
x=apply(d[, .SD, .SDcols = patterns("^iwah_")], 1, min)
d=d[-which(x < 0)]
d$iwah__diff = d$iwah__community - d$iwah__world

# honhum
neg <- c("honhum_makemoney", "honhum_celebrity", "honhum_special") 
d[, c(neg) := lapply(.SD, function(x) 6-x), .SDcols = neg ]
d[, honhum := rowMeans(.SD), .SDcols = patterns("^honhum_")]
# SVO calculation (using case_when because fcase not on CRAN yet)
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
                                 svo_5 == 2 ~ 89,
                                 svo_5 == 3 ~ 79,
                                 svo_5 == 4 ~ 68,
                                 svo_5 == 5 ~ 58,
                                 svo_5 == 6 ~ 47,
                                 svo_5 == 7 ~ 36,
                                 svo_5 == 8 ~ 26,
                                 svo_5 == 9 ~ 15),
         svo_given_5 = case_when(svo_5 == 1 ~ 50,
                                 svo_5 == 2 ~ 54,
                                 svo_5 == 3 ~ 59,
                                 svo_5 == 4 ~ 63,
                                 svo_5 == 5 ~ 68,
                                 svo_5 == 6 ~ 72,
                                 svo_5 == 7 ~ 76,
                                 svo_5 == 8 ~ 81,
                                 svo_5 == 9 ~ 85),
         svo_kept_6 =  case_when(svo_6 == 1 ~ 20,
                                 svo_6 == 2 ~ 25,
                                 svo_6 == 3 ~ 30,
                                 svo_6 == 4 ~ 35,
                                 svo_6 == 5 ~ 40,
                                 svo_6 == 6 ~ 45,
                                 svo_6 == 7 ~ 50,
                                 svo_6 == 8 ~ 55,
                                 svo_6 == 9 ~ 60),
         svo_given_6 = case_when(svo_6 == 1 ~ 70,
                                 svo_6 == 2 ~ 65,
                                 svo_6 == 3 ~ 60,
                                 svo_6 == 4 ~ 55,
                                 svo_6 == 5 ~ 50,
                                 svo_6 == 6 ~ 45,
                                 svo_6 == 7 ~ 40,
                                 svo_6 == 8 ~ 35,
                                 svo_6 == 9 ~ 30))
d[, svo_kept := rowMeans(.SD) - 50, .SDcols = grep("svo_kept", colnames(d))]
d[, svo_given := rowMeans(.SD) - 50, .SDcols = grep("svo_given", colnames(d))]
d[, svo__angle := atan(svo_given/svo_kept) * 180 / pi]

# Moderators --------------------------------------------------
d[, belief__efficiency := rowMeans(.SD), .SDcols = patterns("belief_efficiency|belief_5")]
d[, belief__local := rowMeans(.SD), .SDcols = patterns("belief_local")]
d[, belief__global := rowMeans(.SD), .SDcols = patterns("belief_global")]

# Control variables --------------------------------------------------
## Risk knowledge
# Replace value that has been entered before the check for a number was implemented in Qualtrics. The actual value in the raw file is "Über 70".
d[6,"know_age"] <- 70
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
d$know_age = 1-range01(abs(d$know_age - 65))
d$know_infected_all_ab = 1-range01(abs(d$know_infected_all_ab - 32500))
d$know_death_total = 1-range01(abs(d$know_death_total - 1750))
symptoms_correct = c(1, 2, 3, 4, 5, 10)
symptoms=sapply(d$know_symptoms, strsplit, split=",")
d$know_symptoms=unlist(unname(lapply(symptoms, function(x)mean(as.numeric(x)==symptoms_correct))))
d[, know__health := rowMeans(.SD), .SDcols = patterns("know_age|know_infected_all_ab|know_death_total|know_symptoms")]
# Recode know_econ
recode_econ = data.table(old=c(1,2,3,13,4,14,15,5,6,7,8,9,16,10,12), new=c(25,75,125,175,225,275,325,375,425,475,525,575,625,675,725))
d$know__econ = sapply(d$know_econ, function(x){recode_econ[old==x, new]})
## Technology
d$tech=sapply(1:nrow(d), function(i){
  if(d[i, has_smartphone] == 1)
    mean(d[i, c(tech_general, tech_interest_apps, tech_interest_ability), with=TRUE])
  else
    d[i, tech_general]
})
## Policy attitudes
d$attitudes_civilrights = 6-d$attitudes_civilrights 
d[, policy := rowMeans(.SD), .SDcols = patterns("^attitudes_")]
##Mental health
d[, mhealth := rowMeans(.SD), .SDcols = patterns("^mhealth_")]
## Comprehension
d$comprehension_self = 6-d$comprehension_self
d[, comprehension := rowMeans(.SD), .SDcols = patterns("^comprehension_")]
## Household
d$household_kids = ifelse(is.na(d$household_kids), 0, d$household_kids)
## Symptoms
d[, has__symptoms := rowSums(.SD), .SDcols = patterns("^has_symptoms_")]
## econ
d$income_loss = ifelse(is.na(d$income_loss), -1, d$income_loss)
d$homeoffice = ifelse(is.na(d$homeoffice), -1, d$homeoffice)


#Delete participants who did not pay attention
attention_index=d$check1_correct3==3&d$check2_correct1==1&d$check3_correct5==5
sum(attention_index==FALSE)
attention_percent=sum(attention_index)/nrow(d)
d=d[attention_index,]

nrow(d)

# Delete the columns that were used to create the variables (TODO) ------------------
d[, grep("(^svo_)(.*)([0-9])", colnames(d)):=NULL]
d[, grep("^mhealth_|^iwah_|^svo_|^acc_|^com_|^vacc|^comprehension_|^honhum_|^perc_|^attitudes_|^know|^tech_|belief_|has_symptoms", 
         colnames(d)[1:100]):=NULL]

setnames(d, gsub("__", "_", names(d)))
nrow(d)
# Save data
fwrite(d, "../../data/processed/test.csv")

