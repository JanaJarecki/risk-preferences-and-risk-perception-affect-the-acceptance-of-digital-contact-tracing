# ==========================================================================
# Preprocessing of the main BCRQ study
# ==========================================================================

# Setup --------------------------------------------------------------------
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


# Load -----------------------------------------------------------------------
# Load raw data, set column names to lower-case names
d <- setnames(
  fread("../../data/raw/data.csv", skip = 2),
  tolower(names(fread("../../data/raw/data.csv", nrows = 0))))
# Delete left over survey preview data
d <- d[status != "Survey Preview" & status != 1]
# Drop columns
drop <- c("status", "ipaddress", "recordeddate", "recipientlastname", "recipientfirstname", "recipientemail", "externalreference", "locationlatitude", "locationlongitude", "distributionchannel", grep("^qid", names(d), value = T))
d <- d[, !drop, with = FALSE]
# Rename variables
setnames(d, c("responseid", "duration (in seconds)"), c("id", "duration_seconds"))
setnames(d, c("risk_domain_1", "risk_domain_2", "risk_domain_3", "risk_general_1"), paste0("risk_", c("health", "data", "economy", "general")))
setcolorder(d, "id")
# Spelling error in wealth variable names
setnames(d, gsub("wealtch", "wealth", names(d)))


# Exclude participants --------------------------------------------------------
# List to store the reasons for exclusions
excluded <- list()
excluded$progress    <- d[progress < 100]$id
excluded$dataquality <- d[data_quality < 4]$id
excluded$attention   <- d[, all(.SD == c(3,1,5)), .SDcols = patterns("^check"), by = id][V1 == FALSE]$id
d <- d[!id %in% unlist(excluded)]
# Participants who did not complete IWAH
x <- apply(d[, .SD, .SDcols = patterns("^iwah_")], 1, min)
d <- d[!(x < 0)]


# Reverse polarity of variables ----------------------------------------------
to_reverse <- c("honhum_makemoney", "honhum_celebrity", "honhum_special","attitudes_civilrights", "comprehension_self")
d[, c(to_reverse) := lapply(.SD, function(x) 6 - x), .SDcols = to_reverse]



# Recode variables -------------------------------------------------------
recode_dict   <- data.table(old=c(11:20), new=c(2,8,13,18,23,28,33,38,43,46))
d$perc_severe <- sapply(d$perc_severe, function(x){recode_dict[old==x, new]})
d$perc_data  <- sapply(d$perc_data, function(x){recode_dict[old==x, new]})
d$perc_moneyloss<-sapply(d$perc_moneyloss, function(x){recode_dict[x==(old-10), new]})
recode_dict2 <- data.table(old=c(1,2,3,13,4,14,15,5,6,7,8,9,16,10,12), new=c(25,75,125,175,225,275,325,375,425,475,525,575,625,675,725))
d$know_econ = sapply(d$know_econ, function(x){recode_dict2[old==x, new]})



# Transform and impute variables ----------------------------------------------
# Replace NA values
d$household_kids  <- ifelse(is.na(d$household_kids), 0, d$household_kids)
d$income_loss     <- ifelse(is.na(d$income_loss), -1, d$income_loss)
d$homeoffice      <- ifelse(is.na(d$homeoffice), -1, d$homeoffice)
# Replace value that has been entered before the check for a number was implemented in Qualtrics. The actual value in the raw file is "Über 70".
d[id == "R_2uE8zq3yYMhqyaI", "know_age"] <- 70
# Transform the knowledge of corona symptoms iinto correct or wrong
calc_perc_correct <- function(x) {
   # Calculation takes into account true pos and true neg symptoms
   symptoms_correct <- c(1, 2, 3, 4, 5, 10)
   symptoms_wrong <- c(6, 7, 8, 9, 10)
   x <- as.numeric(unlist(strsplit(x, ",")))
   true_positive <- symptoms_correct %in% x
   false_negative <- symptoms_wrong %in% x
   return(mean(c(true_positive, !false_negative)))
}
d[, know_symptoms_perc := calc_perc_correct(know_symptoms), by = id]
# Technology usage
d[has_smartphone == 1, tech := rowMeans(.SD), .SDcols = c("tech_general", "tech_interest_apps", "tech_interest_ability")]
d[has_smartphone == 0, tech := tech_general]



# Normalize variables ---------------------------------------------------------
norm_range <- function(x) {
   (x - min(x)) / (max(x) - min(x))
}
d$raw_know_age <- d$know_age
d$raw_know_infected_all_ab <- d$know_infected_all_ab
d$raw_know_death_total <- d$know_death_total
d$know_age             <- 1-norm_range(abs(d$know_age - 65))
d$know_infected_all_ab <- 1-norm_range(abs(d$know_infected_all_ab-32500))
d$know_death_total     <- 1-norm_range(abs(d$know_death_total - 1750))



# Make dependent variables ----------------------------------------------------
# accept_index & comply_index & vacc_index
d[, accept_index := rowMeans(.SD), .SDcols = patterns("^acc_")]
d[, comply_index := rowMeans(.SD), .SDcols = patterns("^com_")]
d[, vaccine_index := rowMeans(.SD), .SDcols = patterns("^vacc_")]



# Make independent variables -----------------------------------------
d[, honhum__score     := rowMeans(.SD), .SDcols = patterns("^honhum_")]
d[, perc__health :=rowMeans(.SD), .SDcols=patterns("^perc_infect|perc_severe")]
d[, iwah__community   := rowMeans(.SD), .SDcols = patterns("^iwah_.*_1")]
d[, iwah__swiss       := rowMeans(.SD), .SDcols = patterns("^iwah_.*_2")]
d[, iwah__world       := rowMeans(.SD), .SDcols = patterns("^iwah_.*_3")]
d[, iwah__diff__score := iwah__community - iwah__world]
d[, svo__angle        := suppressWarnings(calc_svo_angle(d))] # see utilities.R
d[, policy__score     := rowMeans(.SD), .SDcols = patterns("^attitudes_")]
d[, mhealth__score    := rowMeans(.SD), .SDcols = patterns("^mhealth_")]
d[, tech__score       := rowMeans(.SD), .SDcols = patterns("^tech_")]
d[, compreh__score    := rowMeans(.SD), .SDcols =patterns("^comprehension_")]
d[, has__symptoms     := rowSums(.SD), .SDcols = patterns("^has_symptoms_")]
d[, know__health      := rowMeans(.SD), .SDcols = patterns("know_age|know_infected_all_ab|know_death_total|know_symptoms_perc")]
# Moderators
d[, belief__efficiency:= rowMeans(.SD),.SDcols=patterns("belief_eff|belief_5")]
d[, belief__local     := rowMeans(.SD), .SDcols = patterns("belief_local")]
d[, belief__global    := rowMeans(.SD), .SDcols = patterns("belief_global")]




# Delete the columns that were used to create the variables ------------------
# d[, grep("(^svo_)(.*)([0-9])", colnames(d)):=NULL]
# d[, grep("^mhealth_|^iwah_|^svo_|^acc_|^com_|^vacc|^comprehension_|^honhum_|^perc_|^attitudes_|^know|^tech_|belief_|has_symptoms", 
#          colnames(d)[1:100]):=NULL]


# Cosmetics
setnames(d, c("belief_efficiency", "belief_local", "belief_global"), paste0("raw_", c("belief_efficiency", "belief_local", "belief_global")))
setnames(d, gsub("__", "_", names(d)))



# Save data ------------------------------------------------------------------
fwrite(d, "../../data/processed/data.csv")



# Exclusion report -----------------------------------------------------------
excluded <- rbindlist(lapply(excluded, as.data.table), id = "reason")
excluded <- dcast(excluded, V1 ~ reason, value.var = "reason")
cat("\n---------------------------",
   "Exclusion Report\n"
   )
print(as.matrix(excluded[, list(Count=.N), c(names(excluded)[-1])]), na.print = "", quote=F, row.names=F)