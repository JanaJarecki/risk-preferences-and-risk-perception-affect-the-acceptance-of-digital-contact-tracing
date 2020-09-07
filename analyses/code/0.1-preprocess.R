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
#   * Use "choice text", not numeric variables
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
setnames(d, c("risk_domain_1", "risk_domain_2", "risk_domain_3", "risk_general_1"), paste0("seek_risk_", c("health", "data", "econ", "general")))
setcolorder(d, "id")
# Spelling error in wealth variable names
setnames(d, gsub("wealtch", "wealth", names(d)))


# Exclude participants --------------------------------------------------------
# List to store the reasons for exclusions
excluded <- list()
excluded$progress    <- d[progress < 100]$id
excluded$dataquality <- d[as.numeric(data_quality) < 4]$id
excluded$attention   <- d[, all(.SD == c(3,1,5)), .SDcols = patterns("^check"), by = id][V1 == FALSE]$id
d <- d[!id %in% unlist(excluded)]
# Participants who did not complete IWAH
x <- apply(d[, .SD, .SDcols = patterns("^iwah_")], 1, min)
d <- d[!(x < 0)]


# Reverse polarity of variables ----------------------------------------------
to_reverse <- c("honhum_makemoney", "honhum_celebrity", "honhum_special","attitudes_civilrights", "comprehension_self")
d[, c(to_reverse) := lapply(.SD, function(x) 6 - x), .SDcols = to_reverse]



# Recode variables -------------------------------------------------------
recode_dict <- data.table(old=11:20,new=c(seq(0.025,0.45,0.05),.70))
d[, perc_risk_severe := recode_dict[copy(.SD),on=.(old=perc_severe),x.new]]
d[, perc_risk_data := recode_dict[copy(.SD),on=.(old=perc_data),x.new]] #fast!
recode_dict <- data.table(old=c(1:10),new=c(seq(0.025,0.45,0.05),.70))
d[, perc_risk_econ := recode_dict[copy(.SD),on=.(old=perc_moneyloss),x.new]]
recode_dict <- data.table(old=c(1,2,3,13,4,14,15,5,6,7,8,9,16,10,12), new=c(25,75,125,175,225,275,325,375,425,475,525,575,625,675,725))
d[, know_econ := recode_dict[copy(.SD), on=.(old=know_econ), x.new]]



# Replace values -----------------------------------------------------------
# Replace values of variable that were not shown
d$household_kids  <- ifelse(is.na(d$household_kids), 0, d$household_kids)
# `income_loss` only shown if had_work == 1, recode to 0 = no loss of income
d$income_loss     <- ifelse(is.na(d$income_loss), 0, d$income_loss)
# 'homeoffice'  only shown if has_work == 1, recode to 0 = not working at home
d$homeoffice      <- ifelse(is.na(d$homeoffice), 0, d$homeoffice)
# Replace value that has been entered before the check for a number was implemented in Qualtrics. The actual value in the raw file is "Über 70".
d[id == "R_2uE8zq3yYMhqyaI", "know_age"] <- 70



# Clean open-ended texts -----------------------------------------------------
# Non-numeric entries in open text-fields in columns: wealth, income, community
cols  <- c("income_num_1_text", "wealth_num_1_text", "community_1_text")
nonum <- "'|.-| \\(Familie\\)| \\(nur mein\\)| |\\*|\\^|,|`"
d[, c(cols):= lapply(.SD, gsub, pattern=nonum, replacement=""), .SDcols=cols]
# These need manual replacement
d[income_num_1_text=="370Ã¼", income_num_1_text := "3700"]
d[wealth_num_1_text=="3mio", wealth_num_1_text := "3000000"]
d[wealth_num_1_text=="700â\200\230000", wealth_num_1_text := "700000"]
d[wealth_num_1_text=="8ooo", wealth_num_1_text := "8000"]
d[community_1_text=="350Ã¼", community_1_text := "3500"]
d[community_1_text=="2â\200\230500", community_1_text := "2500"]



# Impute variables ------------------------------------------------------------
# Community size is imputed based on the mean of the category
d$community_imputed <- as.numeric(d$community_1_text)
dict <- data.table(community_cat=0:5,new=c(NA,250,7500,25000,75000,200000))
d[community==2, community_imputed := dict[copy(.SD),on=.(community_cat),x.new]]
# Income
# Categorical responses imputed by mean of the categories: 0-1000 = 500, etc,
d$income_imputed <- as.numeric(d$income_num_1_text)
dict <- data.table(income_cat=0:9, new=c(NA,100*c(5,15,25,35,45,55,65,75,85)))
d[income_num==0, income_imputed := dict[copy(.SD),on=.(income_cat), x.new]]
sum(is.na(d$income_imputed)) # Still 140 missing values
# Wealth
# Imputed like income based on mean
d$wealth_imputed <- as.numeric(d$wealth_num_1_text)
dict <- data.table(wealth_cat=0:8,new=c(NA,1000*c(0,12.5,37.5,75,150,350,750,1500)))
d[wealth_num==0, wealth_imputed := dict[copy(.SD), on=.(wealth_cat), x.new]]
sum(is.na(d$wealth_imputed)) #  Still 175 missing values



# Transform values --------------------------------------------------------
# The knowledge of corona symptoms into correct or wrong
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
# Normalize values
norm_range <- function(x) {
   (x - min(x)) / (max(x) - min(x))
}
d$raw_know_age <- d$know_age
d$raw_know_infected_all_ab <- d$know_infected_all_ab
d$raw_know_death_total <- d$know_death_total
d$know_age             <- 1-norm_range(abs(d$know_age - 65))
d$know_infected_all_ab <- 1-norm_range(abs(d$know_infected_all_ab-32500))
d$know_death_total     <- 1-norm_range(abs(d$know_death_total - 1750))
d$perc_infected_next7 <- scale(d$perc_infected_next7)
d$perc_infected_last7 <- scale(d$perc_infected_last7)
d$perc_risk_severe <- scale(d$perc_risk_severe)



# Make dependent variables ----------------------------------------------------
# accept_index & comply_index & vacc_index
d[, accept_index := rowMeans(.SD), .SDcols = patterns("^acc_")]
d[, comply_index := rowMeans(.SD), .SDcols = patterns("^com_")]
d[, vaccine_index := rowMeans(.SD), .SDcols = patterns("^vacc_")]




# Make independent variables -----------------------------------------
d[, honhum_score     := rowMeans(.SD), .SDcols = patterns("^honhum_")]
d[, perc__risk__health :=rowMeans(.SD), .SDcols=patterns("^perc_infect|perc_risk_severe")]
d[, iwah__community   := rowMeans(.SD), .SDcols = patterns("^iwah_.*_1")]
d[, iwah__swiss       := rowMeans(.SD), .SDcols = patterns("^iwah_.*_2")]
d[, iwah__world       := rowMeans(.SD), .SDcols = patterns("^iwah_.*_3")]
d[, iwah__diff_score := iwah__community - iwah__world]
d[, svo__angle        := suppressWarnings(calc_svo_angle(d))] # see utilities.R
d[, policy_score     := rowMeans(.SD), .SDcols = patterns("^attitudes_")]
d[, mhealth_score    := rowMeans(.SD), .SDcols = patterns("^mhealth_")]
d[, tech_score       := rowMeans(.SD), .SDcols = patterns("^tech_")]
d[, compreh_score    := rowMeans(.SD), .SDcols =patterns("^comprehension_data|comprehension_other|comprehension_severe")]
d[, has__symptoms    := rowSums(.SD), .SDcols = patterns("^has_symptoms_")]
d[, know__health_score := rowMeans(.SD), .SDcols = patterns("know_age|know_infected_all_ab|know_death_total|know_symptoms_perc")]
d[, belief__efficiency:= rowMeans(.SD),.SDcols=patterns("belief_eff|belief_5")]
d[, belief__local     := rowMeans(.SD), .SDcols = patterns("belief_local")]
d[, belief__global    := rowMeans(.SD), .SDcols = patterns("belief_global")]
d[, safebehavior_score:= rowMeans(.SD), .SDcols = patterns("^safety_")]
d[has_smartphone == 0L, tech_score := as.double(tech_general)]
d[has_smartphone == 1L, tech_score := rowMeans(.SD, na.rm = TRUE), .SDcols = patterns("^tech_")]


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
cat("\nExclusion Report\n"
   )
print(as.matrix(excluded[, list(Count=.N), c(names(excluded)[-1])]), na.print = "", quote=F, row.names=F)