rm(list=ls())

if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, brms, projpred, bayesplot, standardize, mice, tidyverse)
# set working directory to THIS file location (if rstudio)
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }

# Load data ---------------------------------------------------------------
d <- fread("../../data/processed/data.csv")

# Preprocess ----------------------------------------------------------------
# IWAH difference score
d[, iwah_diff_score := iwah_community - iwah_world]
# Factors
fac <- c("has_work", "income_loss", "is_infected", "was_infected")
d[, (fac) := lapply(.SD, factor, labels=c("no", "yes")), .SDcols = fac]
d[, homeoffice := factor(homeoffice, labels = c("no", "yes", "partially"))]
d[, female := factor(female, labels = c("male", "female", "no response"))]
# Impute missing income and wealth
# d[, summary(.SD), .SDcols=c("income_imputed", "wealth_imputed")] #140,175 NA
d[, income_imputed := fifelse(is.na(income_imputed), median(income_imputed, na.rm=T), income_imputed)]
d[, wealth_imputed := fifelse(is.na(wealth_imputed), median(wealth_imputed,na.rm=T), income_imputed)]


############## Risk knowledge
get_correct <- function(var, correct){
  res=c(mean(var == correct),  mean(var < correct),  mean(var > correct))
  names(res) =c("correct", "lower", "higher")
  res
}

# Read correct responses based on calculations from the BAG data file (retrieved 14.10.2020)
#Die Leute hatten keine Ahnung wie die App funktioniert aber dafür von der Krankheit
data_know = fread("../../data/processed/risk_knowledge.csv")
day=as.numeric(sapply(d$startdate, function(x){strsplit(strsplit(x, " ")[[1]][1], "-")[[1]][3]}))
d$day = day

know_age_correct = 65
know_infected_all_ab_correct = 32500
know_death_total_correct = 1750
perc_infected_last7_correct = 8
perc_infected_next7_correct = 8

get_correct(d$raw_know_infected_all_ab, know_infected_all_ab_correct)
get_correct(d$raw_know_death_total, know_death_total_correct)
get_correct(d$raw_perc_infected_last7, perc_infected_last7_correct)
get_correct(d$raw_perc_infected_next7, perc_infected_next7_correct)
get_correct(d$raw_perc_infected_last7, d$raw_perc_infected_next7)

mean(d$raw_perc_infected_last7 == max(d$raw_perc_infected_last7))
mean(d$raw_perc_infected_next7 == max(d$raw_perc_infected_next7))

d$comprehension_data
d$comprehension_self
d$comprehension_other
d$comprehension_severe
table(round(d$compreh_score))

risk_change = d$raw_perc_infected_next7 - d$raw_perc_infected_last7
ggplot(data=as.data.frame(risk_change)) + geom_bar(aes(x=risk_change))
risk_asses_last = d$raw_perc_infected_last7 - perc_infected_last7_correct
ggplot(data=as.data.frame(risk_asses_last)) + geom_bar(aes(x=risk_asses_last))
risk_asses_next = d$raw_perc_infected_next7 - perc_infected_next7_correct
ggplot(data=as.data.frame(risk_asses_next)) + geom_bar(aes(x=risk_asses_next))



know_symptoms=sapply(d$know_symptoms, strsplit, split=",")
m_know_symptoms=matrix(F, ncol=10, nrow=length(know_symptoms))
for(i in 1:length(know_symptoms)){
  m_know_symptoms[i, as.numeric(know_symptoms[[i]])] <- T
}
colSums(m_know_symptoms)
symptoms <- c(
  "Bluthochdruck","Herz-Kreislauf-Erkrankungen","Diabetes","Chronische Atemwegserkrankungen",
  "Krebs","Erkrankungen der Leber","Erkrankungen der Nieren",
  "Nervenst?rungen (Neurologische St?rungen)",
  "Magendarmerkrankungen (Gastrointestinale Erkrankungen)",
  "Erkrankungen und Therapien, die das Immunsystem schw?chen"
)
symptoms=as.data.frame(symptoms)
symptoms$N=colSums(m_know_symptoms)
symptoms = as_tibble(symptoms)

symptoms %>%  arrange(N) %>%  mutate(perc=N/nrow(d))
