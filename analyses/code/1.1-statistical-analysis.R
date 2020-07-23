if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, brms)

# set working directory to THIS file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Load preprocessed data
d <- fread("../../data/processed/test.csv")

# Regression, dependent variable: acceptance_index ----------------------------
priors <- prior_string("normal(0,10)")

fit_acc_all_main <- brm(formula = accept_index ~ 
                         #IV 
                         perc_health +
                          perc_data +
                          perc_moneyloss +
                          risk_general + 
                          risk_domain_health + 
                          risk_domain_data + 
                          risk_domain_economic + 
                          iwah_community +
                          iwah_world +
                          svo_angle +
                          honhum +
                         #Mod
                          belief_efficiency +
                          belief_local +
                          belief_global +
                         #Control
                         know_health +
                         know_econ +
                         female +
                         age +
                         education +
                         household +
                         household_kids +
                         was_infected +
                         is_infected +
                         has_symptoms +
                         had_work +
                         has_work +
                         income_loss +
                         policy +
                         mhealth +
                         tech +
                         comprehension,
                        data = d, 
                        prior = priors, 
                        save_all_pars=T)

fit_acc_all_mod <- brm(formula = accept_index ~ 
                          #IV 
                          perc_health +
                          perc_data +
                          perc_moneyloss +
                          risk_general + 
                          risk_domain_health + 
                          risk_domain_data + 
                          risk_domain_economic + 
                          iwah_community +
                          iwah_world +
                          svo_angle +
                          honhum +
                          perc_health:belief_efficiency +
                          perc_data:belief_efficiency +
                          perc_moneyloss:belief_efficiency +
                          risk_general:belief_efficiency + 
                          risk_domain_health:belief_efficiency + 
                          risk_domain_data:belief_efficiency + 
                          risk_domain_economic:belief_efficiency + 
                          iwah_community:belief_efficiency +
                          iwah_world:belief_efficiency +
                          svo_angle:belief_efficiency +
                          honhum:belief_efficiency +
                          #Mod
                          belief_efficiency +
                          belief_local +
                          belief_global +
                          #Control
                          know_health +
                          know_econ +
                          female +
                          age +
                          education +
                          household +
                          household_kids +
                          was_infected +
                          is_infected +
                          has_symptoms +
                          had_work +
                          has_work +
                          income_loss +
                          policy +
                          mhealth +
                          tech +
                          comprehension,
                        data = d, 
                        prior = priors, 
                        save_all_pars=T)

bayes_factor(fit_acc_all_main, fit_acc_all_mod)

hdi(fit_acc_all_main)

fit_com_all_main <- brm(formula = comply_index ~ 
                         #IV 
                         perc_health +
                         perc_data +
                         perc_moneyloss +
                         risk_general + 
                         risk_domain_health + 
                         risk_domain_data + 
                         risk_domain_economic + 
                         iwah_community +
                         iwah_world +
                         svo_angle +
                         honhum +
                         #Mod
                         belief_efficiency +
                         belief_local +
                         belief_global +
                         #Control
                         know_health +
                         know_econ +
                         female +
                         age +
                         education +
                         household +
                         household_kids +
                         was_infected +
                         is_infected +
                         has_symptoms +
                         had_work +
                         has_work +
                         income_loss +
                         policy +
                         mhealth +
                         tech +
                         comprehension,
                       data = d, 
                       prior = priors, 
                       save_all_pars=T)

fit_vacc_all_main <- brm(formula = vaccine_index ~ 
                         #IV 
                         perc_health +
                         perc_data +
                         perc_moneyloss +
                         risk_general + 
                         risk_domain_health + 
                         risk_domain_data + 
                         risk_domain_economic + 
                         iwah_community +
                         iwah_world +
                         svo_angle +
                         honhum +
                         #Mod
                         belief_efficiency +
                         belief_local +
                         belief_global +
                         #Control
                         know_health +
                         know_econ +
                         female +
                         age +
                         education +
                         household +
                         household_kids +
                         was_infected +
                         is_infected +
                         has_symptoms +
                         had_work +
                         has_work +
                         income_loss +
                         policy +
                         mhealth +
                         tech +
                         comprehension,
                       data = d, 
                       prior = priors, 
                       save_all_pars=T)

library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(gganimate)

get_variables(fit_acc_all_main)


#bayes_factor(fit_acc, fit_acc_eff_mod)
#fit2 <- update(fit, . ~ . - know_econ)
#summary(fit2)


