if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, data.table, brms, projpred, standardize)
# set working directory to THIS file location (if rstudio)
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }


# Load data ---------------------------------------------------------------
d <- fread("../../data/processed/data.csv")
d$seek_risk_data
cor.test(d$accept_index, d$seek_risk_data)
cor.test(d$accept_index, d$seek_risk_health)
cor.test(d$accept_index, d$perc_risk_data)
cor.test(d$accept_index, d$perc_risk_health)
cor.test(d$policy_score, d$perc_risk_econ)

d_tibble <- as_tibble(d) %>% 
  mutate(seek_risk_data_z = (seek_risk_data - mean(seek_risk_data))/sd(seek_risk_data),
         seek_risk_health_z = (seek_risk_health - mean(seek_risk_health))/sd(seek_risk_health),
         perc_risk_data_z = (perc_risk_data - mean(perc_risk_data))/sd(perc_risk_data),
         perc_risk_health_z = (perc_risk_health - mean(perc_risk_health))/sd(perc_risk_health)
         )

d_tibble <- d_tibble  %>% 
  select(accept_index, seek_risk_data_z, seek_risk_health_z, perc_risk_data_z, perc_risk_health_z) %>%
  pivot_longer(c(seek_risk_data_z, seek_risk_health_z, perc_risk_data_z, perc_risk_health_z))


d_tibble <- d_tibble %>% 
  mutate( name_1 = sapply(name, function(x)str_split(x, "_")[[1]][1]),
          Domain = sapply(name, function(x)str_split(x, "_")[[1]][3]),
          name_1 = ifelse(name_1 == "perc", "Risk Perception", "Risk Seeking Preference"),
          Domain = ifelse(Domain == "data", "Data", "Health"))

ggplot(data=d_tibble, aes(x=accept_index, y=value, color=Domain)) + 
  geom_smooth(method=lm) + #fill = "grey85"
  facet_wrap(~name_1) + 
  #scale_colour_manual(values = c("grey0", "grey50")) +
  labs(x = "Acceptance", y="Z-score") +
  theme_classic() 

### Compare projection and fitted model
dep_var <- "accept_index"
cvs <- readRDS(paste0("fitted_models/", dep_var, "_variable_selection.rds"))
nvar <- suggest_size(cvs)
proj <- project(cvs, nv = nvar, ns = 1000)

predictors_selected <- names(cvs$vind[1:nvar])
formula <- reformulate(
  termlabels = predictors_selected,
  response = dep_var)
sobj <- standardize(formula = formula, d)
fit_reduced <- brm(formula = formula, family = gaussian(), data = sobj$data,
                       prior = prior_coeff,
                       save_all_pars = TRUE, sample_prior = "yes",
                       iter = 8000,
                       seed = 42,
                       control = list(
                         adapt_delta = 0.9 # because horseshoe prior prone to divergent transitions
                       ),
                       file = paste0("fitted_models/", dep_var, "_fit_reduced_all"))

proj$beta
