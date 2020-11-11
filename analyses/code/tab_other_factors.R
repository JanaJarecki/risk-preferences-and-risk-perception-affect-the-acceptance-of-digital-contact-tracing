# ==========================================================================
# Tables of Regression Coefficients for Policy Support & Comprehension
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(data.table, kableExtra, magrittr, broom.mixed, dplyr, stringr)
if (rstudioapi::isAvailable()) { setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }

# Policy --------------------------------------------------------------------
fit_policy <- readRDS(paste0("fitted_models/policy_score_fit_reduced_all.rds")
summary(fit_policy)

tidy(fit_policy) %>% 
  filter(is.na(group)) %>% 
  mutate(term = c('Intercept', 'Comprehension of DCTAs', 'Risk perception - data security risks', 'Risk perception - economic risks', 'Risk seeking - general', 'Mental health in last 30 days', 'Risk seeking - health', 'Gender - female', 'Gender - male')) %>% 
  rename_all(str_to_title) %>% 
  select(!1:3) %>%
  kable(
    format = "latex",
    digits = 2,
    booktabs = TRUE,
    escape = FALSE,
    format.args = list(scientific = F),
    label = "tab_results_policy_score",
    caption = "Results of the Bayesian regression: Effects on the acceptance of COVID-19-related policital measures in Switzerland.",
    col.names = c("Term", "Estimate", "SE", "5\\%", "95\\%")
    ) %>%
  add_header_above(c("", "", "", "CI" = 2)) %>%
  kable_styling(latex_options = "hold_position") %>%
  add_footnote(label = "Note. CIs are Bayesian credibility intervalls. DCTA = digital contact tracing application.", notation="none") %>%
  writeLines(con = paste0("../tables/policy_score_coef_estimates.tex"))


# Comprehension ---------------------------------------------------------------
fit_compreh <- readRDS("fitted_models/compreh_score_fit_reduced_all.rds")
summary(fit_compreh)

tidy(fit_compreh) %>% 
  filter(is.na(group)) %>% 
  mutate(term = c('Intercept', 'Support for political measures', 'Risk perception - data security risks', 'Risk seeking - data security risks', 'Interest in new technologies', 'Not working from home', 'Partially working from home')) %>% 
  rename_all(str_to_title) %>% 
  select(!1:3) %>%
  kable(
    format = "latex",
    digits = 2,
    booktabs = TRUE,
    escape = FALSE,
    format.args = list(scientific = F),
    label = "tab_results_compreh_score",
    caption = "Results of the Bayesian regression: Effects on the comprehension of the functioning of digital contact tracing applications in Switzerland.",
    col.names = c("Term", "Estimate", "SE", "5\\%", "95\\%")
    ) %>%
  add_header_above(c("", "", "", "CI" = 2)) %>%
  kable_styling(latex_options = "hold_position") %>%
  add_footnote(label = "Note. CIs are Bayesian credibility intervalls. DCTA = digital contact tracing application.", notation="none") %>%
  writeLines(con = paste0("../tables/compreh_score_coef_estimates.tex"))