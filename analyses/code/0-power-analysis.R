# ==========================================================================
# Power-analysis - Frequentist Version
# ==========================================================================
if (!require(pacman)) install.packages("pacman")
pacman::p_load(twipc, WebPower)

# 1. Analysis in grant (simple mean comparison)
# 2. Augmented regression analysis

# Between-subject design, interaction
# Two factors
# Let factor values of the two factors be 00, 01, 10, 11
twipc_means(
  a1b1 = 0,       # 00
  a1b2 = 0.2,     # 01
  a2b1 = 0.2,     # 10
  a2b2 = 0,       # 11
  start = 750, end = 800, by = 25,
  alpha = 0.05, reps = 1000,
  verbose = FALSE)


# Alternative approach: regression using Cohen's f2
#   f2 > 0.02 small
#   f2 > 0.15 medium
#   f2 > 0.35 large
wp.regression(
  p1 = 10 * 2,  # num predictors in full model
  p2 = 0,       # num predictors in null model (= no interactions)
  f2 = 0.02,    # f2
  power = 0.80)