# ==========================================================================
# Power Analysis
# Frequentist Version
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
  p1 = 10  *  2,  # num predictors in full model
  p2 = 0,       # num predictors in null model ( =  no interactions)
  f2 = 0.10,    # f2
  power = 0.80)


foo <- function(power = 0.90, alpha = 0.05, sigmasq = 16, betaxz = 1) {

  # SPECIFY
  digits <- 3
  xzvec <- c(
    0.11, -1.02, 0.58, -0.46, 0.27, 0.51, 0.64, 0.35, 0.76, 0.48,
    0.98, 0.26, -0.76, 0.06, -0.18, 0.15, 0.78, 0.70, 0.18, 0.47,
    -0.58, 0.91, 0.28, 1.18, 1.14, 1.43, 0.83, -0.86, -0.78, 0.17,
    0.61, -0.17, 0.08, 0.74,  -0.67, -1.70, 1.52, 0.32, 0.18, 0.85,
    0.04, 2.06, 1.08, -0.31, -0.15, -0.62, -0.50, 0.79, -0.30, -0.02,
    0.60, 0.56, -0.49, 0.60, 0.87, 0.34, -0.29, -0.66, -1.04, 1.30,
    0.14, -1.35, -1.12, -0.79, 0.74, 1.68, -0.69, -1.44, -0.80, -1.01,
    -3.21, -1.91, -0.42, -0.49, 2.79, 2.35, -0.47, -0.96, -0.77, -1.58)
  #END OF REQUIRED USER SPECIFICATION

  xz <- matrix(xzvec, length(xzvec)/2, 2, byrow = TRUE)
  xe <- cbind(xz, xz[,1]*xz[,2])
  n  <- nrow(xe)
  cmean <- colMeans(xe)
  xc <- t(t(xe) - cmean)
  h <- matrix(rep(0,9), 3, 3)
  hh <- h %x% h
  for (i in 1:n) {
    xci <- xc[i, , drop = FALSE]
    h <- h + (t(xci) %*% (xci))
    hh <- hh + (t(xci) %*% (xci)) %x% (t(xci) %*% (xci))
  }
  sigm <- h/n
  psi <- hh/n
  isigm <- solve(sigm)
  muw <- 1/isigm[3,3]
  vw <- isigm[3,,drop = FALSE]
  varw <- (muw^4) * (((vw) %x% (vw)) %*% psi %*% (t(vw) %x% t(vw)) - muw^(-2))
  lfxz <- betaxz * sqrt(muw/sigmasq)
  print(c(alpha, power, n, betaxz, sigmasq), digits = digits)
  print(c(muw, varw, lfxz), digits = digits)

  # for numerical integration
  numint <- 1000
  coevec <- c(1,rep(c(4,2), numint/2 - 1),4,1)
  int <- qnorm(0.999995, 0, 1)
  interval <- 2 * int/numint
  zvec <- interval * seq(0,numint) + (-int)
  wzpdf <- (interval/3) * coevec * dnorm(zvec, 0,1)
  # ST approach
  stpower <- 0
  m <- 5
  while (stpower < power){
    m <- m + 1
    tcrit <- qt(1-alpha/2,m-4,0)
    stpower <- 1-pt(tcrit, m-4,sqrt(m) * lfxz)
  }
  nst <- m

  # NT approach
  ntpower <- 0
  m <- max(nst - 10, 5)
  while (ntpower < power) {
    m <- m + 1
    tcrit <- qt(1-alpha/2,m-4,0)
    wvec <- sqrt(varw/(m-1)) * zvec + muw
    wvec <- wvec * (wvec>0)
    ntpower <- 1-sum(wzpdf * pt(tcrit, m-4,betaxz * sqrt((m-1) * wvec/sigmasq)))
  }
  nnt <- m
  #recalculate ntpower for sample size nst
  m <- nst
  tcrit <- qt(1 - alpha/2, m-4, 0)
  wvec <- sqrt(varw/(m-1)) * zvec + muw
  wvec <- wvec * (wvec > 0)
  ntpower_nst <- 1 - sum(wzpdf * pt(tcrit, m-4,betaxz * sqrt((m-1) * wvec/sigmasq)))
  dn <- nnt - nst
  dntpower <- ntpower - ntpower_nst
  cat("\nN and Power\n")
  print(c(N_ST = nst, Pow_ST = stpower, N_NT = nnt, Pow_NT = ntpower), digits = digits)
  print(c(dn, ntpower_nst, dntpower), digits = digits)
}

foo(power = 0.80, betaxz = 0.50)