

#risk classification
riskName <- 'Ransomware Data Breach'

lossOver1year <- 0.025 # 2.5 %

impactLowBound <- 2850000
impactUpBound <- 28500000

Risk_Estimates_D10 <- lossOver1year
Risk_Estimates_F10 <- impactUpBound
Risk_Estimates_E10 <- impactLowBound

expr <- exp(
  ((log(Risk_Estimates_F10) + log(Risk_Estimates_E10)) / 2) +
    ((log(Risk_Estimates_F10) - log(Risk_Estimates_E10)) / 3.28971)^2 / 2
) * Risk_Estimates_D10


confIntervalWidth <- qnorm(0.95) - qnorm(0.05)

expr <- exp(
  ((log(impactUpBound) + log(impactLowBound)) / 2) +
    ((log(impactUpBound) - log(impactLowBound)) / confIntervalWidth)^2 / 2
) * Risk_Estimates_D10



ExpectedInherentLoss <- ifelse(is.finite(expr), expr, 0)


###


TrialID <- 100
K_8 <- 100000
Risk_Estimates_A10 <- 1


# Custom deterministic pseudo-random generator
pseudo_random <- function(TrialID, K_8, Risk_Estimates_A10) {
  # Combine inputs into a 64-bit seed using arbitrary but fixed large primes
  seed <- (TrialID * 2499997 +
             K_8 * 1800451 +
             Risk_Estimates_A10 * 2299603) %% 4294967296
  
  # Linear congruential generator step
  next_val <- (1103515245 * seed + 12345) %% 4294967296
  
  # Normalize to [0, 1)
  result <- (next_val + 0.5) / 2^32
  return(result)
}

ProbabilityRand <- pseudo_random(TrialID, K_8, Risk_Estimates_A10)

#set.seed(TrialID + K_8 + Risk_Estimates_A10)
#runif(1)


L_8 <- 100000

pseudo_random_L <- function(TrialID, L_8, Risk_Estimates_A10) {
  # Combine inputs into a 64-bit seed using fixed large primes
  seed <- (TrialID * 2499997 +
             L_8 * 1800451 +
             Risk_Estimates_A10 * 2299603) %% 4294967296
  
  # Linear congruential generator step
  next_val <- (1103515245 * seed + 12345) %% 4294967296
  
  # Normalize to [0, 1)
  result <- (next_val + 0.5) / 2^32
  return(result)
}



pseudo_random_L(TrialID, L_8, Risk_Estimates_A10)



Risk_Estimates_H10 = 0
Risk_Estimates_J10 = ExpectedInherentLoss

ExpectedResidualLossExpr <- (1 - Risk_Estimates_H10) * Risk_Estimates_J10
ifelse(is.finite(ExpectedResidualLossExpr), ExpectedResidualLossExpr, Risk_Estimates_J10)


###

Risk_Estimates_D10 <- lossOver1year
Risk_Estimates_K10 <- ProbabilityRand 

tryCatch(
  ifelse(
    Risk_Estimates_K10 < (1 - Risk_Estimates_H10) * Risk_Estimates_D10,
    qlnorm(
      Risk_Estimates_L10,
      (log(Risk_Estimates_F10) + log(Risk_Estimates_E10)) / 2,
      (log(Risk_Estimates_F10) - log(Risk_Estimates_E10)) / 3.29
    ),
    0
  ),
  error = function(e) 0
)