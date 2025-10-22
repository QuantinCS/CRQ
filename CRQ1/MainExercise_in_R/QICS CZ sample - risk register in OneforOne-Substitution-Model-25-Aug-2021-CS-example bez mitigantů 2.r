

#risk classification
riskName <- 'Ransomware Data Breach'

lossOver1year <- 0.025 # 2.5 %

impactLowBound <- 2850000
impactUpBound <- 28500000

confIntervalWidth <- qnorm(0.95) - qnorm(0.05)

expr <- exp(
  ((log(impactUpBound) + log(impactLowBound)) / 2) +
    ((log(impactUpBound) - log(impactLowBound)) / confIntervalWidth)^2 / 2
) * Risk_Estimates_D10

ExpectedInherentLoss <- ifelse(is.finite(expr), expr, 0)

###
z_width <- 3.28971
z_width <- qnorm(0.95) - qnorm(0.05)

# Derive lognormal parameters (mu, sigma)
mu <- (log(impactUpBound) + log(impactLowBound)) / 2
sigma <- (log(impactUpBound) - log(impactLowBound)) / z_width

# Expected loss (same as in your expression)
expected_loss <- exp(mu + 0.5 * sigma^2) * lossOver1year
cat("Expected loss =", round(expected_loss, 2), "\n")

# Create loss values for plotting (log spaced)
loss_values <- exp(seq(log(impactLowBound/5), log(impactUpBound*5), length.out = 300))

# Compute exceedance probabilities (P(loss > L))
exceed_prob <- lossOver1year * (1 - pnorm((log(loss_values) - mu) / sigma))

# Plot exceedance curve
plot(loss_values, exceed_prob, type = "l", log = "x",
     xlab = "Loss amount", ylab = "Probability of exceedance",
     main = "Loss Exceedance Curve (Lognormal Model)",
     lwd = 2, col = "steelblue")

grid()