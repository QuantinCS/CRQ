# Input: Lower and upper 90% estimates (5th and 95th percentiles)
lower <- 2850000     
upper <- 28500000
prob_event <- 0.025  # probability of occurrence

# Constant: width of 90% CI in z-scores
#z_width <- 3.28971

p_low <- 0.05
p_high <- 0.95
z_width <- qnorm(p_high) - qnorm(p_low)

mu_base <- (log(lower) + log(upper)) / 2
sigma_base <- (log(upper) - log(lower)) / z_width

# --- Define mitigants ---
# Each mitigant has two effects:
# - Reduction: decreases probability of event
# - SeverityReduction: decreases the magnitude of losses
mitigants <- data.frame(
  Name = c("Firewall", "Backup", "Monitoring", "Training"),
  Reduction = c(0.80, 0.50, 0.30, 0.10),           # probability reduction
  SeverityReduction = c(0.00, 0.50, 0.20, 0.10)    # severity reduction
)

# --- Define scenarios ---
# Base + individual mitigants + all combined
scenarios <- data.frame(
  Scenario = c("Base", mitigants$Name, "All Combined"),
  Prob = NA,
  SeverityScale = NA
)

# Base scenario
scenarios$Prob[1] <- prob_event
scenarios$SeverityScale[1] <- 1

# Individual mitigants
for (i in 1:nrow(mitigants)) {
  scenarios$Prob[i + 1] <- prob_event * (1 - mitigants$Reduction[i])
  scenarios$SeverityScale[i + 1] <- 1 - mitigants$SeverityReduction[i]
}

# Combined mitigants (assuming independent effects)
scenarios$Prob[nrow(scenarios)] <- prob_event * prod(1 - mitigants$Reduction)
scenarios$SeverityScale[nrow(scenarios)] <- prod(1 - mitigants$SeverityReduction)

print(scenarios)

# --- Loss values for exceedance curve ---
loss_values <- exp(seq(log(lower/5), log(upper*5), length.out = 300))

# --- Compute exceedance probabilities for each scenario ---
exceed_curves <- list()
expected_losses <- numeric(nrow(scenarios))

for (i in seq_len(nrow(scenarios))) {
  # Adjust severity by scaling the mean (mu)
  mu <- log(scenarios$SeverityScale[i]) + mu_base
  sigma <- sigma_base
  prob <- scenarios$Prob[i]

  exceed_curves[[i]] <- prob * (1 - pnorm((log(loss_values) - mu) / sigma))

  # Expected loss for reference
  expected_losses[i] <- prob * exp(mu + 0.5 * sigma^2)
}

# --- Plot exceedance curves ---
plot(loss_values, exceed_curves[[1]], type = "l", log = "x",
     xlab = "Loss amount", ylab = "Probability of exceedance",
     main = "Loss Exceedance Curves with Probability and Severity Mitigation",
     lwd = 2, col = 1)

for (i in 2:length(exceed_curves)) {
  lines(loss_values, exceed_curves[[i]], col = i, lwd = 2)
}

legend("topright", legend = paste0(
  scenarios$Scenario, " (E[L] = ",
  formatC(expected_losses, format = "f", digits = 0, big.mark = ","), ")"
),
col = 1:length(exceed_curves), lwd = 2, bty = "n")

grid()
