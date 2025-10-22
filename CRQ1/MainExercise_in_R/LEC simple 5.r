set.seed(123)

# --- Base risk inputs ---
lower <- 2850000     
upper <- 28500000
prob_event <- 0.025  # probability of occurrence
n_sim <- 100000

p_low <- 0.05
p_high <- 0.95
z_width <- qnorm(p_high) - qnorm(p_low)

mu_base <- (log(lower) + log(upper)) / 2
sigma_base <- (log(upper) - log(lower)) / z_width

# --- Mitigant definitions ---
mitigants <- data.frame(
  Name = c("Firewall", "Backup", "Monitoring", "Training"),
  Reduction = c(0.80, 0.50, 0.30, 0.10),           # reduces event probability
  SeverityReduction = c(0.00, 0.50, 0.20, 0.10),   # reduces severity
  Cost = c(40000, 60000, 25000, 15000)             # annual costs ($)
)

# --- Scenarios: Base, Individual Mitigants, All Combined ---
scenarios <- data.frame(
  Scenario = c("Base", mitigants$Name, "All Combined"),
  Prob = NA,
  SeverityScale = NA
)

scenarios$Prob[1] <- prob_event
scenarios$SeverityScale[1] <- 1

for (i in 1:nrow(mitigants)) {
  scenarios$Prob[i + 1] <- prob_event * (1 - mitigants$Reduction[i])
  scenarios$SeverityScale[i + 1] <- 1 - mitigants$SeverityReduction[i]
}

scenarios$Prob[nrow(scenarios)] <- prob_event * prod(1 - mitigants$Reduction)
scenarios$SeverityScale[nrow(scenarios)] <- prod(1 - mitigants$SeverityReduction)

# --- Analytical expected losses ---
expected_losses <- numeric(nrow(scenarios))
for (i in seq_len(nrow(scenarios))) {
  mu <- log(scenarios$SeverityScale[i]) + mu_base
  sigma <- sigma_base
  prob <- scenarios$Prob[i]
  expected_losses[i] <- prob * exp(mu + 0.5 * sigma^2)
}

# --- ROI and cost-benefit calculations ---
base_loss <- expected_losses[1]
mitigants$ExpectedLoss <- expected_losses[-c(1, nrow(scenarios))]  # exclude base + combined
mitigants$LossReduction <- base_loss - mitigants$ExpectedLoss
mitigants$ROI <- mitigants$LossReduction / mitigants$Cost
mitigants$NetBenefit <- mitigants$LossReduction - mitigants$Cost

# --- Print results ---
cat("\n=== Expected Loss per Scenario ===\n")
print(data.frame(scenarios$Scenario, round(expected_losses, 2)))

cat("\n=== Mitigant Cost-Benefit Analysis ===\n")
print(mitigants[, c("Name", "Cost", "LossReduction", "ROI", "NetBenefit")])

# --- Visualization: ROI bar plot ---
bar_colors <- ifelse(mitigants$NetBenefit > 0, "darkgreen", "firebrick")

barplot(
  mitigants$NetBenefit,
  names.arg = mitigants$Name,
  col = bar_colors,
  main = "Mitigant Cost-Benefit (Net Benefit)",
  ylab = "Net Benefit ($)",
  las = 2
)
abline(h = 0, lwd = 2)

grid()
