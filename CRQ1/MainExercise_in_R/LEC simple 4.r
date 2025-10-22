set.seed(123)  # reproducibility

# --- Input parameters ---
# Input: Lower and upper 90% estimates (5th and 95th percentiles)
lower <- 2850000     
upper <- 28500000
prob_event <- 0.025  # probability of occurrence
n_sim <- 100000    # number of Monte Carlo iterations

# --- Lognormal parameters ---
p_low <- 0.05
p_high <- 0.95
z_width <- qnorm(p_high) - qnorm(p_low)

mu_base <- (log(lower) + log(upper)) / 2
sigma_base <- (log(upper) - log(lower)) / z_width

# --- Define mitigants ---
mitigants <- data.frame(
  Name = c("Firewall", "Backup", "Monitoring", "Training"),
  Reduction = c(0.80, 0.50, 0.30, 0.10),
  SeverityReduction = c(0.00, 0.50, 0.20, 0.10)
)

# --- Define scenarios (Base + mitigants + All Combined) ---
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

print(scenarios)

# --- Analytical exceedance curve setup ---
loss_values <- exp(seq(log(lower/5), log(upper*5), length.out = 300))
analytical_curves <- list()
expected_losses <- numeric(nrow(scenarios))

# --- Monte Carlo storage ---
sim_results <- list()

for (i in seq_len(nrow(scenarios))) {
  mu <- log(scenarios$SeverityScale[i]) + mu_base
  sigma <- sigma_base
  prob <- scenarios$Prob[i]
  
  # Analytical exceedance (for comparison)
  analytical_curves[[i]] <- prob * (1 - pnorm((log(loss_values) - mu) / sigma))
  expected_losses[i] <- prob * exp(mu + 0.5 * sigma^2)
  
  # Monte Carlo simulation
  event_occurs <- rbinom(n_sim, size = 1, prob = prob)
  severity <- rlnorm(n_sim, meanlog = mu, sdlog = sigma)
  total_loss <- event_occurs * severity
  
  sim_results[[i]] <- total_loss
}

# --- Compute empirical exceedance curves from simulation ---
empirical_curves <- lapply(sim_results, function(losses) {
  sapply(loss_values, function(L) mean(losses > L))
})

# --- Plot results ---
plot(loss_values, analytical_curves[[1]], type = "l", log = "x",
     lwd = 2, col = "black",
     xlab = "Loss amount", ylab = "Probability of exceedance",
     main = "Monte Carlo Loss Exceedance Curves with Mitigation")

cols <- c("black", "red", "blue", "darkgreen", "orange", "purple")
for (i in seq_len(nrow(scenarios))) {
  lines(loss_values, analytical_curves[[i]], col = cols[i], lwd = 2, lty = 1)
  lines(loss_values, empirical_curves[[i]], col = cols[i], lwd = 2, lty = 2)
}

legend("topright", legend = paste0(
  scenarios$Scenario, " (E[L] = ",
  formatC(expected_losses, format = "f", digits = 0, big.mark = ","), ")"
),
col = cols[1:nrow(scenarios)], lwd = 2, lty = 1, bty = "n", title = "Analytical (solid) / Simulation (dashed)")

grid()
