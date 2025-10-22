# Input: Lower and upper 90% estimates (5th and 95th percentiles)
lower <- 2850000     
upper <- 28500000
prob_event <- 0.025  # probability of occurrence

# Constant: width of 90% CI in z-scores
#z_width <- 3.28971

p_low <- 0.05
p_high <- 0.95
z_width <- qnorm(p_high) - qnorm(p_low)

# --- Derive lognormal parameters ---
mu <- (log(lower) + log(upper)) / 2
sigma <- (log(upper) - log(lower)) / z_width

# --- Define mitigants ---
# Each mitigant reduces the probability by a proportion
mitigants <- data.frame(
  Name = c("Firewall", "Backup", "Monitoring", "Training"),
  Reduction = c(0.80, 0.50, 0.30, 0.10)  # e.g. Firewall reduces prob by 80%
)

# --- Compute combined mitigation effects ---
# Individual mitigants: apply each separately
scenarios <- data.frame(
  Scenario = c("Base", mitigants$Name, "All Combined"),
  Prob = c(
    prob_event,
    prob_event * (1 - mitigants$Reduction),
    prob_event * prod(1 - mitigants$Reduction)
  )
)

print(scenarios)

# --- Prepare loss values for exceedance curve ---
loss_values <- exp(seq(log(lower/5), log(upper*5), length.out = 300))

# --- Compute exceedance probabilities for each scenario ---
exceed_curves <- lapply(scenarios$Prob, function(p) {
  p * (1 - pnorm((log(loss_values) - mu) / sigma))
})

# --- Plot results ---
plot(loss_values, exceed_curves[[1]], type = "l", log = "x",
     lwd = 2, col = 1,
     xlab = "Loss amount", ylab = "Probability of exceedance",
     main = "Loss Exceedance Curves with Mitigation")

for (i in 2:length(exceed_curves)) {
  lines(loss_values, exceed_curves[[i]], col = i, lwd = 2)
}

legend("topright", legend = scenarios$Scenario, col = 1:length(exceed_curves),
       lwd = 2, bty = "n")

grid()
