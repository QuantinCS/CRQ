# Input: Lower and upper 90% estimates (5th and 95th percentiles)
lower <- 2850000     
upper <- 28500000
prob_event <- 0.025  # probability of occurrence

# Constant: width of 90% CI in z-scores
#z_width <- 3.28971

p_low <- 0.05
p_high <- 0.95
z_width <- qnorm(p_high) - qnorm(p_low)

# Derive lognormal parameters (mu, sigma)
mu <- (log(lower) + log(upper)) / 2
sigma <- (log(upper) - log(lower)) / z_width

# Expected loss (same as in your expression)
expected_loss <- exp(mu + 0.5 * sigma^2) * prob_event
cat("Expected loss =", round(expected_loss, 2), "\n")

# Create loss values for plotting (log spaced)
loss_values <- exp(seq(log(lower/5), log(upper*5), length.out = 300))

# Compute exceedance probabilities (P(loss > L))
exceed_prob <- prob_event * (1 - pnorm((log(loss_values) - mu) / sigma))

# Plot exceedance curve
plot(loss_values, exceed_prob, type = "l", log = "x",
     xlab = "Loss amount", ylab = "Probability of exceedance",
     main = "Loss Exceedance Curve (Lognormal Model)",
     lwd = 2, col = "steelblue")

grid()
