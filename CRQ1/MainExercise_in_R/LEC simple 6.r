# Monte Carlo LEC with 3-point expert prior (Triangular or Beta-PERT)
set.seed(42)

# --- User 3-point expert estimates (min, mode, max) ---
min_est <- 2850000      # lower
mode_est <- 10000000     # most likely
max_est <- 28500000      # upper

# Choose distribution type: "pert" or "triangular"
dist_type <- "pert"    # "pert" or "triangular"
use_log <- TRUE        # TRUE = build distribution on log(loss) (recommended)

# --- PERT sampler (modified PERT; lambda = 4 is common) ---
rpert <- function(n, min, mode, max, lambda = 4) {
  if (min == max) return(rep(min, n))
  # parameters for beta: alpha = 1 + lambda * (mode-min)/(max-min), beta = 1 + lambda * (max-mode)/(max-min)
  alpha <- 1 + lambda * (mode - min) / (max - min)
  beta  <- 1 + lambda * (max - mode) / (max - min)
  u <- rbeta(n, alpha, beta)
  min + u * (max - min)
}

# --- Triangular sampler ---
rtriangle <- function(n, min, mode, max) {
  u <- runif(n)
  c1 <- (mode - min) / (max - min)
  out <- ifelse(u < c1,
                min + sqrt(u * (max - min) * (mode - min)),
                max - sqrt((1 - u) * (max - min) * (max - mode)))
  out
}

# --- Sampling function that returns severity draws ---
r_severity <- function(n_draws, min, mode, max, dist_type = "pert", use_log = TRUE) {
  if (use_log) {
    lo <- log(min); mo <- log(mode); hi <- log(max)
    if (dist_type == "pert") {
      samp_log <- rpert(n_draws, lo, mo, hi)
    } else {
      samp_log <- rtriangle(n_draws, lo, mo, hi)
    }
    exp(samp_log)
  } else {
    if (dist_type == "pert") {
      rpert(n_draws, min, mode, max)
    } else {
      rtriangle(n_draws, min, mode, max)
    }
  }
}

# Quick check: visualize the prior vs log-prior
library(graphics)
n_check <- 50000
samp1 <- r_severity(n_check, min_est, mode_est, max_est, dist_type = dist_type, use_log = TRUE)
samp2 <- r_severity(n_check, min_est, mode_est, max_est, dist_type = dist_type, use_log = FALSE)

par(mfrow = c(2,1))
hist(samp1, breaks = 60, main = "Prior on original scale (log-pert)", xlab = "Loss")
hist(log(samp1), breaks = 60, main = "Prior on log scale (log-loss)", xlab = "log(Loss)")
par(mfrow = c(1,1))

# --- Monte Carlo LEC setup (scenarios & mitigants) ---
set.seed(123)
prob_event <- 0.2
n_sim <- 100000

# Example mitigants (same structure as earlier)
mitigants <- data.frame(
  Name = c("Firewall", "Backup"),
  Reduction = c(0.8, 0.5),
  SeverityReduction = c(0.0, 0.5)
)

# Build scenarios: Base, each mitigant individually, All Combined
scenarios <- data.frame(
  Scenario = c("Base", mitigants$Name, "All Combined"),
  Prob = NA,
  SeverityScale = NA
)
scenarios$Prob[1] <- prob_event; scenarios$SeverityScale[1] <- 1
for (i in seq_len(nrow(mitigants))) {
  scenarios$Prob[i+1] <- prob_event * (1 - mitigants$Reduction[i])
  scenarios$SeverityScale[i+1] <- 1 - mitigants$SeverityReduction[i]
}
scenarios$Prob[nrow(scenarios)] <- prob_event * prod(1 - mitigants$Reduction)
scenarios$SeverityScale[nrow(scenarios)] <- prod(1 - mitigants$SeverityReduction)

print(scenarios)

# --- Simulate for each scenario using the prior (triangular/PERT) ---
loss_values <- exp(seq(log(min_est/5), log(max_est*5), length.out = 300))

sim_results <- list()
analytical_curves <- list()
empirical_curves <- list()

for (i in seq_len(nrow(scenarios))) {
  prob <- scenarios$Prob[i]
  sev_scale <- scenarios$SeverityScale[i]
  
  # For severity reduction, scale the sampled draws (multiplicative)
  # We sample severity from the expert prior and then multiply by severity scale:
  severity_draws <- r_severity(n_sim, min_est, mode_est, max_est, dist_type = dist_type, use_log = use_log) * sev_scale
  
  # Whether event occurs this trial
  event_occurs <- rbinom(n_sim, 1, prob)
  total_loss <- event_occurs * severity_draws
  sim_results[[i]] <- total_loss
  
  # Empirical exceedance
  empirical_curves[[i]] <- sapply(loss_values, function(L) mean(total_loss > L))
  
  # Analytical approximation: if we wanted a closed form we'd fit a lognormal to the prior draws:
  # fit lognormal mean/sd from the prior draws (approximate analytic curve for comparison)
  mu_fit <- mean(log(severity_draws[severity_draws>0]))   # careful: severity_draws >0
  sigma_fit <- sd(log(severity_draws[severity_draws>0]))
  analytical_curves[[i]] <- prob * (1 - pnorm((log(loss_values) - mu_fit) / sigma_fit))
}

# --- Plot analytical vs empirical exceedance (solid = analytical approximation, dashed = empirical) ---
cols <- c("black", "red", "blue", "darkgreen", "orange")
plot(loss_values, analytical_curves[[1]], type = "l", log = "x",
     xlab = "Loss amount", ylab = "Probability of exceedance",
     main = "LEC with 3-point Prior (PERT/Triangular); Monte Carlo Empirical",
     lwd = 2, col = cols[1])
for (i in 1:length(analytical_curves)) {
  lines(loss_values, analytical_curves[[i]], col = cols[i], lwd = 2, lty = 1)
  lines(loss_values, empirical_curves[[i]], col = cols[i], lwd = 2, lty = 2)
}
legend("topright", legend = paste0(scenarios$Scenario), col = cols[1:length(analytical_curves)],
       lwd = 2, lty = 1:1, bty = "n", title = "Solid = fit-lognormal analytic, Dashed = MC empirical")
grid()

# --- Show expected losses from sims ---
expected_losses_sim <- sapply(sim_results, mean)
print(data.frame(Scenario = scenarios$Scenario, ExpectedLoss_MC = expected_losses_sim))
