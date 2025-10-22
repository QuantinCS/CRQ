# =====================================================
# Bayesian Monte Carlo Loss Model with Mitigants & ROI
# =====================================================

set.seed(42)

# ------------------ 1) Inputs ------------------
# Three-point expert estimates (treated as 5th/50th/95th percentiles)
lower <- 2850000
median_val <- 10000000
upper <- 28500000
prob_event <- 0.025
n_post_draws <- 5000

# Mitigants definition
mitigants <- data.frame(
  Name = c("Firewall", "Backup", "Monitoring"),
  Active = c(TRUE, TRUE, FALSE),
  ProbLow = c(0.6, 0.7, 0.8),   # lower bound for probability reduction factor
  ProbHigh = c(0.9, 0.95, 0.95),
  SevLow = c(0.9, 0.5, 0.7),    # severity reduction factor (lower = better)
  SevHigh = c(1.0, 0.8, 0.9),
  CostLow = c(20000, 30000, 15000),
  CostHigh = c(50000, 70000, 40000),
  stringsAsFactors = FALSE
)

# Filter only active mitigants
active_mitigants <- mitigants[mitigants$Active, ]
n_mit <- nrow(active_mitigants)

# ------------------ 2) Lognormal fit (Bayesian style) ------------------
# We'll treat the three expert points as uncertain inputs, drawing mu/sigma per iteration.

# Percentiles
p_low <- 0.05
p_med <- 0.5
p_high <- 0.95

# Calculate z-values
z_low <- qnorm(p_low)
z_high <- qnorm(p_high)

# derive base mu/sigma with slight noise to reflect expert uncertainty
mu_base <- (log(lower) + log(upper)) / 2
sigma_base <- (log(upper) - log(lower)) / (z_high - z_low)

mu_draws <- rnorm(n_post_draws, mean = mu_base, sd = 0.2)
sigma_draws <- rnorm(n_post_draws, mean = sigma_base, sd = 0.1 * sigma_base)

# ------------------ 3) Robust, vectorized PERT utilities ------------------
rpert_single <- function(n, min, mode, max, lambda = 4) {
  if (min == max) return(rep(min, n))
  alpha <- 1 + lambda * (mode - min) / (max - min)
  beta  <- 1 + lambda * (max - mode) / (max - min)
  u <- rbeta(n, alpha, beta)
  min + u * (max - min)
}

rpert_multi <- function(n, low, mode = NULL, high, lambda = 4) {
  low <- as.numeric(low); high <- as.numeric(high)
  k <- length(low)
  if (is.null(mode)) mode <- (low + high) / 2
  mode <- as.numeric(mode)
  if (length(mode) == 1) mode <- rep(mode, k)
  out <- matrix(NA_real_, nrow = n, ncol = k)
  for (j in seq_len(k)) {
    out[, j] <- rpert_single(n, low[j], mode[j], high[j], lambda = lambda)
  }
  colnames(out) <- if (!is.null(names(low))) names(low) else paste0("V", seq_len(k))
  out
}

# ------------------ 4) Monte Carlo simulation ------------------
expected_losses <- list()

for (i in 1:n_post_draws) {
  mu <- mu_draws[i]
  sigma <- abs(sigma_draws[i])
  expected_losses[[i]] <- prob_event * exp(mu + 0.5 * sigma^2)
}

expected_losses <- unlist(expected_losses)
exp_loss_post <- data.frame(Base = expected_losses)

# Generate mitigated expected losses
for (m in seq_len(n_mit)) {
  prob_factor <- rpert_single(n_post_draws,
                              min = active_mitigants$ProbLow[m],
                              mode = (active_mitigants$ProbLow[m] + active_mitigants$ProbHigh[m]) / 2,
                              max = active_mitigants$ProbHigh[m])
  sev_factor <- rpert_single(n_post_draws,
                             min = active_mitigants$SevLow[m],
                             mode = (active_mitigants$SevLow[m] + active_mitigants$SevHigh[m]) / 2,
                             max = active_mitigants$SevHigh[m])
  exp_loss_post[[active_mitigants$Name[m]]] <- expected_losses * prob_factor * sev_factor
}

# Combined scenario
prob_combined <- apply(rpert_multi(n_post_draws,
                                   low = active_mitigants$ProbLow,
                                   mode = (active_mitigants$ProbLow + active_mitigants$ProbHigh) / 2,
                                   high = active_mitigants$ProbHigh), 1, prod)
sev_combined <- apply(rpert_multi(n_post_draws,
                                  low = active_mitigants$SevLow,
                                  mode = (active_mitigants$SevLow + active_mitigants$SevHigh) / 2,
                                  high = active_mitigants$SevHigh), 1, prod)
exp_loss_post$Combined <- expected_losses * prob_combined * sev_combined

# ------------------ 5) ROI computation ------------------
costs_mat <- rpert_multi(n_post_draws,
                         low = active_mitigants$CostLow,
                         mode = (active_mitigants$CostLow + active_mitigants$CostHigh) / 2,
                         high = active_mitigants$CostHigh)

roi_list <- list()
for (m in seq_len(n_mit)) {
  loss_reduction_vec <- exp_loss_post$Base - exp_loss_post[[active_mitigants$Name[m]]]
  roi_vec <- loss_reduction_vec / costs_mat[, m]
  roi_list[[m]] <- roi_vec
}

# Combined ROI
combined_cost_vec <- rowSums(costs_mat)
loss_reduction_comb <- exp_loss_post$Base - exp_loss_post$Combined
roi_combined_vec <- loss_reduction_comb / combined_cost_vec

# ------------------ 6) Violin plots for ROI ------------------
library(ggplot2)
roi_df <- data.frame(
  Mitigant = rep(active_mitigants$Name, each = n_post_draws),
  ROI = unlist(roi_list)
)
roi_df <- rbind(roi_df, data.frame(Mitigant = "Combined Active Set", ROI = roi_combined_vec))

ggplot(roi_df, aes(x = Mitigant, y = ROI, fill = Mitigant)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  theme_minimal(base_size = 13) +
  labs(title = "ROI Distributions (Bayesian Monte Carlo with Mitigants)",
       subtitle = "Includes combined ROI and per-mitigant uncertainty",
       y = "ROI (Expected loss reduction / Cost)") +
  coord_flip()
