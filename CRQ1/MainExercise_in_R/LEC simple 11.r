# =====================================================
# Bayesian Monte Carlo Loss Model – Portfolio Comparison
# =====================================================

set.seed(42)
library(ggplot2)
library(gtools)   # for combinations()

# ------------------ 1) Inputs ------------------
# Expert estimates (5th, 50th, 95th percentiles)
lower <- 2850000
median_val <- 10000000
upper <- 28500000
prob_event <- 0.2
n_post_draws <- 5000

# Mitigants definition
mitigants <- data.frame(
  Name = c("Firewall", "Backup", "Monitoring"),
  ProbLow = c(0.6, 0.7, 0.8),
  ProbHigh = c(0.9, 0.95, 0.95),
  SevLow = c(0.9, 0.5, 0.7),
  SevHigh = c(1.0, 0.8, 0.9),
  CostLow = c(20000, 30000, 15000),
  CostHigh = c(50000, 70000, 40000),
  stringsAsFactors = FALSE
)

n_mit <- nrow(mitigants)

# ------------------ 2) Lognormal fit for severity ------------------
p_low <- 0.05
p_high <- 0.95
z_low <- qnorm(p_low)
z_high <- qnorm(p_high)
mu_base <- (log(lower) + log(upper)) / 2
sigma_base <- (log(upper) - log(lower)) / (z_high - z_low)

mu_draws <- rnorm(n_post_draws, mean = mu_base, sd = 0.2)
sigma_draws <- abs(rnorm(n_post_draws, mean = sigma_base, sd = 0.1 * sigma_base))

# ------------------ 3) Robust PERT utilities ------------------
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

# ------------------ 4) Base expected loss ------------------
expected_losses <- prob_event * exp(mu_draws + 0.5 * sigma_draws^2)

# ------------------ 5) Generate all mitigant combinations ------------------
combo_list <- list()
combo_names <- c("None")

for (k in 1:n_mit) {
  cmb <- combinations(n_mit, k, seq_len(n_mit))
  for (i in 1:nrow(cmb)) {
    combo_list[[length(combo_list) + 1]] <- cmb[i, ]
    combo_names <- c(combo_names, paste(mitigants$Name[cmb[i, ]], collapse = " + "))
  }
}

# ------------------ 6) Simulate portfolios ------------------
portfolio_results <- list()
roi_df <- data.frame()

for (c_idx in seq_along(combo_names)) {
  if (combo_names[c_idx] == "None") {
    exp_loss_vec <- expected_losses
    cost_vec <- rep(0, n_post_draws)
  } else {
    mitigant_ids <- combo_list[[c_idx - 1]]
    prob_mat <- rpert_multi(n_post_draws,
                            low = mitigants$ProbLow[mitigant_ids],
                            mode = (mitigants$ProbLow[mitigant_ids] + mitigants$ProbHigh[mitigant_ids]) / 2,
                            high = mitigants$ProbHigh[mitigant_ids])
    sev_mat <- rpert_multi(n_post_draws,
                           low = mitigants$SevLow[mitigant_ids],
                           mode = (mitigants$SevLow[mitigant_ids] + mitigants$SevHigh[mitigant_ids]) / 2,
                           high = mitigants$SevHigh[mitigant_ids])
    cost_mat <- rpert_multi(n_post_draws,
                            low = mitigants$CostLow[mitigant_ids],
                            mode = (mitigants$CostLow[mitigant_ids] + mitigants$CostHigh[mitigant_ids]) / 2,
                            high = mitigants$CostHigh[mitigant_ids])
    
    prob_factor <- apply(prob_mat, 1, prod)
    sev_factor  <- apply(sev_mat, 1, prod)
    cost_vec <- rowSums(cost_mat)
    exp_loss_vec <- expected_losses * prob_factor * sev_factor
  }
  
  loss_reduction <- expected_losses - exp_loss_vec
  roi_vec <- ifelse(cost_vec > 0, loss_reduction / cost_vec, 0)
  
  portfolio_results[[combo_names[c_idx]]] <- list(
    exp_loss = exp_loss_vec,
    cost = cost_vec,
    roi = roi_vec
  )
  
  roi_df <- rbind(roi_df, data.frame(Portfolio = combo_names[c_idx], ROI = roi_vec))
}

# ------------------ 7) Violin plot of ROI by portfolio ------------------
ggplot(roi_df, aes(x = Portfolio, y = ROI, fill = Portfolio)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  theme_minimal(base_size = 13) +
  labs(title = "ROI Distributions by Mitigant Portfolio",
       subtitle = "Bayesian Monte Carlo with Uncertain Effectiveness & Cost",
       y = "ROI (Expected Loss Reduction / Cost)") +
  coord_flip() +
  theme(legend.position = "none")

# ------------------ 8) Summary statistics ------------------
roi_summary <- aggregate(ROI ~ Portfolio, data = roi_df,
                         FUN = function(x) c(median = median(x), p25 = quantile(x, 0.25), p75 = quantile(x, 0.75)))
roi_summary <- do.call(data.frame, roi_summary)
print(roi_summary)
