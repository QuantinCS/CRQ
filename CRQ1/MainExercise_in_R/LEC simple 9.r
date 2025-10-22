# Bayesian LEC + uncertain mitigant effectiveness + stochastic costs + violin ROI plots
set.seed(123)

# ------------------ 1) Expert percentile inputs ------------------
q_low  <- 2850000
q_med  <- 10000000
q_high <- 28500000
p_vec <- c(0.05, 0.50, 0.95)
log_q_obs <- log(c(q_low, q_med, q_high))
z_p <- qnorm(p_vec)

# MCMC setup
obs_sd <- 0.15
n_iter <- 12000; burn <- 4000; thin <- 2
mean0 <- mean(log_q_obs); sd0 <- 5
sd_logsig0 <- 1.5
lm_fit <- lm(log_q_obs ~ z_p)
mu_init <- coef(lm_fit)[1]; sigma_init <- abs(coef(lm_fit)[2]); logsig_init <- log(sigma_init)
prop_sd_mu <- 0.02; prop_sd_logsig <- 0.02

logpost <- function(mu, logsig) {
  sigma <- exp(logsig)
  ll <- sum(dnorm(log_q_obs, mean = mu + sigma * z_p, sd = obs_sd, log = TRUE))
  lp_mu <- dnorm(mu, mean0, sd0, log = TRUE)
  lp_logsig <- dnorm(logsig, 0, sd_logsig0, log = TRUE)
  ll + lp_mu + lp_logsig
}

# Run MCMC
n_store <- floor((n_iter - burn) / thin)
mu_samples <- numeric(n_store); sig_samples <- numeric(n_store)
mu_curr <- mu_init; logsig_curr <- logsig_init; logpost_curr <- logpost(mu_curr, logsig_curr)
accept <- 0; store_idx <- 0

for (iter in 1:n_iter) {
  mu_prop <- rnorm(1, mu_curr, prop_sd_mu)
  logsig_prop <- rnorm(1, logsig_curr, prop_sd_logsig)
  logpost_prop <- logpost(mu_prop, logsig_prop)
  if (log(runif(1)) < (logpost_prop - logpost_curr)) {
    mu_curr <- mu_prop; logsig_curr <- logsig_prop; logpost_curr <- logpost_prop; accept <- accept + 1
  }
  if (iter > burn && ((iter - burn) %% thin == 0)) {
    store_idx <- store_idx + 1
    mu_samples[store_idx] <- mu_curr
    sig_samples[store_idx] <- exp(logsig_curr)
  }
}
cat(sprintf("MCMC acceptance rate: %.3f\n", accept / n_iter))

# ------------------ 2) Mitigants: include cost uncertainty ------------------
mitigants <- data.frame(
  Name = c("Firewall", "Backup"),
  ProbLow = c(0.70, 0.40),
  ProbHigh = c(0.90, 0.70),
  SevLow = c(0.00, 0.30),
  SevHigh = c(0.00, 0.60),
  CostLow = c(30000, 50000),
  CostHigh = c(50000, 80000),
  stringsAsFactors = FALSE
)
n_mit <- nrow(mitigants)

# ------------------ 3) PERT sampling ------------------
rpert <- function(n, min, mode, max, lambda = 4) {
  if (min == max) return(rep(min, n))
  alpha <- 1 + lambda * (mode - min) / (max - min)
  beta  <- 1 + lambda * (max - mode) / (max - min)
  u <- rbeta(n, alpha, beta)
  min + u * (max - min)
}
pert_sample_bounds <- function(n, low, high, mode = NULL, lambda = 4) {
  if (is.null(mode)) mode <- (low + high) / 2
  rpert(n, low, mode, high, lambda)
}

# ------------------ 4) Posterior predictive sampling ------------------
ns_post <- length(mu_samples)
n_post_draws <- min(ns_post, 2000)
sel_idx <- sample(1:ns_post, n_post_draws, replace = FALSE)

loss_values <- exp(seq(log(q_low/5), log(q_high*5), length.out = 300))
scenarios <- c("Base", mitigants$Name, "All Combined")
n_scen <- length(scenarios)

pred_exceed <- array(NA, dim = c(n_post_draws, length(loss_values), n_scen))
exp_loss_post <- matrix(NA, nrow = n_post_draws, ncol = n_scen)
net_benefit_post <- matrix(NA, nrow = n_post_draws, ncol = n_scen)

for (j in seq_along(sel_idx)) {
  i_post <- sel_idx[j]
  mu_j <- mu_samples[i_post]
  sigma_j <- sig_samples[i_post]

  # sample mitigant parameters
  prob_red_samps <- numeric(n_mit)
  sev_red_samps  <- numeric(n_mit)
  cost_samps     <- numeric(n_mit)
  for (m in 1:n_mit) {
    prob_red_samps[m] <- pert_sample_bounds(1, mitigants$ProbLow[m], mitigants$ProbHigh[m])
    sev_red_samps[m]  <- pert_sample_bounds(1, mitigants$SevLow[m], mitigants$SevHigh[m])
    cost_samps[m]     <- pert_sample_bounds(1, mitigants$CostLow[m], mitigants$CostHigh[m])
  }

  # base
  p_base <- 0.2
  mu_base <- mu_j
  pred_exceed[j, , 1] <- p_base * (1 - pnorm((log(loss_values) - mu_base) / sigma_j))
  exp_loss_post[j, 1] <- p_base * exp(mu_base + 0.5 * sigma_j^2)

  # individual mitigants
  for (m in seq_len(n_mit)) {
    p_s <- p_base * (1 - prob_red_samps[m])
    scale_s <- (1 - sev_red_samps[m])
    mu_s <- mu_j + log(scale_s)
    pred_exceed[j, , 1 + m] <- p_s * (1 - pnorm((log(loss_values) - mu_s) / sigma_j))
    exp_loss_post[j, 1 + m] <- p_s * exp(mu_s + 0.5 * sigma_j^2)
    loss_reduction <- exp_loss_post[j, 1] - exp_loss_post[j, 1 + m]
    net_benefit_post[j, 1 + m] <- loss_reduction - cost_samps[m]
  }

  # combined
  p_comb <- p_base * prod(1 - prob_red_samps)
  scale_comb <- prod(1 - sev_red_samps)
  mu_comb <- mu_j + log(scale_comb)
  pred_exceed[j, , n_scen] <- p_comb * (1 - pnorm((log(loss_values) - mu_comb) / sigma_j))
  exp_loss_post[j, n_scen] <- p_comb * exp(mu_comb + 0.5 * sigma_j^2)
  loss_reduction_comb <- exp_loss_post[j, 1] - exp_loss_post[j, n_scen]
  net_benefit_post[j, n_scen] <- loss_reduction_comb - sum(cost_samps)
}

# ------------------ 5) Summaries ------------------
pred_med <- apply(pred_exceed, c(2,3), median)
pred_lo  <- apply(pred_exceed, c(2,3), function(x) quantile(x, 0.05))
pred_hi  <- apply(pred_exceed, c(2,3), function(x) quantile(x, 0.95))

par(mar = c(5,4,4,2)+0.1)
cols <- c("black", "red", "blue", "darkgreen", "orange")[1:n_scen]
plot(NULL, xlim = range(loss_values), ylim = c(0, max(pred_hi, na.rm = TRUE)),
     log = "x", xlab = "Loss amount", ylab = "Probability of exceedance",
     main = "Bayesian predictive LECs with stochastic mitigant effects")
for (s in 1:n_scen) {
  x <- loss_values; ylo <- pred_lo[, s]; yhi <- pred_hi[, s]
  polygon(c(x, rev(x)), c(ylo, rev(yhi)), col = adjustcolor(cols[s], alpha.f = 0.15), border = NA)
  lines(x, pred_med[, s], col = cols[s], lwd = 2)
}
legend("topright", legend = scenarios, col = cols, lwd = 2, bty = "n")
grid()

# Expected losses
exp_loss_summary <- data.frame(
  Scenario = scenarios,
  Median = apply(exp_loss_post, 2, median),
  Low90 = apply(exp_loss_post, 2, function(x) quantile(x, 0.05)),
  High90 = apply(exp_loss_post, 2, function(x) quantile(x, 0.95))
)
print(exp_loss_summary)

# ------------------ 6) ROI distributions and violin plots ------------------
# ROI = (loss reduction) / cost
library(ggplot2)
roi_df <- data.frame()

for (m in 1:n_mit) {
  loss_reduction_draws <- exp_loss_post[,1] - exp_loss_post[,1 + m]
  # cost draws: sample again to get same dimension
  cost_draws <- pert_sample_bounds(n_post_draws, mitigants$CostLow[m], mitigants$CostHigh[m])
  roi_draws <- loss_reduction_draws / cost_draws
  roi_df <- rbind(roi_df, data.frame(Mitigant = mitigants$Name[m], ROI = roi_draws))
}

ggplot(roi_df, aes(x = Mitigant, y = ROI, fill = Mitigant)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed", size = 1) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 3, fill = "blue") +
  labs(title = "ROI distributions with stochastic costs",
       subtitle = "Red dashed line = breakeven (ROI=1)",
       y = "ROI (loss reduction / cost)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
