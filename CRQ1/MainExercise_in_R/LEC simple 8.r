# Bayesian LEC + uncertain mitigant effectiveness (PERT priors from [low,high])
set.seed(123)

# ------------------ 1) Expert percentile inputs (observed quantiles) ------------------
q_low  <- 2850000
q_med  <- 10000000
q_high <- 28500000
p_vec <- c(0.05, 0.50, 0.95)
log_q_obs <- log(c(q_low, q_med, q_high))
z_p <- qnorm(p_vec)

# MCMC / elicitation noise
obs_sd <- 0.15
n_iter <- 12000; burn <- 4000; thin <- 2

# Priors & initial estimates (as before)
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

# Run MH MCMC to get posterior on mu, sigma
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

# ------------------ 2) Define mitigants with lower / upper bounds ------------------
# User-configurable: add rows or change bounds
mitigants <- data.frame(
  Name = c("Firewall", "Backup"),
  ProbLow = c(0.70, 0.40),   # lower bound on probability *reduction* (i.e. effectiveness)
  ProbHigh = c(0.90, 0.70),  # upper bound
  SevLow = c(0.00, 0.30),    # lower bound on severity reduction
  SevHigh = c(0.00, 0.60),   # upper bound
  Cost = c(40000, 60000),    # fixed annual cost (can be made stochastic)
  stringsAsFactors = FALSE
)
n_mit <- nrow(mitigants)

# ------------------ 3) PERT sampler (on [min, max]) ------------------
rpert <- function(n, min, mode, max, lambda = 4) {
  if (min == max) return(rep(min, n))
  alpha <- 1 + lambda * (mode - min) / (max - min)
  beta  <- 1 + lambda * (max - mode) / (max - min)
  u <- rbeta(n, alpha, beta)
  min + u * (max - min)
}
# If no explicit mode provided, use midpoint
pert_sample_bounds <- function(n, low, high, mode = NULL, lambda = 4) {
  if (is.null(mode)) mode <- (low + high) / 2
  rpert(n, low, mode, high, lambda)
}

# ------------------ 4) Posterior predictive sampling with mitigant uncertainty ------------------
# Select subset of posterior samples for speed (you may increase)
ns_post <- length(mu_samples)
n_post_draws <- min(ns_post, 2000)
sel_idx <- sample(1:ns_post, n_post_draws, replace = FALSE)

# Simulation grid for exceedance curves
loss_values <- exp(seq(log(q_low/5), log(q_high*5), length.out = 300))

# Storage: we'll create arrays indexed by posterior-draw x loss-value x scenario
# Scenarios: Base, each individual mitigant applied alone, All Combined (sampled jointly)
scenarios <- c("Base", mitigants$Name, "All Combined")
n_scen <- length(scenarios)

pred_exceed <- array(NA, dim = c(n_post_draws, length(loss_values), n_scen))
# For ROI we also compute expected-loss per draw & scenario
exp_loss_post <- matrix(NA, nrow = n_post_draws, ncol = n_scen)
net_benefit_post <- matrix(NA, nrow = n_post_draws, ncol = n_scen) # = loss_reduction - cost

# For base we have no mitigant uncertainty; for others we sample pert effectiveness per posterior draw
for (j in seq_along(sel_idx)) {
  i_post <- sel_idx[j]
  mu_j <- mu_samples[i_post]
  sigma_j <- sig_samples[i_post]
  
  # Sample mitigant effectivenesses for this posterior draw
  # For each mitigant, sample ProbReduction ~ PERT(ProbLow, ProbMid, ProbHigh)
  # and SevReduction ~ PERT(SevLow, SevMid, SevHigh)
  prob_red_samps <- numeric(n_mit)
  sev_red_samps  <- numeric(n_mit)
  for (m in 1:n_mit) {
    prob_red_samps[m] <- pert_sample_bounds(1, mitigants$ProbLow[m], mitigants$ProbHigh[m])
    sev_red_samps[m]  <- pert_sample_bounds(1, mitigants$SevLow[m], mitigants$SevHigh[m])
    # clamp to [0,1]
    prob_red_samps[m] <- max(0, min(1, prob_red_samps[m]))
    sev_red_samps[m]  <- max(0, min(1, sev_red_samps[m]))
  }
  
  # 1) Base scenario
  p_base <- 0.2
  mu_base <- mu_j
  pred_exceed[j, , 1] <- p_base * (1 - pnorm((log(loss_values) - mu_base) / sigma_j))
  exp_loss_post[j, 1] <- p_base * exp(mu_base + 0.5 * sigma_j^2)
  net_benefit_post[j, 1] <- NA  # no cost for base
  
  # 2) Individual mitigants
  for (m in seq_len(n_mit)) {
    p_s <- p_base * (1 - prob_red_samps[m])
    scale_s <- (1 - sev_red_samps[m])
    mu_s <- mu_j + log(scale_s)
    pred_exceed[j, , 1 + m] <- p_s * (1 - pnorm((log(loss_values) - mu_s) / sigma_j))
    exp_loss_post[j, 1 + m] <- p_s * exp(mu_s + 0.5 * sigma_j^2)
    # Loss reduction relative to base for this draw:
    loss_reduction <- exp_loss_post[j, 1] - exp_loss_post[j, 1 + m]
    net_benefit_post[j, 1 + m] <- loss_reduction - mitigants$Cost[m]
  }
  
  # 3) All Combined: sample independent effectivenesses (we already sampled above)
  p_comb <- p_base * prod(1 - prob_red_samps)
  scale_comb <- prod(1 - sev_red_samps)
  mu_comb <- mu_j + log(scale_comb)
  pred_exceed[j, , n_scen] <- p_comb * (1 - pnorm((log(loss_values) - mu_comb) / sigma_j))
  exp_loss_post[j, n_scen] <- p_comb * exp(mu_comb + 0.5 * sigma_j^2)
  # combined cost = sum of costs
  combined_cost <- sum(mitigants$Cost)
  loss_reduction_comb <- exp_loss_post[j, 1] - exp_loss_post[j, n_scen]
  net_benefit_post[j, n_scen] <- loss_reduction_comb - combined_cost
}

# ------------------ 5) Summarize posterior predictive exceedance curves ------------------
pred_med <- apply(pred_exceed, c(2,3), median)
pred_lo  <- apply(pred_exceed, c(2,3), function(x) quantile(x, 0.05))
pred_hi  <- apply(pred_exceed, c(2,3), function(x) quantile(x, 0.95))

# Plot median + 90% credible ribbon for each scenario
par(mar = c(5,4,4,2)+0.1)
cols <- c("black", "red", "blue", "darkgreen", "orange")[1:n_scen]
plot(NULL, xlim = range(loss_values), ylim = c(0, max(pred_hi, na.rm = TRUE)),
     log = "x", xlab = "Loss amount", ylab = "Probability of exceedance",
     main = "Bayesian predictive LECs with uncertain mitigant effectiveness")
for (s in 1:n_scen) {
  x <- loss_values; ylo <- pred_lo[, s]; yhi <- pred_hi[, s]
  polygon(c(x, rev(x)), c(ylo, rev(yhi)), col = adjustcolor(cols[s], alpha.f = 0.15), border = NA)
  lines(x, pred_med[, s], col = cols[s], lwd = 2)
}
legend("topright", legend = scenarios, col = cols, lwd = 2, bty = "n")
grid()

# ------------------ 6) Summarize expected loss & ROI (stochastic) ------------------
# Expected loss summary: median and 90% CI per scenario
exp_loss_summary <- data.frame(
  Scenario = scenarios,
  ExpLoss_Median = apply(exp_loss_post, 2, median),
  ExpLoss_5 = apply(exp_loss_post, 2, function(x) quantile(x, 0.05)),
  ExpLoss_95 = apply(exp_loss_post, 2, function(x) quantile(x, 0.95))
)
print(exp_loss_summary)

# Net benefit summary (for mitigant scenarios only)
nb_summary <- data.frame(
  Scenario = scenarios[-1],  # exclude Base
  NetBen_Median = apply(net_benefit_post[, -1, drop = FALSE], 2, median),
  NetBen_5 = apply(net_benefit_post[, -1, drop = FALSE], 2, function(x) quantile(x, 0.05)),
  NetBen_95 = apply(net_benefit_post[, -1, drop = FALSE], 2, function(x) quantile(x, 0.95))
)
print(nb_summary)

# Also compute probability that NetBenefit > 0 (i.e., cost pays off)
prob_payoff <- colMeans(net_benefit_post[, -1, drop = FALSE] > 0, na.rm = TRUE)
payoff_table <- data.frame(Scenario = scenarios[-1], Prob_NetPositive = prob_payoff)
print(payoff_table)

# ------------------ 7) Optional: display ROI distribution plot per mitigant ------------------
par(mfrow = c(1, n_mit))
for (m in 1:n_mit) {
  # ROI here defined as LossReduction / Cost for each draw
  loss_reduction_draws <- exp_loss_post[,1] - exp_loss_post[,1 + m]
  roi_draws <- loss_reduction_draws / mitigants$Cost[m]
  hist(roi_draws, breaks = 40, main = paste0(mitigants$Name[m], " ROI"), xlab = "ROI", col = "lightgray")
  abline(v = median(roi_draws, na.rm = TRUE), col = "blue", lwd = 2)
  abline(v = 1, col = "red", lwd = 2, lty = 2) # breakeven line
}
par(mfrow = c(1,1))
