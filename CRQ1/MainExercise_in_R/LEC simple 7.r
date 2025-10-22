# Bayesian fit from 3 expert percentiles -> posterior on lognormal (mu, sigma)
# Then produce posterior-predictive Loss Exceedance Curves for scenarios with mitigants

set.seed(123)

# ----------- User inputs -------------
# Expert percentiles (interpret these reported values as quantiles)
q_low   <- 2850000    # e.g. 5th percentile reported by expert
q_med   <- 10000000    # e.g. median (50%)
q_high  <- 28500000    # e.g. 95th percentile

# Which probabilities those numbers represent (can change if different)
p_vec <- c(0.05, 0.50, 0.95)   # default: 5%, 50%, 95%
q_obs <- c(q_low, q_med, q_high)

# Observation noise on log-quantiles (elicitation uncertainty).
# Smaller -> expert numbers treated as precise; larger -> more uncertain.
# A reasonable starting value: 0.1 - 0.3 (log-space). Adjust as needed.
obs_sd <- 0.15

# Monte Carlo / MCMC tuning
n_iter <- 12000
burn   <- 4000
thin   <- 2

# Base event probability
prob_event <- 0.2

# Mitigants example (probability reductions and severity scalers)
mitigants <- data.frame(
  Name = c("Base", "Firewall", "Backup", "All Combined"),
  ProbReduction = c(0.0, 0.80, 0.50, NA),    # fractional reduction of prob (NA for combined computed below)
  SeverityReduction = c(0.0, 0.00, 0.50, NA) # fractional reduction (multiplicative)
)
# compute combined (independent) for last row
mitigants$ProbReduction[nrow(mitigants)] <- prod(1 - mitigants$ProbReduction[-nrow(mitigants)]) * 0  # placeholder
# Instead calculate scenarios separately below

# ----------- Precompute z-scores ----------
z_p <- qnorm(p_vec)

# log-observed quantiles
log_q_obs <- log(q_obs)

# ----------- MCMC: Metropolis-Hastings on (mu, log_sigma) -------------
# Priors:
# mu ~ Normal(mean0, sd0)
# log_sigma ~ Normal(0, sd_logsig0)  (implies weak prior on sigma)
mean0 <- mean(log_q_obs)    ; sd0 <- 5
sd_logsig0 <- 1.5

# initial values (method of moments-ish)
# Use simple estimate: fit linear regression of log(q_obs) = mu + sigma * z
lm_fit <- lm(log_q_obs ~ z_p)
mu_init <- coef(lm_fit)[1]
sigma_init <- abs(coef(lm_fit)[2])
logsig_init <- log(sigma_init)

# proposal sds (tune if acceptance rate too low/high)
prop_sd_mu <- 0.02
prop_sd_logsig <- 0.02

# log-posterior function (up to additive constant)
logpost <- function(mu, logsig) {
  sigma <- exp(logsig)
  # likelihood of observed log-quantiles
  ll <- sum(dnorm(log_q_obs, mean = mu + sigma * z_p, sd = obs_sd, log = TRUE))
  # priors
  lp_mu <- dnorm(mu, mean0, sd0, log = TRUE)
  lp_logsig <- dnorm(logsig, 0, sd_logsig0, log = TRUE)
  ll + lp_mu + lp_logsig
}

# MCMC storage
n_store <- floor((n_iter - burn) / thin)
mu_samples <- numeric(n_store)
sig_samples <- numeric(n_store)

mu_curr <- mu_init
logsig_curr <- logsig_init
logpost_curr <- logpost(mu_curr, logsig_curr)

accept <- 0
store_idx <- 0

for (iter in 1:n_iter) {
  # propose
  mu_prop <- rnorm(1, mu_curr, prop_sd_mu)
  logsig_prop <- rnorm(1, logsig_curr, prop_sd_logsig)
  logpost_prop <- logpost(mu_prop, logsig_prop)
  
  # MH accept
  if (log(runif(1)) < (logpost_prop - logpost_curr)) {
    mu_curr <- mu_prop
    logsig_curr <- logsig_prop
    logpost_curr <- logpost_prop
    accept <- accept + 1
  }
  
  # store after burn and thinning
  if (iter > burn && ((iter - burn) %% thin == 0)) {
    store_idx <- store_idx + 1
    mu_samples[store_idx] <- mu_curr
    sig_samples[store_idx] <- exp(logsig_curr)
  }
}

cat(sprintf("MCMC acceptance rate: %.3f\n", accept / n_iter))

# Quick posterior summaries
post_summary <- data.frame(
  mu_mean = mean(mu_samples),
  mu_med  = median(mu_samples),
  sigma_mean = mean(sig_samples),
  sigma_med  = median(sig_samples)
)
print(post_summary)

# ----------- Posterior-predictive LECs --------------
# Loss grid (log spaced)
loss_values <- exp(seq(log(q_low/5), log(q_high*5), length.out = 300))

# We'll compute predictive exceedance for each posterior sample and summarize (median + 90% credible band)
ns_post <- length(mu_samples)
# For speed, sample up to n_post_draws (if ns_post large)
n_post_draws <- min(ns_post, 2000)
sel_idx <- sample(1:ns_post, n_post_draws, replace = FALSE)

# Define scenarios: base + individual mitigants + combined (independent)
# Here define separate scenario table
m_df <- data.frame(
  Scenario = c("Base", "Firewall", "Backup", "All Combined"),
  Prob = NA,
  SeverityScale = NA,
  stringsAsFactors = FALSE
)
m_df$Prob[1] <- prob_event
m_df$SeverityScale[1] <- 1
m_df$Prob[2] <- prob_event * (1 - 0.80); m_df$SeverityScale[2] <- 1 - 0.00
m_df$Prob[3] <- prob_event * (1 - 0.50); m_df$SeverityScale[3] <- 1 - 0.50
m_df$Prob[4] <- prob_event * prod(1 - c(0.80, 0.50)); m_df$SeverityScale[4] <- prod(1 - c(0.00, 0.50))

# storage for predictive exceedance probabilities (samples x losses x scenarios)
pred_exceed <- array(NA, dim = c(n_post_draws, length(loss_values), nrow(m_df)))

for (j in seq_along(sel_idx)) {
  i_post <- sel_idx[j]
  mu_j <- mu_samples[i_post]
  sigma_j <- sig_samples[i_post]
  for (s in 1:nrow(m_df)) {
    mu_s <- mu_j + log(m_df$SeverityScale[s])   # multiplicative severity scaling
    p_s  <- m_df$Prob[s]
    # predictive exceedance at each L given mu_s, sigma_j
    pe <- p_s * (1 - pnorm((log(loss_values) - mu_s) / sigma_j))
    pred_exceed[j, , s] <- pe
  }
}

# Summarize: median and 5%-95% bands across posterior draws
pred_med <- apply(pred_exceed, c(2,3), median)
pred_lo  <- apply(pred_exceed, c(2,3), function(x) quantile(x, 0.05))
pred_hi  <- apply(pred_exceed, c(2,3), function(x) quantile(x, 0.95))

# ----------- Plot median curve with credible band for each scenario -----------
cols <- c("black", "red", "blue", "darkgreen")
plot(NULL, xlim = range(loss_values), ylim = c(0, max(pred_hi)), log = "x",
     xlab = "Loss amount", ylab = "Probability of exceedance",
     main = "Bayesian posterior-predictive Loss Exceedance Curves\n(Median and 90% credible bands)")

for (s in 1:nrow(m_df)) {
  # credible ribbon: polygon between pred_lo and pred_hi
  x <- loss_values
  ylo <- pred_lo[, s]
  yhi <- pred_hi[, s]
  # draw ribbon (transparent-ish)
  polygon(c(x, rev(x)), c(ylo, rev(yhi)), col = adjustcolor(cols[s], alpha.f = 0.15), border = NA)
  # median line
  lines(x, pred_med[, s], col = cols[s], lwd = 2)
}
legend("topright", legend = paste0(m_df$Scenario, " (p=", signif(m_df$Prob,3), 
                                   ", scale=", signif(m_df$SeverityScale,3), ")"),
       col = cols, lwd = 2, bty = "n")
grid()

# ----------- Optional: Compare to fit-from-3points analytic (point estimate) -----------
# Fit simple lognormal via regression (as earlier) for comparison
lm_fit <- lm(log_q_obs ~ z_p)
mu_point <- coef(lm_fit)[1]
sigma_point <- abs(coef(lm_fit)[2])
# analytic exceedance for point estimate (base scenario)
anal_point <- prob_event * (1 - pnorm((log(loss_values) - mu_point) / sigma_point))
lines(loss_values, anal_point, col = "orange", lwd = 2, lty = 2)
legend("bottomleft", legend = c("Point-fit analytic (dashed orange)"),
       col = c("orange"), lwd = 2, lty = 2, bty = "n")

# ----------- Posterior expected losses (per scenario) -----------
# For each posterior sample compute expected loss = p * exp(mu + 0.5 sigma^2)
exp_loss_post <- matrix(NA, nrow = n_post_draws, ncol = nrow(m_df))
for (j in seq_along(sel_idx)) {
  i_post <- sel_idx[j]
  mu_j <- mu_samples[i_post]
  sigma_j <- sig_samples[i_post]
  for (s in 1:nrow(m_df)) {
    mu_s <- mu_j + log(m_df$SeverityScale[s])
    p_s <- m_df$Prob[s]
    exp_loss_post[j, s] <- p_s * exp(mu_s + 0.5 * sigma_j^2)
  }
}
exp_loss_summary <- data.frame(
  Scenario = m_df$Scenario,
  ExpLoss_Median = apply(exp_loss_post, 2, median),
  ExpLoss_Lower5 = apply(exp_loss_post, 2, function(x) quantile(x, 0.05)),
  ExpLoss_Upper95 = apply(exp_loss_post, 2, function(x) quantile(x, 0.95))
)
print(exp_loss_summary)
