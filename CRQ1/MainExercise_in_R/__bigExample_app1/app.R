# ===============================================
# Bayesian Monte Carlo Risk Mitigation Simulator
# ===============================================
# Run in RStudio: click "Run App"
# Requires: shiny, ggplot2, gtools
# ===============================================

library(shiny)
library(ggplot2)
library(gtools)

# ---- Helper: Robust PERT Sampling ----
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
  colnames(out) <- paste0("M", seq_len(k))
  out
}

# ---------------- UI ----------------
ui <- fluidPage(
  titlePanel("🧠 Bayesian Monte Carlo Risk Mitigation Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      h4("📊 Severity Distribution"),
      numericInput("low", "5th Percentile (Loss $):", 1e5, step = 1e4),
      numericInput("mid", "50th Percentile (Loss $):", 5e5, step = 1e4),
      numericInput("high", "95th Percentile (Loss $):", 1e6, step = 1e4),
      numericInput("prob_event", "Annual Event Probability:", 0.2, step = 0.05),
      numericInput("n_sim", "Monte Carlo Draws:", 5000, min = 1000, step = 1000),
      hr(),
      h4("🧩 Mitigants"),
      checkboxGroupInput("active_mit", "Select Active Mitigants:",
                         choices = c("Firewall", "Backup", "Monitoring"),
                         selected = c("Firewall", "Backup")),
      actionButton("simulate", "Run Simulation", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("ROI Distribution", plotOutput("roiPlot")),
        tabPanel("Summary", tableOutput("summaryTable"))
      )
    )
  )
)

# ---------------- Server ----------------
server <- function(input, output, session) {
  
  mitigants <- reactive({
    data.frame(
      Name = c("Firewall", "Backup", "Monitoring"),
      ProbLow = c(0.6, 0.7, 0.8),
      ProbHigh = c(0.9, 0.95, 0.95),
      SevLow = c(0.9, 0.5, 0.7),
      SevHigh = c(1.0, 0.8, 0.9),
      CostLow = c(20000, 30000, 15000),
      CostHigh = c(50000, 70000, 40000),
      stringsAsFactors = FALSE
    )
  })
  
  sim_data <- eventReactive(input$simulate, {
    set.seed(42)
    n_post_draws <- input$n_sim
    
    # --- Fit base lognormal distribution from expert percentiles ---
    p_low <- 0.05; p_high <- 0.95
    z_low <- qnorm(p_low); z_high <- qnorm(p_high)
    mu_base <- (log(input$low) + log(input$high)) / 2
    sigma_base <- (log(input$high) - log(input$low)) / (z_high - z_low)
    mu_draws <- rnorm(n_post_draws, mean = mu_base, sd = 0.2)
    sigma_draws <- abs(rnorm(n_post_draws, mean = sigma_base, sd = 0.1 * sigma_base))
    expected_losses <- input$prob_event * exp(mu_draws + 0.5 * sigma_draws^2)
    
    # --- Determine active mitigants ---
    mit_data <- mitigants()
    active <- mit_data$Name %in% input$active_mit
    mit_data <- mit_data[active, ]
    
    # --- Compute mitigated losses ---
    if (nrow(mit_data) == 0) {
      exp_loss_vec <- expected_losses
      cost_vec <- rep(0, n_post_draws)
      combo <- "None"
    } else {
      prob_mat <- rpert_multi(n_post_draws,
                              low = mit_data$ProbLow,
                              mode = (mit_data$ProbLow + mit_data$ProbHigh) / 2,
                              high = mit_data$ProbHigh)
      sev_mat <- rpert_multi(n_post_draws,
                             low = mit_data$SevLow,
                             mode = (mit_data$SevLow + mit_data$SevHigh) / 2,
                             high = mit_data$SevHigh)
      cost_mat <- rpert_multi(n_post_draws,
                              low = mit_data$CostLow,
                              mode = (mit_data$CostLow + mit_data$CostHigh) / 2,
                              high = mit_data$CostHigh)
      
      prob_factor <- apply(prob_mat, 1, prod)
      sev_factor  <- apply(sev_mat, 1, prod)
      cost_vec <- rowSums(cost_mat)
      exp_loss_vec <- expected_losses * prob_factor * sev_factor
      combo <- paste(mit_data$Name, collapse = " + ")
    }
    
    loss_reduction <- expected_losses - exp_loss_vec
    roi_vec <- ifelse(cost_vec > 0, loss_reduction / cost_vec, 0)
    
    data.frame(ROI = roi_vec, Portfolio = combo, Cost = cost_vec)
  })
  
  # --- ROI Violin Plot ---
  output$roiPlot <- renderPlot({
    df <- sim_data()
    ggplot(df, aes(x = Portfolio, y = ROI, fill = Portfolio)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
      theme_minimal(base_size = 13) +
      labs(title = "ROI Distribution",
           subtitle = "Bayesian Monte Carlo Simulation with Uncertain Mitigant Effectiveness",
           y = "ROI (Loss Reduction / Cost)", x = "") +
      coord_flip() +
      theme(legend.position = "none")
  })
  
  # --- Summary Table ---
  output$summaryTable <- renderTable({
    df <- sim_data()
    data.frame(
      Portfolio = unique(df$Portfolio),
      Median_ROI = median(df$ROI),
      Mean_ROI = mean(df$ROI),
      P25 = quantile(df$ROI, 0.25),
      P75 = quantile(df$ROI, 0.75),
      Median_Cost = median(df$Cost)
    )
  })
}

# ---------------- Run App ----------------
shinyApp(ui, server)
