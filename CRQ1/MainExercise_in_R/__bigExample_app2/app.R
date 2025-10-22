# =======================================================
# Bayesian Monte Carlo Risk Mitigation Simulator (fixed)
# =======================================================
# Run in RStudio: click "Run App"
# Requires: shiny, ggplot2, gtools, dplyr
# =======================================================

library(shiny)
library(ggplot2)
library(gtools)
library(dplyr)
library(scales)

# ---- Robust, vectorized PERT sampler ----
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

# =======================================================
# UI
# =======================================================
ui <- fluidPage(
  titlePanel("🧠 Bayesian Monte Carlo Risk Mitigation Simulator (fixed)"),
  
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
      actionButton("simulate", "Run Simulation", class = "btn-primary"),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("ROI Distribution", plotOutput("roiPlot", height = "500px")),
        tabPanel("Loss Exceedance Curve (LEC)", plotOutput("lecPlot", height = "500px")),
        tabPanel("Summary", tableOutput("summaryTable"))
      ),
      width = 9
    )
  )
)

# =======================================================
# Server
# =======================================================
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
    
    # sample μ and σ (simple prior sampling for demo)
    mu_draws <- rnorm(n_post_draws, mean = mu_base, sd = 0.2)
    sigma_draws <- abs(rnorm(n_post_draws, mean = sigma_base, sd = 0.1 * sigma_base))
    
    # --- CORRECTED: sample severity draws directly (no extra exp)
    severity_draws <- rlnorm(n_post_draws, meanlog = mu_draws, sdlog = sigma_draws)
    base_losses <- input$prob_event * severity_draws
    
    mit_data <- mitigants()
    active <- mit_data$Name %in% input$active_mit
    mit_data <- mit_data[active, ]
    
    if (nrow(mit_data) == 0) {
      exp_loss_vec <- base_losses
      cost_vec <- rep(0, n_post_draws)
      combo <- "None"
      sev_draws_mit <- severity_draws  # keep for LEC tab
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
      
      # product of remaining probability factors and severity scaling factors
      prob_factor <- apply(prob_mat, 1, prod)
      sev_factor  <- apply(sev_mat, 1, prod)
      cost_vec <- rowSums(cost_mat)
      
      # mitigated severity draws = severity_draws * sev_factor
      severity_draws_mit <- severity_draws * sev_factor
      sev_draws_mit <- severity_draws_mit
      
      # mitigated expected annual loss per draw = prob_event * severity_draws_mit * prob_factor
      # note: prob_factor already represents residual probability (e.g., 0.2*(1-reduction1)*(1-reduction2)...)
      exp_loss_vec <- input$prob_event * severity_draws_mit * prob_factor
      combo <- paste(mit_data$Name, collapse = " + ")
    }
    
    # Loss reduction and ROI (robust handling of zero/Inf)
    loss_reduction <- base_losses - exp_loss_vec
    # sample costs may be zero; avoid division by zero/infinite ROIs
    cost_vec_clean <- ifelse(is.na(cost_vec) | is.infinite(cost_vec), NA, cost_vec)
    roi_vec <- ifelse(!is.na(cost_vec_clean) & cost_vec_clean > 0, loss_reduction / cost_vec_clean, NA_real_)
    
    # Remove any non-finite values (safeguard)
    ok <- is.finite(roi_vec) | is.na(roi_vec)
    
    data.frame(
      ROI = roi_vec[ok],
      Portfolio = combo,
      Cost = if (!is.null(cost_vec)) cost_vec_clean[ok] else rep(0, sum(ok)),
      BaseLoss = base_losses[ok],
      MitigatedLoss = exp_loss_vec[ok]
    )
  }, ignoreNULL = FALSE)
  
  # ---------- ROI Violin Plot ----------
  output$roiPlot <- renderPlot({
    df <- sim_data()
    # if all ROI are NA (e.g., no costs), give a message plot
    if (all(is.na(df$ROI))) {
      plot.new()
      text(0.5, 0.5, "No ROI data (no costs or all ROIs are undefined). Toggle mitigants with costs.", cex = 1.1)
      return()
    }
    # drop NA ROI values for plotting
    df_plot <- df %>% filter(!is.na(ROI) & is.finite(ROI))
    # cap extreme ROI values for display (optional)
    max_display <- quantile(df_plot$ROI, 0.995, na.rm = TRUE)
    df_plot$ROI_plot <- pmin(df_plot$ROI, max_display)
    
    ggplot(df_plot, aes(x = Portfolio, y = ROI_plot, fill = Portfolio)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      geom_boxplot(width = 0.08, fill = "white", outlier.shape = NA) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      theme_minimal(base_size = 13) +
      labs(title = "ROI Distribution",
           subtitle = "Bayesian Monte Carlo Simulation with Uncertain Mitigant Effectiveness",
           y = "ROI (Loss Reduction / Cost)", x = "") +
      coord_flip() +
      theme(legend.position = "none")
  })
  
  # ---------- LEC Plot ----------
  output$lecPlot <- renderPlot({
    df <- sim_data()
    # need base and mitigated loss draws
    if (!("BaseLoss" %in% names(df)) || !("MitigatedLoss" %in% names(df))) {
      plot.new(); text(0.5,0.5,"No loss data available"); return()
    }
    base_losses <- df$BaseLoss
    mit_losses <- df$MitigatedLoss
    
    # create a loss grid (log-spaced) between small fraction and larger multiple of observed draws
    minL <- max(1, min(c(base_losses, mit_losses), na.rm = TRUE) * 0.2)
    maxL <- max(c(base_losses, mit_losses), na.rm = TRUE) * 5
    loss_vals <- exp(seq(log(minL), log(maxL), length.out = 300))
    
    exceed_base <- sapply(loss_vals, function(L) mean(base_losses > L, na.rm = TRUE))
    exceed_mit  <- sapply(loss_vals, function(L) mean(mit_losses > L, na.rm = TRUE))
    
    lec_df <- data.frame(
      Loss = rep(loss_vals, 2),
      ProbExceed = c(exceed_base, exceed_mit),
      Scenario = rep(c("Base", unique(df$Portfolio)), each = length(loss_vals))
    )
    
    ggplot(lec_df, aes(x = Loss, y = ProbExceed, color = Scenario)) +
      geom_line(size = 1.1) +
      scale_x_log10(labels = dollar_format()) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      theme_minimal(base_size = 13) +
      labs(title = "Loss Exceedance Curves (LEC)",
           subtitle = "Empirical exceedance probabilities from Monte Carlo draws",
           x = "Annual Loss ($, log scale)", y = "Probability of Exceedance") +
      theme(legend.position = "top")
  })
  
  # ---------- Summary Table ----------
  output$summaryTable <- renderTable({
    df <- sim_data()
    df_clean <- df %>% mutate(ROI = as.numeric(ROI))
    data.frame(
      Portfolio = unique(df_clean$Portfolio),
      Median_ROI = round(median(df_clean$ROI, na.rm = TRUE), 3),
      Mean_ROI = round(mean(df_clean$ROI, na.rm = TRUE), 3),
      P25 = round(quantile(df_clean$ROI, 0.25, na.rm = TRUE), 3),
      P75 = round(quantile(df_clean$ROI, 0.75, na.rm = TRUE), 3),
      Median_Cost = round(median(df_clean$Cost, na.rm = TRUE), 0),
      Median_Loss_Reduction = round(median(df_clean$BaseLoss - df_clean$MitigatedLoss, na.rm = TRUE), 0)
    )
  }, striped = TRUE, hover = TRUE)
}

# =======================================================
# Run App
# =======================================================
shinyApp(ui, server)
