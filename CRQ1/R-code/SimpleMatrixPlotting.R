# Install ggplot2 if not already installed
# install.packages("ggplot2")

library(ggplot2)

# Create the 3x3 matrix data
#risk_matrix <- expand.grid(
#  Likelihood = 1:3,
#  Consequence = 1:3
#)

risk_matrix <- expand.grid(
  Likelihood = c(0.3,0.7,1),
  Consequence = c(10000,20000,30000)
)

risk_matrix$Risk = risk_matrix$Likelihood * risk_matrix$Consequence

risk_matrix$RiskLevel <- cut(
  risk_matrix$Risk,
  breaks = c(0, 8999, 14000, 30000),
  labels = c("Low", "Medium", "High")
)
risk_matrix$Color <- c("green", "yellow", "red")[risk_matrix$RiskLevel]


  ggplot(risk_matrix, aes(x = as.factor(Consequence), y = as.factor(Likelihood), fill = RiskLevel)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("Low" = "green", "Medium" = "yellow", "High" = "red")) +
  geom_text(aes(label = Risk), size = 6) +
  labs(
    title = "3x3 Security Risk Matrix",
    x = "Consequence (Severity)",
    y = "Likelihood (Probability)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), plot.title = element_text(hjust = 0.5))