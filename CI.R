# Confidence Interval for Scholarship importance = "Extremely important" (5)

# Total sample size
n <- nrow(survey_data)

# Sample proportion (students who answered 5 on Scholarship_info)
p_hat <- mean(survey_data$Scholarship_info == 5, na.rm = TRUE)

# Standard error
se <- sqrt(p_hat * (1 - p_hat) / n)

# 95% CI critical value
z <- 1.96

# CI bounds
lower <- p_hat - z * se
upper <- p_hat + z * se

# Create a results table
ci_table <- data.frame(
  Estimate = round(p_hat, 3),
  CI_Lower = round(lower, 3),
  CI_Upper = round(upper, 3),
  N = n
)

ci_table
