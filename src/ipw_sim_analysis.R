# Set random seed for reproducibility
set.seed(123)

# Number of subjects
n <- 1000

# Baseline covariates
age <- rnorm(n, mean = 50, sd = 10)
gender <- rbinom(n, size = 1, prob = 0.5)

# Outcome of interest at baseline
y_baseline <- 100 + 2 * age + 5 * gender + rnorm(n)

# Simulate attrition at follow-up 1 (related to baseline outcome)
attrition_1_prob <- 1 / (1 + exp(-(0.006 * y_baseline)))
attrition_1 <- rbinom(n, size = 1, prob = attrition_1_prob)

# Invert attrition_1
# attrition_1 <- 1 - attrition_1

# Outcome of interest at follow-up 1 (only for those who did not drop out)
y_fu1 <- rep(NA, n)
y_fu1[attrition_1 == 0] <- y_baseline[attrition_1 == 0] + 2 * age[attrition_1 == 0] + 3 * gender[attrition_1 == 0] + rnorm(sum(1 - attrition_1))

# Simulate attrition at follow-up 2 (related to follow-up 1 outcome)
attrition_2_prob <- 1 / (1 + exp(-(0.002* y_fu1)))
attrition_2 <- rbinom(n, size = 1, prob = attrition_2_prob)
# Invert attrition_1
attrition_2 <- 1 - attrition_2

# Replace attrition_2 for those who dropped out at follow-up 1
attrition_2[attrition_1 == 1] <- 1

# Outcome of interest at follow-up 2 (only for those who did not drop out)
y_fu2 <- rep(NA, n)
y_fu2[attrition_1 == 0 & attrition_2 == 0] <- y_fu1[attrition_1 == 0 & attrition_2 == 0] + 2 * age[attrition_1 == 0 & attrition_2 == 0] + 3 * gender[attrition_1 == 0 & attrition_2 == 0] + rnorm(sum(1 - attrition_2[attrition_1 == 0 & attrition_2 == 0]))

# Create a data frame
data <- data.frame(age, gender, y_baseline, attrition_1, y_fu1, attrition_2, y_fu2)

# Logistic regression model to predict attrition_1
model_attrition_1 <- glm(attrition_1 ~ age + gender, data = data, family = binomial(link = "logit"))

# Logistic regression model to predict attrition_2
model_attrition_2 <- glm(attrition_2 ~ age + gender + y_fu1, data = data, family = binomial(link = "logit"))

# Predict propensity scores for attrition_1
propensity_attrition_1 <- predict(model_attrition_1, type = "response")

# Predict propensity scores for attrition_2
propensity_attrition_2 <- predict(model_attrition_2, type = "response")

# Calculate IPW for attrition_1
data$ipw_attrition_1 <- ifelse(data$attrition_1 == 1, 1 / (1 - propensity_attrition_1), 1 / propensity_attrition_1)

# Calculate IPW for attrition_2
data$ipw_attrition_2 <- ifelse(data$attrition_2 == 1, 1 / (1 - propensity_attrition_2), 1 / propensity_attrition_2)

# Combine IPW weights
data$ipw <- data$ipw_attrition_1 * data$ipw_attrition_2



# Create a data frame for plotting
plot_data <- data.frame(
  Outcome = c(rep("Before IPW", sum(data$attrition_1 == 0)), rep("After IPW", sum(data$attrition_1 == 0))),
  Value = c(data$y_baseline[data$attrition_1 == 0], data$y_baseline[data$attrition_1 == 0] * data$ipw[data$attrition_1 == 0])
)

# Create a density plot
density_plot <- ggplot(plot_data, aes(x = Value, fill = Outcome, color = Outcome)) +
  geom_density(alpha = 0.5) +
  labs(title = "Effect of IPW on Outcome Distribution",
       x = "Outcome Value",
       y = "Density") +
  theme_minimal()

# Display the plot
density_plot

