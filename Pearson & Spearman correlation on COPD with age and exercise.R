# Histogram for MWT1best (Walking Distance)
hist(COPD_data$MWT1Best, main="Histogram of MWT1Best (Walking Distance)", xlab="MWT1Best", breaks=12)

# Histogram for AGE (Age)
hist(COPD_data$AGE, main="Histogram of Age", xlab="Age", breaks=12)

# Summary for MWT1Best (Walking Distance)
summary(COPD_data$MWT1Best)

# Summary for AGE (Age)
summary(COPD_data$AGE)

# Additional statistics like mean and standard deviation
mean_MWT1Best <- mean(COPD_data$MWT1Best, na.rm = TRUE)
sd_MWT1Best <- sd(COPD_data$MWT1Best, na.rm = TRUE)

mean_AGE <- mean(COPD_data$AGE, na.rm = TRUE)
sd_AGE <- sd(COPD_data$AGE, na.rm = TRUE)

# Displaying results
list(
  "Mean MWT1Best" = mean_MWT1Best,
  "Standard Deviation MWT1Best" = sd_MWT1Best,
  "Mean Age" = mean_AGE,
  "Standard Deviation Age" = sd_AGE
)

# Scatter plot for MWT1Best vs AGE
plot(COPD_data$AGE, COPD_data$MWT1Best, main="Scatter Plot of MWT1Best vs AGE", 
     xlab="Age", ylab="MWT1Best (Walking Distance)", pch=19, col="blue")

# Pearson's correlation test
pearson_result <- cor.test(COPD_data$MWT1Best, COPD_data$AGE, method = "pearson", use = "complete.obs")

# Display the result
pearson_result

# Spearman's correlation test
spearman_result <- cor.test(COPD_data$MWT1Best, COPD_data$AGE, method = "spearman", use = "complete.obs")

# Display the result
spearman_result
