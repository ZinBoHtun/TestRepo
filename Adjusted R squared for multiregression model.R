# Load necessary packages
# If you don't have these packages installed, you can install them with install.packages()
# library(ggplot2)  # For plotting (if needed)

# 1. Fit Simple Regression Model (Walking Distance vs. FVC)
MWT1Best_FVC <- lm(MWT1Best ~ FVC, data = COPD_data)

# View summary of the simple model (Walking Distance ~ FVC)
summary(MWT1Best_FVC)

# 2. Fit Multiple Regression Model (Walking Distance vs. FVC and AGE)
MWT1Best_FVC_AGE <- lm(MWT1Best ~ FVC + AGE, data = COPD_data)

# View summary of the multiple model (Walking Distance ~ FVC + AGE)
summary(MWT1Best_FVC_AGE)

# 3. Confidence Intervals for Multiple Regression Coefficients
confint(MWT1Best_FVC_AGE)

# 4. Check for Multicollinearity (Correlation between FVC and AGE)
cor(COPD_data$FVC, COPD_data$AGE)

# 5. Model Diagnostics: Plot the Residuals
# Plot diagnostic plots to check assumptions
# These plots will help you assess linearity, homoscedasticity, and normality
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(MWT1Best_FVC_AGE)

# 6. Fit the Model with FVC, FEV1, and AGE (if needed for comparison)
MWT1Best_FVC_FEV1_AGE <- lm(MWT1Best ~ FVC + FEV1 + AGE, data = COPD_data)

# View summary of the model (Walking Distance ~ FVC + FEV1 + AGE)
summary(MWT1Best_FVC_FEV1_AGE)

# Optionally, you can also plot diagnostics for this new model
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(MWT1Best_FVC_FEV1_AGE)

# 7. Comparison of Adjusted R-squared Values
# You can compare adjusted R-squared values for the models to assess which one fits better:
adjusted_r2_FVC <- summary(MWT1Best_FVC)$adj.r.squared
adjusted_r2_FVC_AGE <- summary(MWT1Best_FVC_AGE)$adj.r.squared
adjusted_r2_FVC_FEV1_AGE <- summary(MWT1Best_FVC_FEV1_AGE)$adj.r.squared

# Print adjusted R-squared values for comparison
cat("Adjusted R-squared for MWT1Best ~ FVC: ", adjusted_r2_FVC, "\n")
cat("Adjusted R-squared for MWT1Best ~ FVC + AGE: ", adjusted_r2_FVC_AGE, "\n")
cat("Adjusted R-squared for MWT1Best ~ FVC + FEV1 + AGE: ", adjusted_r2_FVC_FEV1_AGE, "\n")
