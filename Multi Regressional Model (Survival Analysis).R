
# Load libraries
library(survival)
library(survminer)

# Load the dataset
data <- read.csv("C:/Users/user/Downloads/simulated HF mort data for GMPH (1K) final.csv")

# Inspect the first few rows of the data to check the column names and data
head(data)

# Fit the initial Cox model with chosen predictors from the report
# Assuming relevant columns are available in the dataset (e.g., age, length_of_stay, prior_missed_appointments, comorbidities, gender)
cox_model <- coxph(Surv(fu_time, death) ~ age + los + prior_appts_attended + gender + diabetes + hypertension, data = data)

# Summary of the model to check coefficients, hazard ratios, and p-values
summary(cox_model)

# Perform backward elimination using the step() function
step_model <- step(cox_model, direction = "backward", trace = 0)

# Summary of the final model after backward elimination
summary(step_model)

# Check proportional hazards assumption
ph_assumption <- cox.zph(step_model)

# Print results of the test for proportional hazards assumption
print(ph_assumption)

# Plot the results of the test
plot(ph_assumption)

# Compare the coefficients and hazard ratios (HRs) from the original and final model
original_coefs <- summary(cox_model)$coefficients
final_coefs <- summary(step_model)$coefficients

# Calculate hazard ratios for both models
original_HRs <- exp(original_coefs[, "coef"])
final_HRs <- exp(final_coefs[, "coef"])

# Display the comparison of hazard ratios
comparison_HRs <- data.frame(Original_HRs = original_HRs, Final_HRs = final_HRs)
print(comparison_HRs)
