data <- read.csv("C:/Users/user/Downloads/simulated HF mort data for GMPH (1K) final.csv")
# Check the structure of the dataset
str(data)

# See the first few rows
head(data)

# Check the column names
colnames(data)


# Step 1: Install and load necessary packages
install.packages("survival")
install.packages("survminer")

library(survival)
library(survminer)

# Step 2: Load the dataset
g <- read.csv("C:/Users/user/Downloads/simulated HF mort data for GMPH (1K) final.csv")

# Step 3: Prepare the variables
# Convert gender to a factor (1=male, 2=female)
g$gender <- factor(g$gender, levels = c(1, 2), labels = c("Male", "Female"))

# Create a Surv object for survival analysis
# Assume 'death' is the event (1 = death, 0 = alive) and 'fu_time' is the follow-up time
surv_object <- Surv(time = g$fu_time, event = g$death)

# Step 4: Fit a Kaplan-Meier survival model
# This will calculate survival curves for each gender
surv_fit <- survfit(surv_object ~ gender, data = g)

# Step 5: Plot Kaplan-Meier survival curve
ggsurvplot(surv_fit, data = g, pval = TRUE, conf.int = TRUE, 
           title = "Kaplan-Meier Survival Curve by Gender", 
           xlab = "Follow-up Time (Days)", ylab = "Survival Probability",
           legend.title = "Gender")

# Step 6: View the Kaplan-Meier survival probabilities at specific times
summary(surv_fit, times = c(1, 30, 60, 90, 180, 365, 900))  # Adjust the time points as needed

# Step 7: Run the Log-Rank test to compare survival by gender
logrank_test <- survdiff(surv_object ~ gender, data = g)
print(logrank_test)

# Step 8: Interpretation (based on the result of the log-rank test)
# If the p-value from the log-rank test is less than 0.05, there is a significant difference in survival by gender.
if (logrank_test$pvalue < 0.05) {
  print("There is a significant difference in survival between men and women.")
} else {
  print("There is no significant difference in survival between men and women.")
}

###Another KM Plot and Lo-rank Test
# Load necessary libraries
library(survival)
library(survminer)

# Load the dataset
g <- read.csv("C:/Users/user/Downloads/simulated HF mort data for GMPH (1K) final.csv")

# Check for missing values in fu_time, death, and age columns
cat("Missing values in fu_time:", sum(is.na(g$fu_time)), "\n")
cat("Missing values in death:", sum(is.na(g$death)), "\n")
cat("Missing values in age:", sum(is.na(g$age)), "\n")

# Check the structure of the data to verify variable types
str(g)

# Create age group variable: Under 65 vs 65+
g$age_group <- ifelse(g$age >= 65, "65 and above", "Under 65")

# Check the unique values in the age_group column
cat("Unique age group values:", unique(g$age_group), "\n")

# Convert variables into appropriate types
g$death <- factor(g$death, levels = c(0, 1), labels = c("Alive", "Dead"))
g$age_group <- factor(g$age_group, levels = c("Under 65", "65 and above"))

# Check the levels of the 'age_group' and 'death' factors
cat("Levels of age_group:", levels(g$age_group), "\n")
cat("Levels of death:", levels(g$death), "\n")

# Create a Surv object
surv_obj <- Surv(time = g$fu_time, event = g$death)

# Fit the Kaplan-Meier model
km_fit_age_group <- survfit(surv_obj ~ age_group, data = g)

# Check the summary of the fitted Kaplan-Meier model
summary(km_fit_age_group)

# Plot the Kaplan-Meier curve for age groups
ggsurvplot(km_fit_age_group, data = g, pval = TRUE, conf.int = TRUE, 
           risk.table = TRUE, ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival Curve by Age Group",
           xlab = "Time (Days)", ylab = "Survival Probability")

# Check for missing values in critical columns
sum(is.na(g$age))  # Check if any missing values in 'age'
sum(is.na(g$fu_time))  # Check for missing values in 'fu_time'
sum(is.na(g$death))  # Check for missing values in 'death'

# Check the structure of the data (ensure there are no unexpected NAs or misalignment)
str(g)  # This will display the structure of your dataset

# Convert death to numeric (1 = Dead, 0 = Alive)
g$death <- ifelse(g$death == "Dead", 1, 0)

# Now create the Kaplan-Meier survival model
km_fit_age_group <- survfit(Surv(fu_time, death) ~ age_group, data = g)

# Plot the Kaplan-Meier curve for age groups
ggsurvplot(km_fit_age_group, data = g, pval = TRUE, conf.int = TRUE, 
           risk.table = TRUE, ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival Curve by Age Group",
           xlab = "Time (Days)", ylab = "Survival Probability")

##Cox Model

# Load necessary libraries
library(survival)

# Fit the Cox model (example using age and gender as predictors)
cox_model <- coxph(Surv(fu_time, death) ~ age + gender, data = g)

# Summary of the model to see the results
summary(cox_model)

## Running a simple Cox proportional hazards model
# Load necessary libraries
library(survival)

# Ensure ethnicgroup is a factor
ethnicgroup <- factor(g[,"ethnicgroup"])

# Extract survival time and death status
fu_time <- g[,"fu_time"]
death <- g[,"death"]

# Run Cox regression model
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)

# Display summary of the model
summary(cox)


# Age (Continuous Variable)
summary(g$age)

# Gender (Categorical Variable)
table(g$gender, exclude = NULL)

# Prior OPD Appointments Missed (Categorical Variable)
table(g$prior_dnas, exclude = NULL)

# Ethnic Group (Categorical Variable)
table(g$ethnicgroup, exclude = NULL)

# COPD (Categorical Variable)
table(g$copd, exclude = NULL)

##Running Multiple Cox Model
# Load required packages
library(survival)

# Assume 'g' is your dataset with appropriate variables
cox_model <- coxph(Surv(fu_time, death) ~ age + gender + ethnicgroup + copd + prior_dnas, data = g)

# Display the summary of the model
summary(cox_model)


# Run Cox model with the new set of predictors
cox_model <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup, data = g)

# Check the summary of the model
summary(cox_model)

# Check for multicollinearity using VIF
library(car)
vif(cox_model)

# Check the proportional hazards assumption
cox.zph(cox_model)

# Impute missing data if necessary
library(mice)
g_imputed <- mice(g, m = 5, method = 'pmm', seed = 123)

##Fitting a model with issues
##Non-convergence
# Convert 'quintile' to a factor if it's not already
g$quintile <- as.factor(g$quintile)

# Set the reference category for 'quintile' to 1 (the highest socio-economic status)
g$quintile <- relevel(g$quintile, ref = 1)

# Optionally, exclude patients in quintile 0 if needed (optional step if reference change isn't enough)
g <- g[g$quintile != 0, ]  # Remove quintile 0 if it has very few patients and represents missing data

# Run the Cox model with the new reference category and the cleaned dataset
cox_model <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup, data = g)

# Check the model summary
summary(cox_model)


##Steps to Check Proportional Hazards Assumption:
# Fit the Cox model
cox_model <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup, data = g)

# Check the proportional hazards assumption
ph_assumption <- cox.zph(cox_model)

# Display the summary of the test (for checking p-values and rho statistics)
summary(ph_assumption)

# Visualize the results to inspect the proportional hazards assumption
plot(ph_assumption)

# View detailed test results for each covariate
ph_assumption$table

# Check the structure of the table
str(ph_assumption$table)


file_path <- "C:\\Users\\user\\Downloads\\simulated HF mort data for GMPH (1K) final.csv"

# Load necessary library
library(survival)

# Read CSV data
data <- read.csv(file_path)

# Inspect structure
str(data)

# Example Cox model (adjust variables as needed)
fit <- coxph(Surv(fu_time, death) ~ gender, data = data)

# Check proportional hazards assumption
temp <- cox.zph(fit)
print(temp)  # Displays p-value for PH assumption
plot(temp)   # Visual check for violations


# Fit Cox model with COPD
fit_copd <- coxph(Surv(fu_time, death) ~ copd, data = data)

# Check proportional hazards assumption
temp_copd <- cox.zph(fit_copd)
print(temp_copd)  # Displays p-value for COPD

# Fit the Cox model for gender
fit <- coxph(Surv(fu_time, death) ~ gender + copd + quintile + ethnicgroup, data = g)

# Check proportional hazards assumption using cox.zph function
ph_assumption <- cox.zph(fit)
print(ph_assumption)  # Display the test results

# If proportionality assumption is violated, add interaction term with time
# (using the "tt" function to allow for time-varying effect of gender)
fit_with_interaction <- coxph(Surv(fu_time, death) ~ gender + copd + quintile + ethnicgroup + tt(gender), data = g)
summary(fit_with_interaction)  # Check p-value for the interaction term

# Split the follow-up time into two periods if necessary (e.g., <=5 years and >5 years)
g$time_period <- ifelse(g$fu_time <= 5, 1, 2)

# Fit separate models for the two time periods
fit_early <- coxph(Surv(fu_time, death) ~ gender + copd + quintile + ethnicgroup, data = g[g$time_period == 1, ])
fit_late <- coxph(Surv(fu_time, death) ~ gender + copd + quintile + ethnicgroup, data = g[g$time_period == 2, ])
summary(fit_early)
summary(fit_late)

# Stratify by gender if proportional hazards assumption is violated
fit_stratified <- coxph(Surv(fu_time, death) ~ strata(gender) + copd + quintile + ethnicgroup, data = g)
summary(fit_stratified)

# Alternatively, treat gender as a time-varying covariate
fit_time_varying <- coxph(Surv(fu_time, death) ~ gender * fu_time + copd + quintile + ethnicgroup, data = g)
summary(fit_time_varying)

# Plot results for visual inspection (use ggcoxzph for prettier plots)
library(survminer)
ggcoxzph(ph_assumption)  # Plot scaled Schoenfeld residuals against transformed time

# If you need to plot Kaplan-Meier curves for gender
ggsurv <- survfit(Surv(fu_time, death) ~ gender, data = g)
ggsurv_plot <- ggsurvplot(ggsurv, data = g, pval = TRUE, conf.int = TRUE)
print(ggsurv_plot)







