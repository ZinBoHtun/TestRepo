# Load dataset
data <- read.csv("C:/Users/user/Downloads/final-diabetes-data.csv", header = TRUE)

# Check dimensions and column names
dim(data)                 # Number of rows and columns
colnames(data)           # Column names

# Convert gender to factor with labels (edit if your column is named differently)
data$gender <- factor(data$gender, levels = c(0, 1), labels = c("Male", "Female"))

# Tabulate gender frequencies and proportions
gender_table <- table(data$gender, useNA = "always")
gender_table
round(100 * prop.table(gender_table), 1)  # Percentages

# Summarize cholesterol (edit if your cholesterol column is named differently)
summary(data$cholesterol)

# Convert weight and height to SI units
data$weight_kg <- data$weight * 0.4536    # pounds to kg
data$height_m <- data$height * 0.0254     # inches to meters

# Calculate BMI
data$bmi <- data$weight_kg / (data$height_m ^ 2)
summary(data$bmi)

# Categorize BMI
data$bmi_cat <- cut(data$bmi,
                    breaks = c(0, 18.5, 25, 30, Inf),
                    labels = c("Underweight", "Normal", "Overweight", "Obese"),
                    right = FALSE)

# Check distribution of BMI categories
bmi_cat_table <- table(data$bmi_cat, useNA = "always")
bmi_cat_table
round(100 * prop.table(bmi_cat_table), 1)

# Cross-tabulation of BMI category and diabetes
# (edit "dm" if your diabetes variable has a different name)
bmi_dm_table <- table(data$bmi_cat, data$dm)
bmi_dm_table

# Row-wise proportions
prop.table(bmi_dm_table, margin = 1)

# Optional: Chi-squared test
chisq.test(bmi_dm_table)

# Read the data
data <- read.csv("C:/Users/user/Downloads/final-diabetes-data.csv", header = TRUE)

# Clean gender column (trim whitespace and standardize capitalization)
data$gender <- trimws(tolower(data$gender))
data$gender[data$gender == "f"] <- "female"
data$gender[data$gender == "m"] <- "male"
data$gender <- factor(data$gender, levels = c("male", "female"))

# Create age groups
data$age_group <- cut(data$age,
                      breaks = c(0, 45, 65, 75, Inf),
                      labels = c("Under 45", "45-64", "65-74", "75+"),
                      right = FALSE)

# Cross-tabulate age group by gender
age_gender_tab <- table(data$age_group, data$gender, useNA = "ifany")
print(age_gender_tab)

# Add row percentages
print(round(100 * prop.table(age_gender_tab, margin = 1), 1))

# Get number of females under 45
age_gender_tab["Under 45", "female"]

# 1. Run an Empty (Null) Logistic Regression Model
# Load the dataset
data <- read.csv("C:/Users/user/Downloads/final-diabetes-data.csv", header = TRUE)

# Convert 'dm' column from "Yes"/"No" to numeric (1/0)
data$dm <- ifelse(tolower(data$dm) == "yes", 1,
                  ifelse(tolower(data$dm) == "no", 0, NA))

# Check how many NA values (if any) were produced
sum(is.na(data$dm))

# Remove rows with NA in 'dm'
data <- data[!is.na(data$dm), ]

# Fit the null logistic regression model (no predictors, just intercept)
m_null <- glm(dm ~ 1, data = data, family = binomial)

# Show summary of the model
summary(m_null)

#2. Fit a Model with One Predictor (Gender)
# Fit the logistic regression model with gender as the predictor
m_gender <- glm(dm ~ gender, data = data, family = binomial)

# Summarize the results
summary(m_gender)

#3. Fit a Model with a Continuous Predictor (Age)
# Fit the logistic regression model with age as the predictor
m_age <- glm(dm ~ age, data = data, family = binomial)

# Summarize the results
summary(m_age)

#4. Plot Age vs Diabetes to Check Linearity
# Plot age vs diabetes
plot(data$age, data$dm, 
     main = "Age vs Diabetes",
     xlab = "Age", ylab = "Diabetes (0 = No, 1 = Yes)",
     pch = 19, col = ifelse(data$dm == 1, "red", "blue"))


#DM by Location
# Filter the data to only include rows where diabetes status (dm) is recorded (non-missing)
data_complete <- subset(data, !is.na(dm))

# Get the subset of data for people from Buckingham
buckingham_data <- subset(data_complete, location == "Buckingham")

# Calculate the percentage of people from Buckingham with diabetes (dm == 1)
percentage_buckingham <- mean(buckingham_data$dm == 1) * 100
percentage_buckingham

data <- read.csv("C:\\Users\\user\\Downloads\\final-diabetes-data.csv")
head(data)  # Check the first few rows
colnames(data)  # Check the column names

# If height is in centimeters, convert it to meters
data$height_m <- data$height / 100  # if height is in cm

# Calculate BMI
data$bmi <- data$weight / (data$height_m^2)

#1. Summarizing Variables Numerically:
# Summary for continuous variables
summary(data$age)
summary(data$bmi)
summary(data$hdl)
summary(data$chol)

# Table for categorical variables
table(data$gender)

#2. Visualizing Continuous Variables:
# Histograms
hist(data$age, main = "Age Distribution", xlab = "Age", breaks = 20, col = "lightblue")
hist(data$bmi, main = "BMI Distribution", xlab = "BMI", breaks = 20, col = "lightgreen")
hist(data$hdl, main = "HDL Distribution", xlab = "HDL", breaks = 20, col = "lightcoral")
hist(data$chol, main = "Cholesterol Distribution", xlab = "Cholesterol", breaks = 20, col = "lightgoldenrodyellow")

# Boxplots
boxplot(data$age, main = "Boxplot of Age", ylab = "Age")
boxplot(data$bmi, main = "Boxplot of BMI", ylab = "BMI")
boxplot(data$hdl, main = "Boxplot of HDL", ylab = "HDL")
boxplot(data$chol, main = "Boxplot of Cholesterol", ylab = "Cholesterol")

#3. Relating Variables to Diabetes Outcome:
# Cross-tab for Gender and Diabetes
table(data$gender, data$dm)

# Boxplot comparing continuous variables with diabetes status
boxplot(data$bmi ~ data$dm, main = "BMI vs Diabetes", xlab = "Diabetes", ylab = "BMI")
boxplot(data$hdl ~ data$dm, main = "HDL vs Diabetes", xlab = "Diabetes", ylab = "HDL")
boxplot(data$chol ~ data$dm, main = "Cholesterol vs Diabetes", xlab = "Diabetes", ylab = "Cholesterol")
boxplot(data$age ~ data$dm, main = "Age vs Diabetes", xlab = "Diabetes", ylab = "Age")

# Mean for continuous variables grouped by diabetes status, ignoring NA values
tapply(data$bmi, data$dm, mean, na.rm = TRUE)
tapply(data$hdl, data$dm, mean, na.rm = TRUE)
tapply(data$chol, data$dm, mean, na.rm = TRUE)
tapply(data$age, data$dm, mean, na.rm = TRUE)

# Recode 'dm' to numeric values: "no" becomes 0, "yes" becomes 1
data$dm <- ifelse(data$dm == "yes", 1, 0)

# Remove rows with NA values in 'dm' or any predictor (age, bmi, hdl, chol)
data <- data[!is.na(data$dm) & !is.na(data$age) & !is.na(data$gender) & !is.na(data$bmi) & !is.na(data$hdl) & !is.na(data$chol), ]

# Fit the logistic regression model with age, bmi, hdl, and chol as predictors
model <- glm(dm ~ age + gender + bmi + hdl + chol, data = data, family = binomial)

# Display the summary of the model
summary(model)

# Example plot for Age vs. Log Odds
plot(data$age, predict(model, type = "link"), xlab = "Age", ylab = "Log Odds of Diabetes")


data <- read.csv("C:\\Users\\user\\Downloads\\final-diabetes-data.csv")
head(data)  # Check the first few rows
colnames(data)  # Check the column names
data$dm <- ifelse(tolower(trimws(data$dm)) == "yes", 1,
                  ifelse(tolower(trimws(data$dm)) == "no", 0, NA))
table(data$dm, useNA = "ifany")
data_clean <- subset(data, !is.na(dm))
names(data_clean)
data_clean$bmi <- data_clean$weight / (data_clean$height/100)^2


model <- glm(dm ~ age + gender + bmi + hdl + chol, family = binomial, data = data_clean)
summary(model)

#New Regressional Model with Insurance (Gov and Private)
# Load necessary libraries
library(dplyr)

# Assuming your data is named 'data_clean'
# Create dummy variables for the 'insurance' variable
data_clean <- data_clean %>%
  mutate(insurance_gov = ifelse(insurance == 1, 1, 0),
         insurance_private = ifelse(insurance == 2, 1, 0))

# Logistic regression model with one-hot encoded insurance variables
model_updated <- glm(dm ~ age + chol + insurance_gov + insurance_private, 
                     data = data_clean, 
                     family = binomial)

# Model summary
summary(model_updated)

# Calculate Odds Ratios (OR)
odds_ratios_updated <- exp(coef(model_updated))

# Display odds ratios to 2 decimal places
odds_ratios_updated <- round(odds_ratios_updated, 2)
odds_ratios_updated

# Check for statistical significance
p_values_updated <- summary(model_updated)$coefficients[, 4]

# Display p-values
p_values_updated

# Check for significance at 5% level
significance_updated <- ifelse(p_values_updated < 0.05, "Significant", "Not Significant")
significance_updated

#3. Practical Steps in R
#McFadden's R-squared:
# Assuming your model is named 'model'
log_likelihood_full <- logLik(model)
log_likelihood_null <- logLik(glm(dm ~ 1, data = data_clean, family = "binomial"))
mcfadden_r2 <- 1 - (log_likelihood_full / log_likelihood_null)
print(mcfadden_r2)

#C-statistic / AUC:
# Check for missing values in response and predicted values
sum(is.na(data_clean$dm))
sum(is.na(predict(model, newdata = data_clean, type = "response")))

# Ensure data is complete for both response and predictions
data_clean <- data_clean[complete.cases(data_clean$dm), ]

# Recalculate predictions for the complete cases
predicted_values <- predict(model, newdata = data_clean, type = "response")

# Check that lengths match
length(data_clean$dm)
length(predicted_values)

# Create the ROC curve
library(pROC)
roc_curve <- roc(data_clean$dm, predicted_values)

# Plot the ROC curve
plot(roc_curve)
auc(roc_curve) ## AUC=1(Perfetmodel),=0.5 (guessing model),=>0.7 (considered good model)


#Hosmer-Lemeshow Test:
# Using the 'ResourceSelection' package for the Hosmer-Lemeshow test
library(ResourceSelection)
hoslem_test <- hoslem.test(model$fitted.values, model$y, g = 2)  # Try reducing the number of bins (e.g., g = 10)
print(hoslem_test) ##indicating very low repetition of predicted probabilities that causes errors
table(model$fitted.values)

#Brier Score (Calibration metric):
mean((model$fitted.values - model$y)^2)

#Calibration Plot:
# Load required libraries
library(ggplot2)
library(dplyr)

# Create a data frame with predicted probabilities and observed values
calibration_data <- data.frame(
  predicted = model$fitted.values,
  observed = model$y
)

# Bin the predicted probabilities into deciles (10 groups)
calibration_data <- calibration_data %>%
  mutate(bin = ntile(predicted, 10)) %>%
  group_by(bin) %>%
  summarise(
    mean_predicted = mean(predicted),
    mean_observed = mean(observed),
    count = n()
  )

# Plot observed vs predicted
ggplot(calibration_data, aes(x = mean_predicted, y = mean_observed)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Calibration Plot",
       x = "Mean Predicted Probability",
       y = "Mean Observed Proportion") +
  theme_minimal()
###Interpretation calibration plot
#Points close to the 45-degree line (dashed line) = good calibration
#Points below the line indicate the model overpredicts risk.
#Points above the line indicate the model underpredicts risk.


#Backwards Elimination
##### Make the variables and run the models #####
df <- data

model_full <- glm(dm ~ age + bmi + chol + hdl + bp.1s + bp.1d,
                  data = df, family = binomial)
summary(model_full)

#Step 1: Drop bp.1d (highest p-value)
model_step1 <- glm(dm ~ age + bmi + chol + hdl + bp.1s,
                   data = df, family = binomial)
summary(model_step1)

#Step 2: Drop bp.1s
model_final <- glm(dm ~ age + bmi + chol + hdl,
                   data = df, family = binomial)
summary(model_final)

##Model Expanded
# Full model with all specified predictors
model_expanded <- glm(dm ~ age + bmi + chol + hdl + bp.1s + bp.1d +
                        gender + location + frame + insurance + smoking,
                      data = df, family = binomial)

# View the summary to assess significance
summary(model_expanded)

