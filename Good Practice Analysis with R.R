# Load necessary libraries
library(Hmisc)   # For describe function
library(gmodels) # For CrossTable

# 1. Inspect the dataset for missing values and outliers

# Inspect dataset using describe from the Hmisc package
describe(COPD_data)

# Check for missing values in the entire dataset
sum(is.na(COPD_data))  # Check for NAs in the dataset

# Summary statistics for continuous variables
summary(COPD_data$AGE)  # For continuous variable AGE

# Visualize the distribution of AGE using a histogram
hist(COPD_data$AGE, main="Distribution of AGE", xlab="Age", col="lightblue", border="black")

# Repeat summary and histogram for other continuous variables like FEV1, FVC, etc.
summary(COPD_data$FEV1)
hist(COPD_data$FEV1, main="Distribution of FEV1", xlab="FEV1", col="lightblue", border="black")

# Check for missing values in categorical variables
sum(is.na(COPD_data$COPDSEVERITY))  # Example for categorical variable
sum(is.na(COPD_data$smoking))  # Example for categorical variable

# Visualize other continuous variables
hist(COPD_data$FVC, main="Distribution of FVC", xlab="FVC", col="lightblue", border="black")

# 2. Examine the relationship between candidate predictor variables

# Calculate Pearson's pairwise correlations for continuous variables
cor(COPD_data[, c("AGE", "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")])

# Or, for Spearman correlations if needed
cor(COPD_data[, c("AGE", "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")], method="spearman")

# Visualize pairwise relationships using a scatterplot matrix
pairs(COPD_data[, c("AGE", "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")], 
      main="Scatterplot Matrix of Predictors")

# Cross-tabulate categorical variables to look for associations
CrossTable(COPD_data$COPDSEVERITY, COPD_data$smoking, prop.r = TRUE, prop.c = TRUE, prop.t = TRUE)

# Example of cross-tabulation for comorbidity data (IHD and Hypertension)
CrossTable(COPD_data$IHD, COPD_data$hypertension)

# 3. Fit Simple Linear Regression Models for Each Predictor

# Fit a simple linear regression model for AGE with MWT1Best
model_age <- lm(MWT1Best ~ AGE, data = COPD_data)
summary(model_age)  # Check results for AGE

# Fit a simple linear regression model for FEV1 with MWT1Best
model_fev1 <- lm(MWT1Best ~ FEV1, data = COPD_data)
summary(model_fev1)  # Check results for FEV1

# Fit a simple linear regression model for FVC with MWT1Best
model_fvc <- lm(MWT1Best ~ FVC, data = COPD_data)
summary(model_fvc)  # Check results for FVC

# Fit a simple linear regression model for CAT with MWT1Best
model_cat <- lm(MWT1Best ~ CAT, data = COPD_data)
summary(model_cat)  # Check results for CAT

# Fit a simple linear regression model for smoking with MWT1Best
model_smoking <- lm(MWT1Best ~ smoking, data = COPD_data)
summary(model_smoking)  # Check results for smoking

# Fit a simple linear regression model for PackHistory with MWT1Best
model_packhistory <- lm(MWT1Best ~ PackHistory, data = COPD_data)
summary(model_packhistory)  # Check results for PackHistory

# Fit a simple linear regression model for FEV1PRED with MWT1Best
model_fev1pred <- lm(MWT1Best ~ FEV1PRED, data = COPD_data)
summary(model_fev1pred)  # Check results for FEV1PRED

# Fit a simple linear regression model for SGRQ with MWT1Best
model_sgrq <- lm(MWT1Best ~ SGRQ, data = COPD_data)
summary(model_sgrq)  # Check results for SGRQ

# Check Confidence Intervals for each model
confint(model_age)  # Confidence interval for AGE model
confint(model_fev1)  # Confidence interval for FEV1 model
confint(model_fvc)   # Confidence interval for FVC model
confint(model_cat)   # Confidence interval for CAT model
confint(model_smoking) # Confidence interval for smoking model


# Load the libraries
library(car)    # For VIF calculation
library(Hmisc)  # For describe function

# Assuming COPD_data is already loaded

# 1. Inspect the dataset for missing values and outliers
# Check the first few rows of the dataset
head(COPD_data)

# Get summary statistics for the dataset
describe(COPD_data)

# Check missing values for each variable
colSums(is.na(COPD_data))

# For continuous variables, check the summary
summary(COPD_data$MWT1Best)
summary(COPD_data$FEV1)
summary(COPD_data$AGE)

# Visualize the distribution of continuous variables (histograms)
hist(COPD_data$AGE, main="Age Distribution", xlab="Age", col="lightblue", border="black")
hist(COPD_data$MWT1Best, main="Walking Distance (MWT1Best)", xlab="MWT1Best", col="lightgreen", border="black")

# 2. Examine the relationship between candidate predictor variables

# For continuous variables, calculate pairwise correlations
cor_matrix <- cor(COPD_data[, c("AGE", "FEV1", "MWT1Best", "FVC", "FEV1PRED")], use="complete.obs")
print(cor_matrix)

# Plot the correlation matrix
pairs(COPD_data[, c("AGE", "FEV1", "MWT1Best", "FVC", "FEV1PRED")])

# For categorical variables, use cross-tabulations
library(gmodels)
CrossTable(COPD_data$gender, COPD_data$COPDSEVERITY)

# 3. Fit a simple linear regression model for each variable in turn
model_age <- lm(MWT1Best ~ AGE, data = COPD_data)
summary(model_age)

model_FEV1 <- lm(MWT1Best ~ FEV1, data = COPD_data)
summary(model_FEV1)

# 4. Fit the multivariable regression model with selected predictors
model_multivariable <- lm(MWT1Best ~ FEV1 + AGE + gender + COPDSEVERITY + comorbid, data = COPD_data)

# Get the summary of the model
summary(model_multivariable)

# Check the adjusted R-squared
adjusted_r_squared <- summary(model_multivariable)$adj.r.squared
cat("Adjusted R-squared: ", adjusted_r_squared, "\n")

# 5. Assess multicollinearity using VIF (Variance Inflation Factor)
vif(model_multivariable)

# 6. Plot residuals to check regression assumptions
# Plot residuals
plot(model_multivariable$residuals, main="Residuals vs Fitted", ylab="Residuals", xlab="Fitted Values")

# Q-Q plot to check for normality of residuals
qqnorm(model_multivariable$residuals)
qqline(model_multivariable$residuals)

# 7. Refine the model if necessary
# Example of adding interaction term if needed (for example, FEV1 and AGE interaction)
model_refined <- lm(MWT1Best ~ FEV1 * AGE + gender + COPDSEVERITY + comorbid, data = COPD_data)

# Get the summary of the refined model
summary(model_refined)


#Running Multiple Regression
# Ensure that the variables are treated as integers in the COPD_data dataset
COPD_data$Diabetes <- as.integer(COPD_data$Diabetes)
COPD_data$AtrialFib <- as.integer(COPD_data$AtrialFib)
COPD_data$smoking <- as.integer(COPD_data$smoking)
COPD_data$gender <- as.integer(COPD_data$gender)
COPD_data$IHD <- as.integer(COPD_data$IHD)

# Create interaction variables
DAF <- COPD_data$Diabetes * COPD_data$AtrialFib
SG_interaction <- COPD_data$smoking * COPD_data$gender
IHD_Diabetes_interaction <- COPD_data$IHD * COPD_data$Diabetes

# Fit regression models

# Model with Diabetes, AtrialFib, and their interaction
r1 <- lm(MWT1Best ~ factor(Diabetes) + factor(AtrialFib) + factor(DAF), data = COPD_data)
summary(r1)
confint(r1)

# Model with Diabetes, AtrialFib, and the interaction between Diabetes and AtrialFib
r2 <- lm(MWT1Best ~ factor(Diabetes) + factor(AtrialFib) + factor(Diabetes * AtrialFib), data = COPD_data)
summary(r2)
confint(r2)

# Model with Smoking, Gender, and their interaction
r_smoking_gender <- lm(MWT1Best ~ factor(smoking) + factor(gender) + factor(SG_interaction), data = COPD_data)
summary(r_smoking_gender)
confint(r_smoking_gender)

# Model with IHD, Diabetes, and their interaction
r_IHD_diabetes <- lm(MWT1Best ~ factor(IHD) + factor(Diabetes) + factor(IHD_Diabetes_interaction), data = COPD_data)
summary(r_IHD_diabetes)
confint(r_IHD_diabetes)
