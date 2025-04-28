# Load necessary libraries
library(VIM)  # For visualizing missing data
library(mice)  # For imputation
library(car)  # For VIF calculation

# Load the dataset
COPD_data <- read.csv("C:/Users/user/Downloads/COPD_student_dataset.csv")

# Clean column names: Replace spaces with underscores and ensure proper case
colnames(COPD_data) <- gsub(" ", "_", colnames(COPD_data))  # Replace spaces with underscores
colnames(COPD_data) <- c("X", "ID", "AGE", "PackHistory", "COPDSEVERITY", "MWT1", "MWT2", "MWT1Best", 
                         "FEV1", "FEV1PRED", "FVC", "FVCPRED", "CAT", "HAD", "SGRQ", "AGEquartiles", 
                         "copd", "gender", "smoking", "Diabetes", "muscular", "hypertension", "AtrialFib", "IHD")

# Visualize missing data pattern
aggr(COPD_data, col = c("navyblue", "yellow"), numbers = TRUE, sortVars = TRUE, 
     labels = names(COPD_data), cex.axis = .7, gap = 3, ylab = c("Missing data", "Pattern"))

# Impute missing values using predictive mean matching (PMM)
imputed_data <- mice(COPD_data, method = "pmm", m = 5)
COPD_data_imputed <- complete(imputed_data, 1)

# Check summary and levels of categorical variables
summary(COPD_data_imputed)
table(COPD_data_imputed$gender)
table(COPD_data_imputed$smoking)
table(COPD_data_imputed$copd)

# Recode 'COPDSEVERITY' to combine 'Very Severe' and 'Severe'
COPD_data_imputed$COPDSEVERITY <- factor(COPD_data_imputed$COPDSEVERITY,
                                         levels = c("Mild", "Moderate", "Severe", "Very Severe"),
                                         labels = c("Mild", "Moderate", "Severe", "Severe"))

# Recode 'copd' as binary (0 = No COPD, 1 = Has COPD)
COPD_data_imputed$copd <- ifelse(COPD_data_imputed$copd == 1, 0, 1)
COPD_data_imputed$copd <- factor(COPD_data_imputed$copd, levels = c(0, 1))

# Fit the logistic regression model
logistic_model_comorbid <- glm(copd ~ AGE + FEV1 + FVC + gender + muscular + hypertension + AtrialFib + IHD, 
                               data = COPD_data_imputed, family = binomial)

# Check for multicollinearity using VIF
vif(logistic_model_comorbid)

# Check levels of categorical variables to ensure correct factor levels
table(COPD_data_imputed$gender)
table(COPD_data_imputed$comorbid)
table(COPD_data_imputed$COPDSEVERITY)

#Defining Comorbid as a group
COPD_data_imputed$comorbid <- factor(COPD_data_imputed$Diabetes | COPD_data_imputed$muscular | 
                                       COPD_data_imputed$hypertension | COPD_data_imputed$AtrialFib | 
                                       COPD_data_imputed$IHD, levels = c(0, 1))

# Convert variables to factors for logistic regression
COPD_data_imputed$gender <- factor(COPD_data_imputed$gender)
COPD_data_imputed$comorbid <- factor(COPD_data_imputed$comorbid)
COPD_data_imputed$COPDSEVERITY <- factor(COPD_data_imputed$COPDSEVERITY)

# Fit the final logistic regression model with interactions
logistic_model <- glm(copd ~ AGE + FEV1 + FVC + gender + comorbid + AGE:comorbid + FEV1:gender, 
                      family = binomial(link = "logit"), data = COPD_data_imputed)

# Print summary of the logistic regression model
summary(logistic_model)

# Access the coefficients of the model
logistic_model$coefficients

# Access the p-values for the model coefficients
summary(logistic_model)$coefficients[, 4]
