# Set the file path
file_path <- "C:/Users/user/Downloads/cancer-data-for-MOOC-1-_1.csv"

# Read the CSV file
data <- read.csv(file_path)

# View the first few rows of the data
head(data)

tail (data)

# Load necessary libraries
library(dplyr)

# Read the dataset
file_path <- "C:/Users/user/Downloads/cancer-data-for-MOOC-1-_1.csv"
data <- read.csv(file_path)

# Check the distribution of Age and BMI
# Histogram for Age
hist(data$age, main = "Age Distribution", xlab = "Age", col = "lightblue", border = "black")

# Histogram for BMI
hist(data$bmi, main = "BMI Distribution", xlab = "BMI", col = "lightgreen", border = "black")

# Check missing values in 'smoking' column
missing_smoking <- sum(is.na(data$smoking))

# If there are missing values in smoking, drop rows with missing data
if (missing_smoking > 0) {
  data <- data %>% filter(!is.na(smoking))
}

# Check BMI by gender
average_bmi_by_gender <- data %>%
  group_by(gender) %>%
  summarise(average_bmi = mean(bmi, na.rm = TRUE))

# Print the average BMI by gender
print(average_bmi_by_gender)

# Check if BMI is higher in males (gender = 0) on average
# Gender '0' is male, '1' is female
if (average_bmi_by_gender$average_bmi[average_bmi_by_gender$gender == 0] > average_bmi_by_gender$average_bmi[average_bmi_by_gender$gender == 1]) {
  cat("BMI is higher in males on average.\n")
} else {
  cat("BMI is higher in females on average.\n")
}

# Summarize the findings
cat("Missing values in smoking data:", missing_smoking, "\n")
cat("Age and BMI are mostly normally distributed based on histograms.\n")
cat("BMI is higher in males on average based on the summary.\n")

# Load necessary libraries
library(ggplot2)

# Read the dataset
file_path <- "C:/Users/user/Downloads/cancer-data-for-MOOC-1-_1.csv"
data <- read.csv(file_path)

# Remove rows with missing smoking data (if any)
data <- data %>% filter(!is.na(smoking))

# Logistic regression: smoking against cancer
model <- glm(cancer ~ smoking, data = data, family = binomial)

# Summary of the model
summary(model)

# Plotting the regression using ggplot2
ggplot(data, aes(x = smoking, y = cancer)) +
  geom_point(aes(color = factor(cancer)), alpha = 0.7) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "blue") +
  labs(title = "Regression between Smoking and Cancer",
       x = "Smoking (0 = No, 1/2 = Yes)",
       y = "Cancer (0 = No, 1 = Yes)",
       color = "Cancer") +
  theme_minimal()

# Load necessary libraries
library(ggplot2)
library(GGally)

# Read the dataset
file_path <- "C:/Users/user/Downloads/cancer-data-for-MOOC-1-_1.csv"
data <- read.csv(file_path)

# Remove rows with missing smoking data (if any)
data <- data %>% filter(!is.na(smoking))

# Create a scatter plot matrix using ggpairs (pairs plot)
ggpairs(data, columns = c("gender", "age", "smoking", "cancer"), 
        aes(color = factor(cancer), alpha = 0.7),
        upper = list(continuous = "points", combo = "dot"),
        lower = list(continuous = "points", combo = "dot")) +
  labs(title = "Scatter Plot Matrix of Gender, Age, Smoking, and Cancer") +
  theme_minimal()

# Load necessary library
library(ggplot2)

# Read the dataset
file_path <- "C:/Users/user/Downloads/cancer-data-for-MOOC-1-_1.csv"
data <- read.csv(file_path)

# Remove rows with missing smoking data (if any)
data <- data %>% filter(!is.na(smoking))

# Plot histograms for each variable

# Histogram for Age
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency") +
  theme_minimal()

# Histogram for BMI (if you want to add BMI)
ggplot(data, aes(x = bmi)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of BMI", x = "BMI", y = "Frequency") +
  theme_minimal()

# Histogram for Smoking (0 = No, 1/2 = Yes)
ggplot(data, aes(x = smoking)) +
  geom_bar(fill = "salmon", color = "black") +
  labs(title = "Histogram of Smoking Status", x = "Smoking (0 = No, 1/2 = Yes)", y = "Count") +
  theme_minimal()

# Histogram for Cancer (0 = No, 1 = Yes)
ggplot(data, aes(x = cancer)) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Histogram of Cancer Status", x = "Cancer (0 = No, 1 = Yes)", y = "Count") +
  theme_minimal()

# Histogram for Gender (0 = Male, 1 = Female)
ggplot(data, aes(x = gender)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Gender", x = "Gender (0 = Male, 1 = Female)", y = "Count") +
  theme_minimal()

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the dataset
file_path <- "C:/Users/user/Downloads/cancer-data-for-MOOC-1-_1.csv"
data <- read.csv(file_path)

# Remove rows with missing smoking data (if any)
data <- data %>% filter(!is.na(smoking))

# Logistic regression: cancer against age, gender, and smoking
model <- glm(cancer ~ age + gender + smoking, data = data, family = binomial)

# Summary of the model
summary(model)

# Create a new data frame for prediction values
data$predicted_cancer_prob <- predict(model, type = "response")

# Plot the regression (age vs. predicted cancer probability) with gender and smoking
ggplot(data, aes(x = age, y = predicted_cancer_prob, color = factor(gender), shape = factor(smoking))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Logistic Regression: Predicted Cancer Probability vs. Age, Gender, and Smoking",
       x = "Age",
       y = "Predicted Cancer Probability",
       color = "Gender (0 = Male, 1 = Female)",
       shape = "Smoking (0 = No, 1/2 = Yes)") +
  theme_minimal()

# Load necessary libraries
library(dplyr)

# Read the dataset
file_path <- "C:/Users/user/Downloads/cancer-data-for-MOOC-1-_1.csv"
data <- read.csv(file_path)

# Remove rows with missing smoking data (if any)
data <- data %>% filter(!is.na(smoking))

# Logistic regression: cancer against smoking
model <- glm(cancer ~ smoking, data = data, family = binomial)

# Summary of the model to extract p-value
summary(model)

# Extract p-value for smoking
p_value_smoking <- summary(model)$coefficients[2, 4]

# Print the p-value for smoking
cat("P-value for smoking:", p_value_smoking, "\n")

# Load necessary libraries
library(readr)

# Load the data
data <- read.csv("C:/Users/user/Downloads/cancer-data-for-MOOC-1-_1.csv")

# Check the structure of the dataset to ensure correct loading
str(data)

# Recode smoking variable: 0 -> No Smoking, 1 or 2 -> Smoking
data$smoking_status <- ifelse(data$smoking == 0, "No Smoking", "Smoking")

# Perform t-test for Cancer status vs Smoking Status (only for cancer = 1, cancer = 0)
t_test_smoking_cancer <- t.test(cancer ~ smoking_status, data = data)
print("T-test result for Cancer vs Smoking status:")
print(t_test_smoking_cancer)


# Check column names to ensure correct names are being used
names(data)

# Check if there are any missing values in the 'bmi' column
summary(data$bmi,mu=25)

# Ensure 'bmi' is numeric
data$bmi <- as.numeric(data$bmi,mu=25)

# Run the t-test
t.test(bmi ~ cancer, data = data)

# Load your data (make sure the file path is correct)
data <- read.csv("C:/Users/user/Downloads/cancer-data-for-MOOC-1-_1.csv")

# Create a new variable for overweight status (1 = Overweight, 0 = Not overweight)
data$overweight <- ifelse(data$bmi > 25, 1, 0)

# Perform a Chi-Squared test to compare overweight proportions between cancer and no cancer groups
# Assuming 'cancer' is 1 for cancer patients and 0 for non-cancer patients
table_overweight_cancer <- table(data$cancer, data$overweight)

# Perform the Chi-Squared Test
chi_squared_result <- chisq.test(table_overweight_cancer)

# View the results
chi_squared_result

