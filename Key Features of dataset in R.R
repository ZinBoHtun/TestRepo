# Load the dataset from the specified path
COPD_data <- read.csv("C:/Users/user/Downloads/COPD_student_dataset.csv")

# Check the number of rows and columns
dim(COPD_data)

# Print the first few rows of the dataset
head(COPD_data)

# Inspect the variable types for all columns
str(COPD_data)

# Inspect a continuous variable (e.g., Age)
# Check the class of the variable
class(COPD_data$AGE)

# Get a summary of the continuous variable
summary(COPD_data$AGE)

# Check the class of the 'Age' variable
class(COPD_data$AGE)

# If 'Age' is a factor or character, convert it to numeric
COPD_data$AGE <- as.numeric(as.character(COPD_data$AGE))

# Create a histogram to visualize the distribution of Age
hist(COPD_data$AGE, main = "Age Distribution", xlab = "Age", col = "lightblue", border = "black")

# Check for missing values
sum(is.na(COPD_data$AGE))

# Check for a categorical variable (e.g., COPDSEVERITY)
# Check if it's a factor
class(COPD_data$COPDSEVERITY)

# Get the frequency distribution of COPDSEVERITY
table(COPD_data$COPDSEVERITY)

# Check if there are missing values in COPDSEVERITY
sum(is.na(COPD_data$COPDSEVERITY))

# Check a binary variable (e.g., Gender)
# Check if it's a factor
class(COPD_data$gender)

# Get the distribution of the binary variable (Gender)
table(COPD_data$gender)

# Visualize the distribution of the binary variable with a bar plot
barplot(table(COPD_data$gender), main = "Gender Distribution", col = c("lightblue", "lightgreen"))

# For any other continuous variables, repeat the inspection (e.g., for CAT score)
summary(COPD_data$CAT)

# Inspect if there are any outliers in the CAT variable
hist(COPD_data$CAT, main = "CAT Score Distribution", xlab = "CAT Score", col = "lightblue", border = "black")

# If necessary, check for outliers in other variables by using summary statistics and visualization
