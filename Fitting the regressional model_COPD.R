#Step 1: Fitting the Linear Regression Model
#model_name <- lm(outcome_variable ~ predictor_variable, data = dataframe)
MWT1Best_FEV1 <- lm(MWT1Best ~ FEV1, data = COPD_data)
#Step 2: View the Summary of the Model
summary(MWT1Best_FEV1)
#Step 3: View the 95% Confidence Intervals for the Coefficients
confint(MWT1Best_FEV1)
#Step 4: Check Model Assumptions
plot(MWT1Best_FEV1)
#Step 5: Viewing Multiple Plots
par(mfrow=c(2,2))  # Set up a 2x2 plotting window
plot(MWT1Best_FEV1)  # Create the plots
par(mfrow=c(1,1))  # Return to single plot per page

#Running the Multiple Linear Regression Model
MWT1Best_FEV1_AGE <- lm(MWT1Best ~ FEV1 + AGE, data = COPD_data)
summary(MWT1Best_FEV1_AGE)
#Calculating the 95% Confidence Intervals for the Coefficients
confint(MWT1Best_FEV1_AGE)
#Model Assumptions
par(mfrow=c(2,2))  # Arrange the plots in a 2x2 grid
plot(MWT1Best_FEV1_AGE)



