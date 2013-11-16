# Histograms
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
hist(data$salary)
hist(data$years)
hist(data$courses)

# What is the correlation between salary and years of professional experience?
round(cor(data[2:3]),2)

# What is the correlation between salary and courses completed?
round(cor(data[,2], data[,4]))

# What is the percentage of variance explained in a regression 
# model with salary as the outcome variable and professional 
# experience as the predictor variable?
model1 <- lm(data$salary ~ data$years)
summary(model1)

plot(data$salary ~ data$years, main = "Salary vs. Years Experience", ylab = "Salary", xlab = "Experience (Years)")
abline(model1, col="blue")


# Compared to the model from Question 3, would a regression model predicting 
# salary from the number of courses be considered a better fit to the data?
model1 <- lm(data$salary ~ data$courses)
summary(model2)

plot(data$salary ~ data$courses, main = "Salary vs. Courses Completed", ylab = "Salary", xlab = "# Courses Completed")
abline(model2, col="blue")


# Now let's include both predictors (years of professional experience and 
# courses completed) in a regression model with salary as the outcome. 
# Now what is the percentage of variance explained?
model3 <- lm(data$salary ~ data$years + data$courses)
summary(model3)

# Use the model to get predicted values
data$predicted <- fitted(model3)
plot(data$salary ~ data$predicted, main = "Predicted Salary", ylab = "Salary", xlab = "Predicted Salary")
abline(lm(data$salary ~ data$predicted), col="blue")

# Determine residuals and plot
data$e <- resid(model3)
hist(data$e)
plot(data$salary ~ data$e, main = "Predicted vs. Residuals", ylab = "Model 3 Predicted Scores", xlab = "Model 3 Residuals")
abline(lm(data$predicted ~ data$e), col="blue")

# What is the standardized regression coefficient for years of 
# professional experience, predicting salary?
model1.z <- lm(scale(data$salary) ~ scale(data$years))
summary(model1.z)

# What is the standardized regression coefficient for courses 
# completed, predicting salary?
model2.z <- lm(scale(data$salary) ~ scale(data$courses))
summary(model2.z)

# What is the mean of the salary distribution predicted by the model 
# including both years of professional experience and courses completed 
# as predictors? (with 0 decimal places)
mean(data$predicted)

# What is the mean of the residual distribution for the model predicting 
# salary from both years of professional experience and courses completed?


