setwd("C:/Users/Calvin/Dropbox/code/r/confidence_intervals/quiz")
library(psych)
library(ggplot2)
data <- read.table("data.tsv", header=T)

# Salary is outcome years is predictor, what is the 95% confidence interval for the regression coefficient? 
model1 = lm(data$salary ~ data$years)
confint(model1)

# Salary is outcome courses is predictor, what is the 95% confidence interval for the regression coefficient? 
model2 = lm(data$salary ~ data$courses)
confint(model2)

# Run a multiple regression model with both predictors and compare it with both the model from Question 1 
# and the model from Question 2. Is the model with both predictors significantly better than:
# 1 - both single predictor models
# 2 - the single predictor model based on years of experience
# 3 - the single predictor model based on courses
# 4 - none of the above
anova(model1, model3)
anova(model2, model3)

# Run a standardized multiple regression model with both predictors. 
# Do the confidence interval values differ from the corresponding unstandardized model?
model3.z = lm(scale(data$salary) ~ scale(data$years) + scale(data$courses))

# Take a random subset of the original data so that N=15. 
# Is the correlation coefficient between salary and years of experience in this sample 
# higher or lower than in the whole data set?
set.seed(1)
data.sample <- data[sample(nrow(data), 15),]
model1.n15 = lm(data.sample$salary ~ data.sample$years)
summary(model1.n15)

# Take a subset of the original data from row 51 to 70. 
# What is the percentage of variance explained by a multiple regression model 
# with both predictors (Provide your result with no decimal place)
subset <- data[51:70,]
model3.subset <- lm(subset$salary ~ subset$years + subset$courses)
summary(model3.subset)

# Using model comparison, which model provides the best fit for the subsetted data from Question 7?
# 1 - model1.subset = lm(data.subset$salary ~ data.subset$years)
# 2 - model2.subset = lm(data.subset$salary ~ data.subset$courses)
# 3 - model3.subset = lm(data.subset$salary ~ data.subset$years + data.subset$courses)
# 4 - They are all equal
3

# What is the correlation between the salary values predicted by the multiple regression 
# model and the actual salary scores in the subsetted data? 
# Provide your result rounded to 2 decimal places)
data.subset$predicted = fitted(model3.subset)
cor(data.subset$salary, data.subset$predicted)

# Compute the correlation between the scores predicted by the multiple regression model 
# and the residuals from the same model. Is the correlation statistically significant?
data.subset$error = resid(model3.subset)
cor(data.subset$predicted, data.subset$error)

