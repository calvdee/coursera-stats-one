library(psych)
data <- read.table('data.tsv', header=T)

# In a model predicting salary, what is the unstandardized regression coefficient for years, 
# assuming years is the only predictor variable in the model?
model1 <- lm(data$salary ~ data$years)
summary(model1)
# Answer: 5638

# In a model predicting salary, what is the 95% confidence interval for the unstandardized 
# regression coefficient for years, assuming years is the only predictor variable in the model?
confint(model1)
# Answer: 4930 6345

# In a model predicting salary, what is the unstandardized regression coefficient for years, 
# assuming years and courses are both included as predictor variables in the model?
model2 <- lm(data$salary ~ data$years + data$courses)
summary(model2)
#Answer: 4807

# In a model predicting salary, what is the 95% confidence interval for the unstandardized 
# regression coefficient for years, assuming years and courses are both included as predictor 
# variables in the model?
confint(model2)
#Answer: 4140 5473

# What is the predicted difference in salary between Doctors and Lawyers assuming an equal 
# and average number of years and courses?
prof <- C(data$profession, treatment)
model3 <- lm(data$salary ~ data$years + data$courses + (prof))
#Answer: 9204

# Is the predicted difference between Doctors and Lawyers statistically significant?
#Answer: Yes, p-value

# What is the predicted difference in salary between Doctors and Teachers assuming an 
# equal and average number of years and courses?
#Answer: 15903

# Is the predicted difference between Doctors and Teachers statistically significant?
#Answer: Yes, p-value

# What is the actual difference in mean salary between Doctors and Teachers?
tapply(data$salary, data$profession, mean)
#Answer: 24611

# What combination of predictors represents the best model in terms of predicting salary?
# 1 - Years and Courses
# 2 - Years and Profession
# 3 - Courses and Profession
# 4 - Years, Courses, and Profession
anova(years_courses, years_prof)
anova(years_prof, all)
#Answer: Years, Courses, Profession