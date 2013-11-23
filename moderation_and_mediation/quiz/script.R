setwd("~/code/r/stats-one/moderation_and_mediation/quiz/")
data <- read.table("data.tsv", header=T)
 
# What is the correlation between extraversion and happiness? 0.19
cor(data$happy, data$extra)

# What is the correlation between extraversion and diversity of life experience? 0.21
cor(data$extra, data$diverse)

# What is the correlation between diversity of life experience and happiness? 0.21
cor(data$happy, data$diverse)

# What percentage of variance in happiness is explained by extraversion? 4
model0 <- lm(data$happy ~ data$extra)
summary(model0)

# What percentage of variance in happiness is explained by a model with both extraversion and diversity of life experience as predictors? 7
model1 <- lm(data$happy ~ data$extra + data$diverse)
summary(model1)

# What is the 95% confidence interval for the regression coefficient for extraversion when it is the only predictor of happiness? .07 .48
confint(model0)

# What is the 95% confidence interval for the regression coefficient for extraversion when it and diversity of life experience are both predictors of happiness? .02 .43
confint(model1)

# What is the unstandardized regression estimate of the indirect effect? .03
indirect <- sobel(data$extra, data$diverse, data$happy)
indirect

# What is the z-value of the Sobel test? 1.88
indirect

# Do these analyses suggest full mediation, partial mediation, or no mediation? partial