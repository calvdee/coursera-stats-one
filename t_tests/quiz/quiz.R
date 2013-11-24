# Compare the impact of three training conditions (Working Memory training, Physical Exercise, and Designed Sport) 
# on Spatial Reasoning (SR), measured before (pre) and after (post) training.

library(psych)
library(lsr)
library(reshape)
library(car)
setwd("~/code/r/stats-one/t_tests/quiz/")
data <- read.table("data.tsv", header=T)

# Using a dependent t-test, is the difference between pre and post-test scores significant?
data.pre  <- data[data$time == "pre",]
data.post <- data[data$time == "post",]
t.test(data.pre$SR, data.post$SR, paired=T)
# Yes, p < .05

# Create subsets for each training condition. 
# Which group shows no difference between pre- and post-test scores?
#   - How the data changed from pre to post (time) = data$SR ~ data$time
pre <- describeBy(data.pre, data.pre$condition)
post <- describeBy(data.post, data.post$condition)
wm <- subset(data, data$condition == "WM")
pe <- subset(data, data$condition == "PE")
ds <- subset(data, data$condition == "DS")
t.test(wm$SR ~ wm$time, paired=T)
t.test(pe$SR ~ pe$time, paired=T)
t.test(ds$SR ~ ds$time, paired=T)
# PE

# Which training group shows the largest effect size for the difference pre-test to post-test?
#   - Test effect size using Cohen's D (Mean of difference scores / Standard deviation of difference scores)
#   - Is the change is spatial reasoning significant?
cohensD(wm$SR ~ wm$time, method="paired")
cohensD(pe$SR ~ pe$time, method="paired")
cohensD(ds$SR ~ pe$time, method="paired")
# DS

# Reshape the data into a wide format, and create a new variable for gain score. 
#   - Melt the data set, creating columns for subject, condition, and 2 time groups (pre and post)
data.wide <- cast(data, subject+condition ~ time)
data.wide$gain <- data.wide$post - data.wide$pre
# Now subset the new dataframe based on the training conditions. 
wm.wide <- subset(data.wide, data.wide$condition == "WM")
pe.wide <- subset(data.wide, data.wide$condition == "PE")
ds.wide <- subset(data.wide, data.wide$condition == "DS")
# Which comparison between training conditions does not show a significant difference?
#   - Observe differences between groups (WM, PE, DS)
t.test(ds.wide$gain, wm.wide$gain, paired=T)
t.test(wm.wide$gain, pe.wide$gain, paired=T)
t.test(pe.wide$gain, ds.wide$gain, paired=T)
# DS and WM

# To compare the gain scores across all groups, we now turn to ANOVA. 
# Is the homogeneity of variance assumption violated?
#   - Are SD's equivalent?
#   - In conducting Levene's test, there should not be significant difference in gain score (SD's should be equivelant)
leveneTest(data.wide$gain, data.wide$condition, center="mean")
# No, homogeneity of variance is not violated, difference accross conditions is not signficant

# Run an ANOVA model on the gain scores as a function of training condition. 
# Is the effect of condition significant?
model <- aov(data.wide$gain ~ data.wide$condition)
summary(model)
# Yes, p < 0.05

# What is the corresponding eta-squared value? (round to 2 decimal places)
etaSquared(model, anova=F)
# 0.34

# Are the eta-squared and partial eta-squared value different in this case?
# No, eta = 0.3383799 and part = 0.3383799

# Let's now run post-hoc comparisons (Tukey HSD). 
# Which two groups do not significantly differ from one another when considering gain scores?
#   - Evaulate all pairwise comparisons
TukeyHSD(model)
# WM and DS, p > .05

# Based on these data, which training condition should you choose to target 
# some improvements in spatial reasoning?