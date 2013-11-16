library(psych)
library(gclus)
library(rgl)

# What is the correlation between S1 and S2 pre-training?
cor(data[3:4])

# What is the correlation between V1 and V2 pre-training?
cor(data[7:8])

# Which test is most reliable?
cor(data$S1.pre, data$S1.post)
cor(data$S2.pre, data$S2.post)
cor(data$V1.pre, data$V1.post)
cor(data$V2.pre, data$V2.post)

#Does there appear to be a correlation between spatial reasoning before training and the amount of improvement in spatial reasoning?
data$S.pre = (data$S1.pre + data$S2.pre) / 2
data$S.post = (data$S1.post + data$S2.post) / 2
data$Sgain = data$S.post - data$S.pre

# Does there appear to be a correlation between verbal reasoning before training and the amount of improvement in verbal reasoning?
data$V.pre = (data$V1.pre + data$V2.pre) / 2
data$V.post = (data$V1.post + data$V2.post) / 2
data$Vgain = data$V.post - data$V.pre

# Which group exhibited more improvement in spatial reasoning?
data.aer$S.pre = (data.aer$S1.pre + data.aer$S2.pre) / 2
data.aer$S.post = (data.aer$S1.post + data.aer$S2.post) / 2
data.aer$Sgain = data.aer$S.post - data.aer$S.pre
data.des$S.pre = (data.des$S1.pre + data.des$S2.pre) / 2
data.des$S.post = (data.des$S1.post + data.des$S2.post) / 2
data.des$Sgain = data.des$S.post - data.des$S.pre
#OR
describeBy(data$Sgain, data$cond)

pairs(~data$S1.pre + data$S2.pre + data$V1.pre + data$V2.pre, cex
labels = 1.2)



