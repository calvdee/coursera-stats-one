# Subsets
post.ds <- subset(post, post$condition == "DS")
post.pe <- subset(post, post$condition == "PE")
post.wm  <- subset(post, post$condition == "WM")
pre.ds <- subset(pre, post$condition == "DS")
pre.pe <- subset(pre, post$condition == "PE")
pre.wm  <- subset(pre, post$condition == "WM")

# Histograms
par(mfrow = c(3,3))
hist(pre.wm$SR, xlab="WM Pre", main="")
hist(pre.pe$SR, xlab="PE Pre", main="")
hist(pre.pe$SR, xlab="DS Pre", main="")
hist(post.wm$SR, xlab="WM Post", main="")
hist(post.pe$SR, xlab="PE Post", main="")
hist(post.ds$SR, xlab="DS Post", main="")

# Density Plots
par(mfrow = c(3,3))
plot(density(pre.wm$SR), xlab="WM Pre", main="")
plot(density(pre.pe$SR), xlab="PE Pre", main="")
plot(density(pre.ds$SR), xlab="DS Pre", main="")

plot(density(post.wm$SR), xlab="WM Post", main="")
plot(density(post.pe$SR), xlab="PE Post", main="")
plot(density(post.ds$SR), xlab="DS Post", main="")


# Biggest Gains
mean(post.wm$SR) - mean(pre.wm$SR)
mean(post.pe$SR) - mean(pre.pe$SR)
mean(post.ds$SR) - mean(pre.ds$SR)

