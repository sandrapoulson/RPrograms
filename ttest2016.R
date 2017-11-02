dat <- read.csv("~/Dropbox/classes_2016/GLM/ttest_data.csv")
attach(dat)

dat$difference <- dat$post - dat$pre
meanDiff <- mean(dat$difference)

dat$B0 <- meanDiff
dat$e <- dat$difference - dat$B0

N = length(dat$difference)
dat$e2 <- dat$e * dat$e
SampleVariance <- sum(dat$e2)/N

df <- N - 1
estPopVariance <- sum(dat$e2)/df

estPopStd <- sqrt(estPopVariance)

# Wil's favourite thing! CENTRAL LIMIT!!!!!!!
# Standard Error or the sampling distribution of means

SE <- estPopStd/sqrt(N)

t.1 <- meanDiff/SE

t.2 <- lm(dat$difference ~ 1)
summary(t.2)


r.1 <- cor.test(dat$pre,dat$post)
r.2 <- lm(dat$pre ~ dat$post)
r.3 <- lm(dat$pre ~ 1 + dat$post)

# scale - z-scores a variable

r.4 <- lm(scale(dat$pre) ~ scale(dat$post))


# independent samples ttest

dat <- read.csv("~/Dropbox/classes_2016/GLM/ttest_data2.csv")
attach(dat)

t.1 <- lm(dat$pre ~ 1 + dat$condition)
cor.test(dat$pre, dat$condition2)
