ttest_data$difference <- ttest_data$post - ttest_data$pre
#lay out your models and hypotheses
#then calculate mean
meanDiff <- mean(ttest_data$difference)
#add the mean Bo
ttest_data$BO <- meanDiff
#add the errors
ttest_data$e <- ttest_data$difference - ttest_data$BO
#add the variance, e*e
ttest_data$e2 <- ttest_data$e * ttest_data$e
# calculating degrees of freedom
N = length(ttest_data$difference)
df <- N - 1
#this gives me 9 degrees of freedom
#estimate sample variance, population variance
SampleVariance <- sum(ttest_data$e2)/N
estPopVariance <- sum(ttest_data$e2)/df
#central limit theorem
estPopStd <- sqrt(estPopVariance)
#calculate standard error (putting together the t-test)
SE <- estPopStd/sqrt(N)
#first t-test will run, save so can compare
#comes out large, rather cheaty, why
t.1 <- meanDiff/SE
#second t-test, the only test you'll need for GLM if you use R...
#lm has more flexibility than glm
#~ is = in the R glm function, has to do with linear algebra, ask Jack
t.2 <- lm(ttest_data$difference ~ 1)
#this gives you estimations in the console
summary(t.2)
