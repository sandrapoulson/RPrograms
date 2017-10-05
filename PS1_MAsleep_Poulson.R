#The file I'm using, MAsleep.cv, initially contains a set of 
#data representing the answer 22 students gave on how many hours
#of sleep (score) they obtained the night before questionnaire.
#
#calling variable Hours within file MAsleep.cv
MAsleep$Hours
#identify variable that is sum of hours
sumHours <- sum(MAsleep$Hours)
#write this into the dataset
MAsleep$sumHours <- sum(MAsleep$Hours)
#variable for number of students
N = 22
#calculating the mean by dividing the total hours by the number of
#students
meanHours <- sumHours/N
#write this into the dataset
MAsleep$meanHours <- sumHours/N
#R will also calculate the median hours for me!
#but this can be done by hand too by arranging the dataset of
#hours from least to greatest, divide 22 by 2 to get 11 to look
#in the middle of the dataset, and since the number of students
#questioned is even I want to take the average of the 11th and 
#12th numbers, which are 5 and 6, so my median is 5.5
#identifying variable that is median of set of hours
medianHours <- median(MAsleep$Hours)
#write into dataset for kicks
MAsleep$medianHours <- median(MAsleep$Hours)
#mode is more tricky in R, so I will move on to the next one
#
#to calculate variance, I first need to know the error, or deviation of each score
#from the sample mean, which is each score (Hours) minus the mean, and I will also
#write this into the dataset in one step
MAsleep$error <- MAsleep$Hours - MAsleep$meanHours
#then create squares of the error
MAsleep$errorSqu <- MAsleep$error * MAsleep$error
#and now that I have squares, I can calculate the sum of squares
MAsleep$SumSquaresError <- sum(MAsleep$errorSqu)
#now caclulating variance: sum of squares of error divided by 
#number of students questioned
#this is different slightly from what we did in class because I'm calling the
#variable from the .csv file rather than calculating it here in the script and
#then inputting it into the .csv file, just depends on what you want to see
#because it can be visualized with this method in the .csv file this script outputs
MAsleep$hoursVar <- (MAsleep$SumSquaresError)/N
#standard deviation is calculated by taking the square root of the variance
MAsleep$hoursSD <- sqrt(MAsleep$hoursVar)
#then calculate standard error of the mean (SEM) as the standard deviation
#divided by the square root of N students
MAsleep$hoursSEM <- (MAsleep$hoursSD)/sqrt(N)
#calculate the Z scores as the error divided by the SD
MAsleep$Zscore <- MAsleep$error / MAsleep$hoursSD
#then I'm going to create an awesome histogram of the number of hours students slept
hist(MAsleep$Hours,
     main="Student Hours of Sleep",
     xlab="Hours",
     border="green",
     col="blue",
     breaks=seq(1, 14, 1))
#also plot the raw data as a density function
plot(density(MAsleep$Hours),
     main="Density function of Hours",
     col="green")
