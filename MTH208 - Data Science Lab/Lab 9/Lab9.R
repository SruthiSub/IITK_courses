# Worksheet 9

# Problem 1
# a) Population - all the orders received in 2022, Sample - the orders we are using for analysis (csv file is a sample)
# b) Population - all the voters, Sample - the voters interviewed in the exit poll
# c) Population - all Data Scientists in India, Sample - the jobs posted for a Data Scientist on Glassdoor

# Problem 2
random_sample <- sample(100, size = 10)
random_sample
sample_mean <- mean(random_sample)
sample_variance <- var(random_sample)
sample_mean
sample_variance

reps <- 1000
pop <- 1:100
mu <- mean(pop)
sig2 <- var(pop)
means <- numeric(length = reps)
vars <- numeric(length = reps)
for (r in 1:reps)
{
  random_sample <- sample(100, size = 10)
  means[r] <- mean(random_sample)
  vars[r] <- var(random_sample) 
}

mean(means)
mean(vars)
mu
sig2

?sample

# Problem 3
# Implementing Problem 2 by Sampling With and without Replacement:
reps <- 1000
pop <- 1:100
mu <- mean(pop)
sig2 <- var(pop)
means <- numeric(length = reps)
vars <- numeric(length = reps)
for (r in 1:reps)
{
  random_sample <- sample(100, size = 10, replace = TRUE)
  means[r] <- mean(random_sample)
  vars[r] <- var(random_sample) 
}

mean(means)
mean(vars)
mu
sig2

means <- numeric(length = reps)
vars <- numeric(length = reps)
for (r in 1:reps)
{
  random_sample <- sample(100, size = 10, replace = FALSE)
  means[r] <- mean(random_sample)
  vars[r] <- var(random_sample) 
}

mean(means)
mean(vars)
mu
sig2

# Problem 4
hall4 <- read.csv("hall4.csv")
# One strata for each day of the week:
means <- numeric(length = 7)
variance <- numeric(length = 7)
l <- numeric(length = 7)
for (i in 1:7)
{
  day <- hall4[which(hall4[,2] == i),]
  daymean <- mean(day[,1])
  dayvar <- var(day[,1])
  means[i] <- daymean
  variance[i] <- dayvar
  l[i] <- length(which(hall4[,2] == i))
}
means
variance

# Problem 5
# weighted mean of the individual means
t <- sum(l)
p <- l/t
weighted_mean <- 0
for (i in 1:7)
{
  weighted_mean <- weighted_mean + p[i]*means[i]
}
weighted_mean