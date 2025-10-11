# Worksheet 10

# Descriptive measures of statistics

# Measures of Central tendency - sample mean, sample median, sample mode
# Positive Skew => mode < median < mean
# Symmetric Distribution => mode = median = mean
# Negative Skew => mode > median > mean

# Problem 1
movies <- read.csv("movie_unweighted.csv")

# Problem 2 
?hist
hist(movies$ratings, main = "Histogram of Ratings", xlab = "Ratings")
hist(movies$ratings, main = "Histogram of Ratings", xlab = "Ratings", col = "white")
ratings_mean <- mean(movies$ratings)
ratings_median <- median(movies$ratings)
unweighted_mean <- mean(movies$unweighted)
unweighted_median <- median(movies$unweighted)
par(mfrow = c(1,2))
hist(movies$ratings, main = "Histogram of Ratings", xlab = "Ratings", col = "white",xlim = c(7.5,10))
abline(v = ratings_mean, col = "red")
abline(v = ratings_median, col = "blue")
hist(movies$unweighted, main = "Histogram of Unweighted", xlab = "Unweighted", col = "white",xlim = c(7.5,10))
abline(v = unweighted_mean, col = "red")
abline(v = unweighted_median, col = "blue")
# Both data points are positively skewed
# From the graphs, we can see that the modal classes are:
# Ratings => 8-8.2, Unweighted => 8-8.2

# Measures of dispersion - sample variance, range, interquartile range
var(movies$ratings)
var(movies$unweighted)

# Problem 3
?boxplot
par(mfrow = c(1,1))
boxplot(movies$ratings, main = "Boxplot of Ratings")
boxplot(movies$ratings, main = "Boxplot of Ratings", col = "pink")

range_ratings <- max(movies$ratings) - min(movies$ratings)
range_unweighted <- max(movies$unweighted) - min(movies$unweighted)
range_ratings
range_unweighted
?quantile
quantile_ratings <- as.matrix(quantile(movies$ratings))
quantile_unweighted <- as.matrix(quantile(movies$unweighted))
IQR_ratings <- quantile_ratings[4] - quantile_ratings[2]
IQR_unweighted <- quantile_unweighted[4] - quantile_unweighted[2]
IQR_ratings
IQR_unweighted

# Problem 4
?boxplot
boxplot(movies$ratings, movies$unweighted, beside = T, xlab = "Boxplots of Ratings and Unweighted")

# Problem 5
hist(movies$unweighted, main = "Histogram of Unweighted", xlab = "Unweighted", col = "white", xlim = c(7.5,10))
hist(movies$ratings, main = "Histogram of Ratings", xlab = "Ratings", col = adjustcolor("red", alpha.f = 0.5), xlim = c(7.5,10), add = TRUE)
legend("right", c("Unweighted", "Ratings"), fill = c("white", adjustcolor("red", alpha.f = 0.5)))

# Problem 6
# Both are right skewed, Ratings being more right skewed than Unweighted