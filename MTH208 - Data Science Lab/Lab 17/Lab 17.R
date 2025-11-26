# Worksheet 17

# Problem 1
auto <- read.csv("auto-mpg.csv")
cor(auto$acceleration, auto$mpg)
# moderately positively correlated

# Problem 2
plot(auto$acceleration, auto$mpg)
abline(lm(auto$mpg ~ auto$acceleration))

# Problem 3
plot(auto$acceleration, auto$mpg, col = auto$cylinders)
y <- auto$mpg[which(auto$cylinder == 8)]
x <- auto$acceleration[which(auto$cylinder == 8)]
abline(lm(y ~ x))
y <- auto$mpg[which(auto$cylinder == 4)]
x <- auto$acceleration[which(auto$cylinder == 4)]
abline(lm(y ~ x))
y <- auto$mpg[which(auto$cylinder == 6)]
x <- auto$acceleration[which(auto$cylinder == 6)]
abline(lm(y ~ x))
unique(auto$cylinders)

cor(auto$mpg[which(auto$cylinders == 4)],auto$acceleration[which(auto$cylinders == 4)])
cor(auto$mpg[which(auto$cylinders == 6)],auto$acceleration[which(auto$cylinders == 6)])
cor(auto$mpg[which(auto$cylinders == 8)],auto$acceleration[which(auto$cylinders == 8)])

# there is not much correlation between the two, once we split the data based on cylinders

# This is an example of Simpson's Paradox!
# - when a trend appearing in a group disappears when the data is combined
# i.e the joint distribution of two variables can change drastically when conditioned on a third random variable

# Problem 4
# simpsons paradox in tennis? 
# player can have more number of winners, first serve percentage, total points won and other vital statistics but he/she eventually ends up losing the overall match.

# Problem 5
iris
names(iris)
plot(iris)
plot(iris, col = iris$Species)
# petal.width and sepal.length is an instance of simpson's paradox
cor(iris$Petal.Width, iris$Sepal.Length)
cor(iris$Petal.Width[which(iris$Species == "virginica")], iris$Sepal.Length[which(iris$Species == "virginica")])
cor(iris$Petal.Width[which(iris$Species == "versicolor")], iris$Sepal.Length[which(iris$Species == "versicolor")])
cor(iris$Petal.Width[which(iris$Species == "setosa")], iris$Sepal.Length[which(iris$Species == "setosa")])
plot(iris$Sepal.Length, iris$Petal.Width)
abline(lm(iris$Petal.Width ~ iris$Sepal.Length))
plot(iris$Sepal.Length, iris$Petal.Width, col = iris$Species)
y <- iris$Petal.Width[which(iris$Species == "setosa")]
x <- iris$Sepal.Length[which(iris$Species == "setosa")]
abline(lm(y ~ x), col = "black")
y <- iris$Petal.Width[which(iris$Species == "versicolor")]
x <- iris$Sepal.Length[which(iris$Species == "versicolor")]
abline(lm(y~x), col = "pink")
y <- iris$Petal.Width[which(iris$Species == "virginica")]
x <- iris$Sepal.Length[which(iris$Species == "virginica")]
abline(lm(y~x), col = "green")

# sepal width and petal length is another instance of the paradox
cor(iris$Sepal.Width, iris$Petal.Length)
cor(iris$Sepal.Width[which(iris$Species == "virginica")], iris$Petal.Length[which(iris$Species == "virginica")])
cor(iris$Sepal.Width[which(iris$Species == "versicolor")], iris$Petal.Length[which(iris$Species == "versicolor")])
cor(iris$Sepal.Width[which(iris$Species == "setosa")], iris$Petal.Length[which(iris$Species == "setosa")])
plot(iris$Sepal.Width, iris$Petal.Length, col = iris$Species)
abline(lm(iris$Petal.Length[which(iris$Species == "versicolor")]~iris$Sepal.Width[which(iris$Species == "versicolor")]), col = "pink")
abline(lm(iris$Petal.Length[which(iris$Species == "setosa")]~iris$Sepal.Width[which(iris$Species == "setosa")]))
abline(lm(iris$Petal.Length[which(iris$Species == "virginica")]~iris$Sepal.Width[which(iris$Species == "virginica")]), col = "green")

# Problem 6
fire_dat <- read.csv("fire-dat.csv")
fire_dat
plot(fire_dat$firefight, fire_dat$injured)
abline(lm(fire_dat$injured ~ fire_dat$firefight))
cor(fire_dat$firefight, fire_dat$injured)
# both of these are however not correlated => the larger the fire, the more firefighters deployed, and the more injured. 
# just comparing these two variables does not give us a measure of how the number of firefighters deployed affected the number of injured.
# we need to control for other factors like size of fire to comment on this

# Problem 7
fire_intense <- read.csv("fire-intense.csv")

y <- fire_intense$injured[which(fire_intense$intensity <= 0.3)]
x <- fire_intense$firefight[which(fire_intense$intensity <= 0.3)]
plot(x,y)
abline(lm(y~x))
cor(x,y)
y <- fire_intense$injured[intersect(which(fire_intense$intensity > 0.3),which(fire_intense$intensity <= 0.4))]
x <- fire_intense$firefight[intersect(which(fire_intense$intensity > 0.3),which(fire_intense$intensity <= 0.4))]
plot(x,y)
abline(lm(y~x))
cor(x,y)
y <- fire_intense$injured[intersect(which(fire_intense$intensity > 0.4),which(fire_intense$intensity <= 0.5))]
x <- fire_intense$firefight[intersect(which(fire_intense$intensity > 0.4),which(fire_intense$intensity <= 0.5))]
plot(x,y)
abline(lm(y~x))
cor(x,y)
y <- fire_intense$injured[intersect(which(fire_intense$intensity > 0.5),which(fire_intense$intensity <= 0.6))]
x <- fire_intense$firefight[intersect(which(fire_intense$intensity > 0.5),which(fire_intense$intensity <= 0.6))]
plot(x,y)
abline(lm(y~x))
cor(x,y)
y <- fire_intense$injured[intersect(which(fire_intense$intensity > 0.6),which(fire_intense$intensity <= 0.7))]
x <- fire_intense$firefight[intersect(which(fire_intense$intensity > 0.6),which(fire_intense$intensity <= 0.7))]
plot(x,y)
abline(lm(y~x))
cor(x,y)
y <- fire_intense$injured[intersect(which(fire_intense$intensity > 0.7),which(fire_intense$intensity <= 0.8))]
x <- fire_intense$firefight[intersect(which(fire_intense$intensity > 0.7),which(fire_intense$intensity <= 0.8))]
plot(x,y)
abline(lm(y~x))
cor(x,y)
y <- fire_intense$injured[intersect(which(fire_intense$intensity > 0.8),which(fire_intense$intensity <= 0.9))]
x <- fire_intense$firefight[intersect(which(fire_intense$intensity > 0.8),which(fire_intense$intensity <= 0.9))]
plot(x,y)
abline(lm(y~x))
cor(x,y)
y <- fire_intense$injured[intersect(which(fire_intense$intensity > 0.9),which(fire_intense$intensity <= 1))]
x <- fire_intense$firefight[intersect(which(fire_intense$intensity > 0.9),which(fire_intense$intensity <= 1))]
plot(x,y)
abline(lm(y~x))
cor(x,y)
y <- fire_intense$injured[which(fire_intense$intensity > 1)]
x <- fire_intense$firefight[which(fire_intense$intensity > 1)]
plot(x,y)
abline(lm(y~x))
cor(x,y)
# in line with our hypothesis, if we account for intensity, there is no correlation between the number of fighters and the number injured!
cor(fire_intense$intensity, fire_intense$injured)
cor(fire_intense$intensity, fire_intense$firefight)
cor(fire_intense$firefight, fire_intense$injured)
#The variable intensity here is a confounding variable. A confounding variable causes an affect on
#other variables in a such a way that it seems like the other variables are causing an affect of each
#other.

# Correlation does not imply causation

# Problem 8
# a) wealth/expenditure could be the confounding variable. if one has more money, one is more likely to eat more icecream, and will have more assets, making a theft more likely.
# b) exercise could be the confounding variable. if one exercises more, then one is more likely to be taller, and pick up more weight.