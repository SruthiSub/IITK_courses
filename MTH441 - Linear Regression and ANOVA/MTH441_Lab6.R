# MTH441 Lab 6

# Problem 1
# install.packages('ISLR')
library(ISLR)
library(MASS)
?Auto
plot(Auto)
# categorical variables - origin
# observe that cylinder (and year) is numerical (be careful -> is it categorical or not? not necessarily. We keep it as numerical for now.)
str(Auto) # tells us the structure of the data set => here we can see that origin is saved as numerical => convert to factor
# convert factor variables to factors:
Auto$origin <- as.factor(Auto$origin)
str(Auto)
plot(Auto, col = Auto$origin)
hist(Auto$mpg[Auto$origin == "1"], xlim = range(Auto$mpg))
hist(Auto$mpg[Auto$origin == "2"], xlim = range(Auto$mpg), add = TRUE, col = "red")
hist(Auto$mpg[Auto$origin == "3"], xlim = range(Auto$mpg), add = TRUE, col = "blue")
# visually, it appears as if green and pink(2,3) have a higher mpg than black(1)
fit <- lm(mpg ~ 1 + origin, Auto)
summary(fit) # comparing these coefficients also shows us that!
Auto$origin <- relevel(Auto$origin, ref = "2")
head(Auto$origin)
fit <- lm(mpg ~ 1 + origin , Auto)
summary(fit) # the estimates are same but significance levels change.
# from the colored plots we can see that displacement has different slopes. So we add the corresponding interaction terms:
fit <- lm(mpg ~ 1 + origin*displacement , Auto)
summary(fit)

# Problem 2
full <- lm(mpg ~ 1 + cylinders + origin*displacement*horsepower*weight*acceleration + year + I(acceleration^2), Auto)
summary(full)
AIC(full)

# Problem 3
stepAIC(full, direction = "backward")

stepAIC(full, direction = "backward", k = log(length(Auto$origin))) #BIC implementation

empty <- lm(mpg ~1, data = Auto)
stepAIC(empty, direction = "forward", scope = list(upper = ~ 1 + cylinders + origin*displacement*horsepower*weight*acceleration + year + I(acceleration^2), Auto, lower = ~1))

# Problem 4
names(Boston)
full <- lm(crim ~ (zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv)^2, data = Boston)
# full model has inf AIC => can't do step 
empty <- lm(crim ~1, data = Boston)
AIC(full)
stepAIC(full, direction = "backward")
AIC(empty)
stepAIC(empty, direction = "forward")

# Problem 5
titanic <- read.csv("https://dvats.github.io/assets/data/titanic.csv")
names(titanic)
fit_full <- glm(Survived ~ (Sexmale+Age+SibSp+Parch+Fare)^2,titanic, family = binomial(link = "logit"))
AIC(fit_full)
stepAIC(fit_full)
fit_empty <- glm(Survived ~ 1,titanic, family = binomial(link = "logit"))
AIC(fit_empty)
stepAIC(fit_empty)