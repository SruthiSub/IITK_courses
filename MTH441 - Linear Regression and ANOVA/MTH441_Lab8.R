# MTH441 Lab 8

# Problem 1
PlantGrowth
boxplot(weight ~ group, data = PlantGrowth)
# It appears as though the different levels have significantly different means

# Problem 2
weight1 <- PlantGrowth$weight[1:10]
m1 <- mean(weight1)
weight2 <- PlantGrowth$weight[11:20]
m2 <- mean(weight2)
weight3 <- PlantGrowth$weight[21:30]
m3 <- mean(weight3)
m <- mean(PlantGrowth$weight)

SS_Treatment <- 10*((m1-m)^2 + (m2-m)^2 + (m3-m)^2)
SS_Total <- 0
for (i in 1:30){
  SS_Total = SS_Total + (PlantGrowth$weight[i] - m)^2
}
SS_Error <- SS_Total - SS_Treatment
k <- 3
n <- 10
N <- 30
MS_Treatment <- SS_Treatment/(k-1)
MS_Error <- SS_Error/(N-k)
F0 <- MS_Treatment/MS_Error
pval <- pf(F0, k-1, N-k, lower.tail = FALSE)
SS_Treatment
SS_Error
MS_Treatment
MS_Error
F0
pval
# pval < 0.05, hence null hypothesis is rejected!

# Problem 3
one_way_aov <- aov(weight ~ group, data = PlantGrowth)
summary(one_way_aov)

# Problem 4
InsectSprays
boxplot(count ~ spray, data = InsectSprays)
one_way_aov <- aov(count ~ spray, data = InsectSprays)
summary(one_way_aov)
# Here also, null hypothesis is rejected! (aligns with what we would expect from the boxplot here too)

# Problem 5
?aov
y <- c(31, 27, 24, 31, 28, 31, 45, 29, 46, 21, 18, 48, 42, 36, 46, 32, 17, 40)
rest <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)
item <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)
data <- data.frame(y,rest,item)
data$rest <- as.factor(data$rest)
data$item <- as.factor(data$item)
RCBD_aov <- aov(y ~ rest+item, data)
summary(RCBD_aov)
# pval for item is less than 0.05 => null hypothesis rejected => all three are not equal!

# Problem 6
ToothGrowth
means <- aggregate(len ~ dose + supp, data = ToothGrowth, FUN = mean)
means
# Plot with lines for each supplement
plot(len ~ dose, data = subset(means, supp == "OJ"),
     type = "b", pch = 19, col = "orange", ylim = c(0, max(means$len)),
     xlab = "Dose (mg/day)", ylab = "Tooth length",
     main = "Tooth growth by dose and supplement")
lines(len ~ dose, data = subset(means, supp == "VC"),
      type = "b", pch = 17, col = "blue")
legend("topleft", legend = c("OJ", "VC"),
       col = c("orange", "blue"), pch = c(19, 17), lty = 1)
# as there is intersection in the graph, there is likely to be some interaction

# Problem 7
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
two_way_aov <- aov(len ~ dose*supp, data = ToothGrowth)
summary(two_way_aov)
means
# we see that all three covariates are significant
# it matters which supplement we use - OJ having a higher mean, although both perform similarly at high doses
# it matters which dose we use - higher doses having higher means
# interaction does play a role