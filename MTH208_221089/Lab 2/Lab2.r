# Character vector
v <- c("low", "medium", "high", "low")
# Factor
f <- factor(v, levels = c("low", "medium", "high"), ordered = TRUE)
# Output
v
f
summary(v)
summary(f)
# Finally
summary(factor(c("yes", "no", "yes")))

# Some exercises in array search

# By index
x <- c(10, 20, 30, 40)
x[2]
x[c(1,3)]
# By logical conditions to filter elements
x[x > 25]
which(x > 25)
which(x == 20)
# By names
names(x) <- c("a", "b", "c", "d")
x["b"]
x[c("a", "c")]
# Using %in%
x %in% c(20,30)
x[x %in% c(20,30)]
# Using match()
match(30,x)
# Using grep()
names <- c("Aditi", "Bhanu", "Charu")
grep("h", names)
names[grep("h", names)]
# Search in multidimensional array
m <- matrix(1:9, nrow = 3)
m[2,3]
which(m>5, arr.ind = TRUE)

# Data Frames

# Define vectors
Name <- c("Adi", "Bhanu", "Charu", "Dev", "Esha")
Age <- c(25, 30, 22, 28, 24)
Gender <- c("Male", "Female", "Female", "Male", "Female")
# Create data frame
df <- data.frame(Name, Age, Gender)
# View the data frame
print(df)

# Use $ operator (access a column by name):
df$Age
# Use [[]] (access a column by name or position):
df["Age"]
df[[2]]
# Use [,] (subset by rows and columns):
df[1, ]
df[ ,2]
df[1,2]
df[1:5, "Age"]
# Filter based on condition:
df[df$Age > 25, ]
df[df$Gender == "Male", "Name"]
# Filter using the subset() function:
subset(df, Age > 25)
subset(df, Gender == "Female", select = Name)
# Get row numbers matching conditions using which()
which(df$Age == 30)
df[which(df$Age == 30),]
# Search for pattern (strings):
df[grep("ha", df$Name), ]

# Visual and exploratory analysis:
getwd # we need to set the working directory first, so that we can read the csv file
setwd("C:/Users/User/Documents/MTH208_221089")
seat <- read.csv("seating.csv")
seat
ms <- which(seat$Roll.No > 10000000)
bs <- which(seat$Roll.No < 10000000)
print(length(ms)) # number of ms students enrolled in the course
print(length(bs)) # number of bs students enrolled in the course

cricket <- read.csv("battingbowling.csv")
# decent batter: avg > 25, decent bowler: avg < 40
all_rounders <- subset(cricket, (Batting > 25) & (Bowling < 40))
teams <- factor(all_rounders$Team, levels = unique(cricket$Team))
summary(teams)
print("Team with most all rounders: New Zealand")
print("Team with least all rounders: South Africa")

?plot
x = c(0:10)
y = c(0:10)
plot(x,y, type = "l", main = "Y = X Plot", frame.plot = TRUE, pty = "m")

f <- function(x)
{
  val <- (1 + 1/x)^x
  return(val)
}
x = c(1:1000)
y = f(x)
plot(x,y, type = "l")
abline(a = exp(1), b = 0, col = "red")
