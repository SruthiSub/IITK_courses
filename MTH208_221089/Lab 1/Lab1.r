## Basic arithmetic
1 + 2 
3 + 5 - 2

## assigning value
a <- 4  # I prefer arrow over =
b = 5
my.number <- 8
my.vector <- 5:100

## operations with objects
a + b
my.number %% 5   #modulo
my.vector %% 5
b/a + sqrt(my.number) - sin(b) + log(a)




## Indexing 
## Logical checks
a == 10  # is a = 10?
my.number == a
my.vector == c(5:100)
my.vector == my.number

index <- which(my.vector == my.number)
my.vector[index]



# R is great for vectors
sum(my.vector)
mean(my.vector)
median(my.vector)
max(my.vector)
min(my.vector)

## Making functions
greetings <- function(name)
{
  text <- paste("Hello", name)
  return(text)
}
##greetings(Akash)
greetings("Akash")

add <- function(num1, num2 = 5)
{
  y <- num1 + num2
  return(y)
}
add(num1 = 6, num2 = 10)


# function for evaluating f(x) = 5x^2 + 3x - 3
fx <- function(x)
{
  val <- 5*x^2 + 3*x - 3
  return(val)
}
fx(4)
fx(10)
fx(4:10)
fx(a)
fx( c(4, 9, 15) )
x <- c(4, 9, 15)
fx(x)

## working directory
getwd()
list.files()
##setwd("path/to/your/folder")
setwd("C:/Users/User/Desktop/MTH208/Worksheet1")

# Loading a csv file (csv is much easier than xls)
seat <- read.csv("seating.csv")
head(seat)  # to see the top of the data
str(seat)  # structure of the data
dim(seat)  #dimensions

seat[1,1]
seat[1, ]
seat[3:5, ]
seat[c(4, 7, 9), ]

# When in doubt
?head
?dim
?read.csv

# Loops in R
track <- 0
for(i in 1:length(my.vector))
{
  track <- track + my.vector[i]
}
track

#function to find n!
gx <- function(x)
{
  val <- 1
  for (i in 1:x)
  {
    val <- val*i
  }
  return(val)
}
gx(1) == factorial(1)
gx(2) == factorial(2)
gx(3) == factorial(3)
gx(4) == factorial(4)
gx(5) == factorial(5)
gx(6) == factorial(6)
gx(7) == factorial(7)

#function to calculate e
hx <- function(x)
{
  val <- (1 + 1/x)^x
  return(val)
}
hx(50000)
exp(1)

#as we double n, how fast does the error converge? Check if the ratio stabilizes or not
my.errors <- numeric(100)
my.ratios <- numeric(99)
start <- 100
for (i in 1:100)
{
  n <- start*(2^i)
  my.errors[i] = abs(exp(1) - hx(n))
}
for (i in 1:99)
{
  my.ratios[i] = my.errors[i+1]/my.errors[i]
}
my.ratios #observe that the ratio converges to 1!

#to find my assigned seat
seat <- read.csv("seating.csv")
my.rollno <- 221089 #my roll no
index <- which(seat[,1] == my.rollno)
seat[index,3] #my seat

my.currentseat = "C29"
index <- which(seat[,3] == my.currentseat)
seat[index,2] #student assigned to current seat
