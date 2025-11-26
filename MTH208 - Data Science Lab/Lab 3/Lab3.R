# n = number of coin tosses
# size = 1 (tells R we are tossing a coin)
# prob = probability of success
rbinom(n = 1, size = 1, prob = 0.5)
# 1 = success (heads), 0 = failure (tails)
rbinom(n = 10, size = 1, prob = 0.5)

# Problem 1
# Simulate 1000 fair coin tosses and calculate the proportion of heads
v = c(rbinom(n = 1000, size = 1, prob = 0.5))
heads <- 0
for (i in c(1:1000))
{
  if (v[i] == 1)
  {
    heads = heads + 1
  }
}
proportion <- heads/1000
print(proportion)

# Simulate 1000 coin tosses with probability of heads = 0.3, and calculate the proportion of heads
v = c(rbinom(n = 1000, size = 1, prob = 0.3))
heads <- 0
for (i in 1:1000)
{
  if (v[i] == 1)
  {
    heads = heads + 1
  }
}
proportion <- heads/1000
print(proportion)


# Rolling a die
sample(x = 1:6, size = 1)
# Rolling an unfair die
sample(x = 1:6, size = 1, prob = c(.1, .2, .1, .1, .3, .2))
# drawing a random number between [a,b]
# n = number of random numbers
# min = a
# max = b
runif(n = 1, min = 0, max = 1)

# Problem 2
# drawing a ball from a bag with 3 red, 2 green and 2 blue balls
red <- 3/7
green <- 2/7
blue <- 2/7
ball <- sample(x = 1:3, size = 1, prob = c(red, green, blue))
if (ball == 1)
{
  print("red")
}
if (ball == 2)
{
  print("green")
}
if (ball == 3)
{
  print("blue")
}

# picking a column with given probabilities
A <- matrix(c(3, 1, -2, 4, 5, 3, -1, 2, -2), nrow = 3, ncol = 3)
P <- c(0,0,0)
norm <- function(v)
{
  p = (v[1]^2+v[2]^2+v[3]^2)^0.5
  return(p)
}
for (i in 1:3)
{
  P[i] <- norm(A[,i])
}
column <- sample(x = 1:3, size = 1, prob = P)
column

# simulating where the dart lands on a thread of length 5cm
dart <- runif(n = 1, min = 0, max = 5)
dart


# to define a vector of length 1000, you can use the command:
new <- numeric(length = 1000)
new

# Problem 3 - a
exceed <- function()
{
  count <- 0
  sum <- 0
  while (sum <= 1)
  {
    sum = sum + runif(n = 1, min = 0, max = 1)
    count = count + 1
  }
  return(count)
}
# Problem 3 - b
store <- numeric(length = 1000)
for (r in 1:1000)
{
  store[r] <- exceed()
}
# Problem 3 - c
sum <- 0
for (r in 1:1000)
{
  sum = sum + store[r]
}
average <- sum/1000
average
exp(1)

# Problem 4
attempts <- function(age)
{
  candles <- age
  a <- 0
  while(candles > 0)
  {
    p <- 1/candles
    P <- numeric(length = candles)
    for (r in 1:candles)
    {
      P[r] = p
    }
    blow <- sample(x = 1:candles, size = 1, prob = P)
    candles = candles - blow
    a = a + 1 
  }
  return(a)
}
store <- numeric(length = 1000)
for (r in 1:1000)
{
  store[r] = attempts(25)
}
store
tot <- 0
for (r in 1:1000)
{
  tot = tot + store[r]
}
average <- tot/1000
average

# for 30th birthday:
store <- numeric(length = 1000)
for (r in 1:1000)
{
  store[r] = attempts(30)
}
store
tot <- 0
for (r in 1:1000)
{
  tot = tot + store[r]
}
average <- tot/1000
average