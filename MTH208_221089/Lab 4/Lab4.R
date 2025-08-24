# install.packages("imager")
library(imager)
getwd()
setwd("C:/Users/sruth/Downloads/MTH208_221089/Lab 4")

dog <- load.image("dog.jpeg")
dim(dog)
plot(dog)

graydog <- grayscale(dog)
plot(graydog)
dim(graydog)

# to extract the black and white image as matrix
gray.mat <- as.matrix(graydog[,,1,1])
dim(gray.mat)
?as.matrix

# to extract the array with all three rgb channels
col.mat <- as.array(dog[, ,1,])
dim(col.mat)

# we now do manipulations then save the updated arrays as images

# vertical cropping
cropped.mat <- col.mat[1:300, ,]
crop.dog <- as.cimg(cropped.mat)
plot(crop.dog)
?as.cimg

# Problem 1
distance <- function(color1, color2)
{
  d <- ((color1[1]-color2[1])^2 + (color1[2]-color2[2])^2 + (color1[3]-color2[3])^2)^0.5
  return(d)
}
col.matG <- col.mat
dims <- dim(col.mat)
dims
green <- c(0,1,0)
distG <- matrix(0, nrow = dim(col.mat)[1], ncol = dim(col.mat)[2])
for (i in 1:dim(col.mat)[1])
{
  for (j in 1:dim(col.mat)[2])
  {
    distG[i,j]=distance(green, col.mat[i,j,])
  }
}
distG
purestGreen <- which(distG[,] == min(distG), arr.ind = TRUE)
purestGreen
col.matG[purestGreen[1],purestGreen[2],]=c(1,0,0)
markedGreen <- as.cimg(col.mat)
plot(markedGreen)

# Problem 2 
# analogous to Problem 1

# Problem 3
# let us first define a function that takes in an image and a color and compares the average distance between the two:
dist.color <- function(image, color)
{
  imageArray <- as.array(image[,,1,])
  dims <- dim(imageArray)
  distMatrix <- matrix(0, nrow = dims[1], ncol = dims[2])
  sum <- 0
  for (i in 1:dims[1])
  {
    for (j in 1:dims[2])
    {
      distMatrix[i,j] = distance(color,imageArray[i,j,])
      sum = sum + distMatrix[i,j]
    }
  }
  average <- sum/(dims[1]*dims[2])
  return(average)
}
which.color <- function(image)
{
  red <- c(1,0,0)
  green <- c(0,1,0)
  blue <- c(0,0,1)
  distances <- c(dist.color(image,red), dist.color(image,green), dist.color(image,blue))
  final <- which(min(distances) == distances, arr.ind = TRUE)
  guess <- c("red","green","blue")[final]
  return(guess)
}
# dist of colors: RGB from each image
col1 <- load.image("col1.png")
col2 <- load.image("col2.png")
col3 <- load.image("col3.png")
which.color(col1)
which.color(col2)
which.color(col3)

# Problem 4
# write a function to test whether a given image has a lot of snow or not 
# idea: check distance from color:white (but what threshold?)
snow <- function(image)
{
  white <- c(0,0,0)
  red <- c(1,0,0)
  green <- c(0,1,0)
  blue <- c(0,0,1)
  colors <- c(dist.color(image,white), dist.color(image,red), dist.color(image,green), dist.color(image,blue))
  c <- which(max(colors)==colors, arr.ind = TRUE)
  if (c == 1){
    return("Lot of Snow")
  }
  return("Not a Lot of Snow")
}
land1 <- load.image("land1.jpeg")
land2 <- load.image("land2.jpeg")
snow(land1)
snow(land2)

# Problem 5
# Rotate the given image by 180 degrees
rotate180 <- function(image)
{
  imageArray <- as.array(image[,,1,])
  dims <- dim(imageArray)
  rotatedImage <- array(dim = dims)
  l = dims[1]
  h = dims[2]
  for (k in 1:3)
  {
    for (i in 1:l)
    {
      for (j in 1:h)
      {
        rotatedImage[i,j,k] <- imageArray[l-i+1,h-j+1,k]
      }
    }
  }
  rotated <- as.cimg(rotatedImage)
  return(rotated)
}
plot(rotate180(dog))

# Problem 6
# Rotate the given image clockwise by 90 degrees
rotateClock90 <- function(image)
{
  imageArray <- as.array(image[,,1,])
  dims <- dim(imageArray)
  l = dims[1]
  h = dims[2]
  rotatedImage <- array(dim = c(h, l, dims[3]))
  for (k in 1:3)
  {
    for (i in 1:l)
    {
      for (j in 1:h)
      {
        rotatedImage[h-j+1,i,k] <- imageArray[i,j,k]
      }
    }
  }
  rotated <- as.cimg(rotatedImage)
  return(rotated)
}
plot(rotateClock90(dog))

# Problem 7
# Rotate the given image anticlockwise by 90 degrees
rotateAntiClock90 <- function(image)
{
  imageArray <- as.array(image[,,1,])
  dims <- dim(imageArray)
  l = dims[1]
  h = dims[2]
  rotatedImage <- array(dim = c(h, l, dims[3]))
  for (k in 1:3)
  {
    for (i in 1:l)
    {
      for (j in 1:h)
      {
        rotatedImage[j,l-i+1,k] <- imageArray[i,j,k]
      }
    }
  }
  rotated <- as.cimg(rotatedImage)
  return(rotated)
}
plot(rotateAntiClock90(dog))

# Problem 8 
# Let us first write a resize function for a general image
col.mat <- as.array(dog[,,1,])
dim(col.mat)
reduced <- array(0, dim = c(300,300,3))
dim(reduced)
col.mat.crop <- col.mat[1:600, 1:600,]
dim(col.mat.crop)
for (i in 1:300)
{
  for (j in 1:300)
  {
    ind1 <- (2*i-1):(2*i)
    ind2 <- c(2*j-1):(2*j)
    reduced[i,j,1] = mean(col.mat.crop[ind1,ind2,1])
    reduced[i,j,2] = mean(col.mat.crop[ind1,ind2,2])
    reduced[i,j,3] = mean(col.mat.crop[ind1,ind2,3])
  }
}
comp <- as.cimg(reduced)
plot(comp)
save.image(comp, "compresseddog.jpeg")
# size of the image became approximately 1/4th!!

# Problem 9
col.mat.comp60 <- array(dim = c(60,60,3))
dim(col.mat.comp60)
for (i in 1:60)
{
  for (j in 1:60)
  {
    ind1 <- (10*i - 9) : (10*i)
    ind2 <- (10*j - 9) : (10*j)
    col.mat.comp60[i,j,1] = mean(col.mat.crop[ind1,ind2,1])
    col.mat.comp60[i,j,2] = mean(col.mat.crop[ind1,ind2,2])
    col.mat.comp60[i,j,3] = mean(col.mat.crop[ind1,ind2,3])
  }
}
comp60 <- as.cimg(col.mat.comp60)
plot(comp60)
save.image(comp60, "compresseddog60.jpeg")

# Problem 10
# We can decompose the image using methods like SVD and then process it