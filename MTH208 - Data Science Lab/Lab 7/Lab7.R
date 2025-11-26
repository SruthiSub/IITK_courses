# Worksheet 7
setwd("C:/Users/sruth/Downloads/MTH208_221089/Lab 7")
library(tidyverse)
library(rvest)

# Problem 1
url <- "https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now"
page <- read_html(url)

# First, we need to extract the links of the movie specific sites to scrape from
links <- page %>% html_elements(".article_movie_poster") %>% html_attr("href") 
links
tomatometer_score <- array(0, dim = 100)
popcornometer_score <- array(0, dim = 100)
num_tomatometer <- array(0, dim = 100)
num_popcornometer <- array(0, dim = 100)
for (i in 1:100)
{
  movie_url <- links[i]
  movie_page <- read_html(movie_url)
  info <- movie_page %>% html_elements("rt-text") 
  info <- html_text(info)
  pop <- which(info == "Popcornmeter", arr.ind = TRUE)
  tom <- which(info == "Tomatometer", arr.ind = TRUE)
  popscore <- info[pop[1]+1]
  tomscore <- info[tom[1]+5]
  popcornometer_score[i] <- popscore
  tomatometer_score[i] <- tomscore
  links1 <- movie_page %>% html_elements("rt-link")
  links1 <- html_text(links1)
  ind <- which(links1 == "", arr.ind = TRUE)
  reviews <- links1[ind[3]+1]
  ratings <- links1[ind[3]+2]
  reviews <- substring(reviews,14,16)
  reviews <- as.integer(reviews)
  num_tomatometer[i] <- reviews
  ratings <- substring(ratings,14,18)
  if (grepl(",",ratings)) {
    ratings <- 1000
  } else {
    p=4
    while(grepl("+",ratings, fixed = TRUE)){
      ratings <- substring(ratings,1,p)
      p<-p-1
    }
    ratings <- as.integer(ratings)
  }
  num_popcornometer[i] <- ratings
}

tomatometer_score
popcornometer_score
num_tomatometer
num_popcornometer

data <- data.frame(tomatometer_score, popcornometer_score, num_tomatometer, num_popcornometer)
save(data, file = "TomatoList.Rdata")


# Problem 2

# if poster is dull and dark, does that relate to lower ratings?
# how close the picture is to black, and plot that measure against the rating and see if there is a relation

# download.file to download images

library(imager)

darkness_level <- function(image)
{
  image_array <- as.array(image)
  d <- dim(image_array)
  black <- c(0,0,0)
  r <- d[1]
  c <- d[2]
  dist_matrix <- matrix(0, nrow = r, ncol = c)
  for (i in 1:r)
  {
    for (j in 1:c)
    {
        col <- c(image_array[i,j,1,1], image_array[i,j,1,2], image_array[i,j,1,3])
        dist_matrix[i,j] <- norm(col-black, "2")
    }
  }
  measure <- mean(dist_matrix)
  return(measure)
}

darkness_measure <- array(0, dim= 100)

for (i in 1:100)
{
  movie_url <- links[i]
  movie_page <- read_html(movie_url)
  image_url <- movie_page %>% html_nodes("meta[property='og:image']") %>% html_attr("content")
  download.file(image_url, "image.jpeg", mode= "wb")
  image <- load.image("image.jpeg")
  darkness <- darkness_level(image)
  darkness_measure[i] <- darkness
}

darkness_measure
plot(num_popcornometer, darkness_measure)
# There does not appear to be a correlation between darkness of the poster of a movie and it's ratings!