# Worksheet 6
# install.packages("rvest")
# install.packages("tidyverse", repos = "https://cloud.r-project.org/")
# install.packages("tidyverse")
library(rvest)
library(tidyverse)

#| eval: false
url <- "https://home.iitk.ac.in/~akasha/index.html"
page <- read_html(url)

# Extract the page title
title <- page %>% html_element("title") %>% html_text()
print(title)

url <- "https://www.wikipedia.org"
page <- read_html(url)

# Extract the page title
title <- page %>% html_element("title") %>% html_text()
print(title)

#| eval: false
url <- "https://home.iitk.ac.in/~akasha/research.html"
page <- read_html(url)
links <- page %>% html_elements("a") %>% html_attr("href")
links
head(links, 10)
# how many links are there on the webpage?
length(links)
?html_elements
?html_attr

# Problem 1
url <- "https://home.iitk.ac.in/~akasha/research.html"
page <- read_html(url)
links <- page %>% html_nodes("img") %>% html_attr("src") 
links

# Problem 2 - fix this
#| eval: false
url <- "https://cran.r-project.org/mirrors.html"
page <- read_html(url)
urls <- page %>% html_elements("td a") %>% html_attr("href")
head(urls,5)
?html_text
server_names <- page %>% html_elements("td a")
server_names <- html_text(server_names)
server_names
places <- page %>% html_elements("dt")
places <- html_text(places)
places

# Problem 3
# to extract class info, add a . before
url <- "https://quotes.toscrape.com"
page <- read_html(url)
?html_
quotes <- page %>% html_elements(".quote .text")
quotes <- html_text(quotes)
quotes
authors <- page %>% html_elements(".quote .author")
authors <- html_text(authors)
authors
?write.csv
data <- data.frame(quotes, authors)
setwd("C:/Users/sruth/Downloads/MTH208_221089/Lab 6")
csv_file <- write.csv(data, "quotesandauthors.csv")
head(data)

# Problem 4
url <- "https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now"
page <- read_html(url)
rank <- page %>% html_elements(".countdown-index")
rank <- html_text(rank)
rank <- substring(rank,2,4)
rank
name <- page %>% html_elements(".article_movie_title") %>% html_elements("a")
name <- html_text(name)
name
score <- page %>% html_elements(".tMeterScore")
score <- html_text(score)
score <- substring(score,1,2)
score 
year <- page %>% html_elements("h2") %>% html_elements(".subtle")
year <- html_text(year)
year <- substring(year, 2, 5)
year
director <- page %>% html_elements(".director")
director <- html_text(director)
director <- gsub("[\t\n]","", director)
director <- substring(director, 13, )
?strsplit
?substring
?gsub
?substr
netflix_movies <- data.frame("Name" = name, "Year" = year, "Ranking" = rank, "Score" = score, "Director" = director)
netflix_movies

# Problem 5
url <- "https://www.espn.com/tenis/rankings/_/tipo/wta"
page <- read_html(url)
?html_table
table <- html_table(page)
?data.frame
women_tennis <- data.frame(table)
women_tennis <- women_tennis[,c(1,3,4,5)]
women_tennis
#women_tennis is the clean table with rank, name, points and age