# Worksheet 8

#install.packages("dplyr")
library("dplyr")
library("rvest")
library("tidyverse")

# Problem 1

url <- "https://www.relianceiccrankings.com/ranking/womenodi/batting/"
page <- read_html(url)
batters <- html_table(page)
country <- page %>% html_elements("td") %>% html_element("img")
country <- country[seq(4,600,6)]
country <- country %>% html_attr("alt")
batters <- data.frame(batters)
data <- data.frame(batters, country)
data <- data %>% select(-Var.4)
cleaning <- str_split(data$Career.Best.Womens.ODI.Ranking, "\\n")
cleaning <- map_chr(cleaning, ~ str_c(.x, collapse = ""))
data$Career.Best.Womens.ODI.Ranking <- cleaning
data$Career.Best.Womens.ODI.Ranking
data

# Problem 2
batting <- as_tibble(data)
save(batting, file = "batting.Rdata")

# Problem 3
vignette("dbplyr")
?starwars
dim(starwars)
# can organize based on rows - filter(), slice(), arrange()
?filter()
filter(starwars, height < 100)
starwars %>% filter(skin_color == "light")
?slice()
slice(starwars, c(1,3,5))
starwars %>% slice(5:10)
# slice_head() and slice_tail() can also be done
starwars %>% slice_head(n=3)
# slice_sample() randomly selects rows, we can use the option prop to choose a certain proportion of the cases
starwars %>% slice_sample(n=5)
starwars %>% slice_sample(prop = 0.1)
# slice_min() and slice_max() select rows with highest or lowest values of a variable = > note that we first must choose only the values which are not NA
starwars %>% filter(!is.na(height)) %>% slice_max(height, n=3)
?arrange()
arrange(starwars, name)
starwars %>% arrange(height, mass) # if you provide multiple column names, each additional column is used to break ties of preceding columns
starwars %>% arrange(desc(height))
# can organize based on columns - select(), rename(), mutate()
?select()
select(starwars, mass, height, name)
select(starwars, -mass)
select(starwars, 1) # can also use column indices!
# select allows you to rapidly zoom into a useful subset using operations that usually only work on numeric variable positions
# select columns by name
starwars %>% select(hair_color, skin_color, eye_color)
# select all columns between hair_color and eye_color (inclusive)
starwars %>% select(hair_color:eye_color)
# select all columns except those from hair_color to eye_color (inclusive)
starwars %>% select(!(hair_color:eye_color))
# select all columns ending with color
starwars %>% select(ends_with("color"))
# other helper functions within select: starts_with(), ends_with(), matches(), contains()
# can also rename 
select(starwars, NewName = name)
# notice that select drops all the other columns => can use rename to retain them
?rename()
rename(starwars, Name = name) # new_name = old_name syntax
?mutate()
mutate(starwars, mass = NULL) # will remove mass
mutate(starwars, half_height = height/2) %>% select(half_height, height, everything())# add new columns as a function of existing columns 
mutate(starwars, height = height/2) # modify existing columns
# if you want to keep only new variables, use .keep = "none"
starwars %>% mutate(half_height = height/100, .keep = "none")
# can change column order with relocate()
starwars %>% relocate(sex:homeworld, .before = height)
# can organize groups of rows - summarize()
?summarize()
starwars %>% summarise(height = mean(height, na.rm = TRUE))
starwars %>% summarise(height = median(height, na.rm = TRUE))

# main 5 ways to alter a tidy data frame : arrange(), filter(), select(), mutate(), summarise()
# note that new data frames need to be saved separately
# hence in the case of multiple operations, the code becomes long
a1 <- group_by(starwars, species, sex)
a2 <- select(a1, height, mass)
a3 <- summarise(a2, height = mean(height, na.rm = TRUE), mass = mean(mass, na.rm = TRUE))
# or wrapping them together:
summarise(
  select(
    group_by(starwars, species, sex),
    height, mass),
  height = mean(height,na.rm = TRUE), mass = mean(mass, na.rm = TRUE)
  )
# using piping for the same operations:
starwars %>%
  group_by(species, sex) %>%
  select(height, mass) %>%
  summarise(height = mean(height, na.rm = TRUE), mass = mean(mass, na.rm = TRUE))
# Patterns of operations:
# Selecting operations
# when any variable named as a column is passed in a select function, it acts as the column name and not the variable, but if we are doing any operation on the variable then it acts as the variable
# some examples:
height <- 5
starwars %>% select(height)
starwars %>% select(identity(height))
name <- "color"
starwars %>% select(name)
starwars %>% select(ends_with(name))
vars <- c("name", "height")
select(starwars, all_of(vars), mass)
# Mutating operations
df <- starwars %>% select(name, height, mass)
mutate(df, "height",2)
mutate(df, height+10)
var <- seq(1, nrow(df))
mutate(df, new = var) # if you give a valid sequence, then no quotation marks in column name
# group_by() has mutate semantics, not select semantics
group_by(starwars, sex)
group_by(starwars, sex = as.factor(sex))
group_by(starwars, height_binned = cut(height, 3)) %>% select(height_binned)
# you can't supply a column name to group_by(). This amounts to creating a new column containing the string recycled to number of rows
group_by(df, "month")

# Problem 4
mtcars
?mtcars

# Problem 5
table <- mtcars %>% group_by(cyl) %>% summarise(disp = mean(disp, na.rm = TRUE), hp = mean(hp, na.rm = TRUE))
table

# Problem 6
indian <- batting %>% filter(country == "IND")
indian
indian$Rating
teams <- unique(batting %>% select(country))
teams
no_players <- batting %>% group_by(country) %>% summarise(number = n())
no_players
average_ranking <- batting %>% group_by(country) %>% select(Rating, country) %>% summarize(Rating = mean(Rating, na.rm = TRUE))
average_ranking
average_ranking %>% arrange(desc(Rating))

# Problem 7
Asia <- c("IND", "SL", "PAK", "BAN", "THA")
asia_or_not <- function(team)
{
  k <- length(team)
  asias <- numeric(length = k)
  for (i in 1:k)
  {
    if (team[i] %in% Asia)
    {
      asias[i] <- 1
    }
    else{
      asias[i] <- 0 
    }
  }
  return(asias)
}

new1 <- no_players %>% mutate(Asia_or_not = asia_or_not(country))
new1 %>% group_by(Asia_or_not) %>% summarise(n=sum(number))
new2 <- batting %>% mutate(Asia_or_not = asia_or_not(country))
new2 %>% group_by(Asia_or_not) %>% summarise(average = mean(Rating))
