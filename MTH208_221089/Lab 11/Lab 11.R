# Worksheet 11

library(ggplot2)
load("IMDB_movies.Rdata")
# ggplot works in layers 
ggplot(dat, aes(x = rating))
# single variable plots
ggplot(dat, aes(x = rating)) + geom_histogram()
ggplot(dat, aes(x = rating)) + geom_boxplot()
ggplot(dat, aes(x = rating)) + geom_bar()
# two variable plots
ggplot(dat, aes(x = year, y = over.votes)) + geom_point()
# zooming in to some part
ggplot(dat, aes(x = year, y = over.votes)) + geom_point() + coord_cartesian(xlim = c(1996, 2025)) 
# aesthetics can be added using the aes() option
Year <- dat$year < 2000
Year <- as.factor(Year)
levels(Year) <- c("After 2000", "Before 2000")
ggplot(dat, aes(x = over.votes, y = rating)) +
  geom_point(aes(shape = Year, col = Year)) +
  labs(title = "Votes vs Rating", y = "Rating", x = "Number of Votes")
names(dat)

# Problem 1
load("covid.Rdata")
names(india_covid)

# Problem 2
library(tidyverse)
# Visualizing cases state-wise
df <- india_covid %>% select('State/UT', 'Confirmed Cases') 
ggplot(df, aes(x = "", y = `Confirmed Cases`, fill = df$`State/UT`)) +
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), plot.title = element_text(hjust = 0.5)) +
  labs(fill = "State/UT", x = NULL, y = NULL, title = "State-wise Confirmed Cases", caption = "Source: Covid Data") +
  coord_polar("y", start = 0)
# Since there are many states, this is hard to understand information from. So let us split up the states into subgroups based on number of cases, and plot the pie charts group wise
df <- india_covid %>% select('State/UT', 'Confirmed Cases') %>% filter(india_covid$`Confirmed Cases`> 2000000)
ggplot(df, aes(x = "", y = `Confirmed Cases`, fill = `State/UT`)) +
  geom_bar(width = 1, stat = "identity") +
  labs(fill = "State/UT", x = NULL, y = NULL, title = "For states with >2000000 Confirmed Cases", caption = "Source: Covid Data") +
  coord_polar("y", start = 0)
df <- india_covid %>% select('State/UT', 'Confirmed Cases') %>% filter(india_covid$`Confirmed Cases`< 2000000 & india_covid$`Confirmed Cases`> 1000000)
ggplot(df, aes(x = "", y = `Confirmed Cases`, fill = `State/UT`)) +
  geom_bar(width = 1, stat = "identity") +
  labs(fill = "State/UT", x = NULL, y = NULL, title = "For states with >1000000 and <2000000 Confirmed Cases", caption = "Source: Covid Data") +
  coord_polar("y", start = 0)
df <- india_covid %>% select('State/UT', 'Confirmed Cases') %>% filter(india_covid$`Confirmed Cases`< 1000000 & india_covid$`Confirmed Cases`> 250000)
ggplot(df, aes(x = "", y = `Confirmed Cases`, fill = `State/UT`)) +
  geom_bar(width = 1, stat = "identity") +
  labs(fill = "State/UT", x = NULL, y = NULL, title = "For states with >250000 and <1000000 Confirmed Cases", caption = "Source: Covid Data") +
  coord_polar("y", start = 0)
df <- india_covid %>% select('State/UT', 'Confirmed Cases') %>% filter(india_covid$`Confirmed Cases`< 250000)
ggplot(df, aes(x = "", y = `Confirmed Cases`, fill = `State/UT`)) +
  geom_bar(width = 1, stat = "identity") +
  labs(fill = "State/UT", x = NULL, y = NULL, title = "For states with <250000 Confirmed Cases", caption = "Source: Covid Data") +
  coord_polar("y", start = 0)

# Create ordered barplot where color is rate of death in each state
india_covid <- as_tibble(india_covid)
india_covid <- india_covid[order(india_covid$`Confirmed Cases`),]
india_covid$`State/UT` <- factor(india_covid$`State/UT`, levels = india_covid$`State/UT`)
Rate <- round(india_covid$`Death`/india_covid$`Confirmed Cases`,2)
india_covid <- cbind(india_covid, Rate)

df <- india_covid %>% select(`Confirmed Cases`,`Death`,`State/UT`)
ggplot(df, aes(x = `State/UT`, y = log10(`Confirmed Cases`)), fill = `Rate`) +
  geom_bar(stat = "identity", aes(fill = `Rate`)) +
  labs(fill = "Rate",x = NULL, y = NULL, title = "State-wise rate of death") +
  coord_flip()

# Same plot with text as the rate
q <- ggplot(df, aes(x = `State/UT`, y = log10(`Confirmed Cases`)), label = `Rate`) 
q + geom_point(stat = "identity", fill = "black", size = 8)+
  geom_segment(col = "black",aes(y = 0, yend = log10(`Confirmed Cases`),x = `State/UT`, xend = `State/UT`), stat = "identity") +
  geom_text(color = "white", size=2, label = Rate)+
  labs(label = "Rate",title = "State-wise rate of death") +
  coord_flip()
  