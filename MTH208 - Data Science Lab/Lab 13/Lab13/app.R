#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(gapminder)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Gapminder Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("continents", 
                        "Choose which Continent", 
                        choices = list("Africa", "Americas", "Asia", "Europe", "Oceania")),
            
          sliderInput("bins",
                        "Select Years",
                        min = 1952,
                        max = 2007,
                        value = c(1952,2007)),
          selectInput("compare", "Select Country for Comparison", choices = gapminder$country)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h3("Life Expectancy and GDP Analysis"),
           plotOutput("Plot1"),
           h6(textOutput("text")),
           plotOutput("Plot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plot1 <- renderPlot({
      p <-ggplot(
        gapminder,
        aes(x= gdpPercap,y= lifeExp,size= pop, colour= continent)
      ) +
        geom_point(show.legend=TRUE, alpha= 0.7) +
        scale_color_viridis_d()+
        scale_size(range= c(2, 12))+
        scale_x_log10()+
        labs(x= "GDP per capita", y= "Life expectancy")
      p
    })
    
    output$text <- renderText({
      df <- as_tibble(gapminder)
      df <- df %>% filter(year <= input$bins[2]) %>% filter(year >= input$bins[1])
      df <- df %>% filter(continent %in% input$continents)
      m <- which.max(df$lifeExp)
      res <- df$country[m]
      yr <- df$year[m]
      paste("Country with the best Life Expectancy in this time period was ", res, " in the year ", yr)})
    
    output$Plot2 <- renderPlot({
      df <- as_tibble(gapminder)
      df <- df %>% filter(year <= input$bins[2]) %>% filter(year >= input$bins[1])
      com <- df %>% filter(country == input$compare)
      df <- df %>% filter(continent %in% input$continents)
      p <-ggplot(df,aes(year, lifeExp,group = country, color = continent)) +
        geom_line() +
        geom_line(data = com, aes(year, lifeExp, color = "comparison", linewidth = 0.1))+
        labs(x = "Year", y = "Life Expectancy")
      p
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
