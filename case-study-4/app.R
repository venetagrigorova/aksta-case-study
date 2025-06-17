library(shiny)
library(tidyverse)
library(plotly)
library(viridis)
library(DT)
library(countrycode)
library(jsonlite)

# load data
data <- fromJSON("data_cia2.json")
data <- as_tibble(data)

# clean and rename columns
data <- data %>% rename_with(make.names)
names(data)[names(data) == "expenditure"] <- "education"
names(data)[names(data) == "youth_unempl_rate"] <- "youth"
names(data)[names(data) == "net_migr_rate"] <- "net_migration"
names(data)[names(data) == "electricity_fossil_fuel"] <- "electricity_fossil"
names(data)[names(data) == "pop_growth_rate"] <- "growth"
names(data)[names(data) == "life_expectancy"] <- "life"

# mapping from internal names to display labels
nice_names <- c(
  "education" = "Education expenditure (% GDP)",
  "youth" = "Youth unemployment (%)",
  "net_migration" = "Net migration rate",
  "electricity_fossil" = "Electricity from fossil fuels (%)",
  "growth" = "Population growth (%)",
  "life" = "Life expectancy at birth"
)

reverse_names <- setNames(names(nice_names), nice_names)

# match world map to ISO codes
world_map <- map_data("world")
world_map$iso3 <- countrycode(world_map$region, "country.name", "iso3c")
data$ISO3 <- countrycode(data$country, "country.name", "iso3c")

# join map and data
data_map <- left_join(world_map, data, by = c("iso3" = "ISO3"))

# define UI
ui <- fluidPage(
  titlePanel("CIA World Data Analysis (2020)"),
  p("Explore global education, employment, and sustainability indicators."),
  tabsetPanel(
    tabPanel("Univariate analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var", "Select variable:",
                             choices = setNames(names(nice_names), nice_names)),
                 actionButton("show", "View raw data"),
                 DTOutput("datatable")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Map", plotlyOutput("map")),
                   tabPanel("Global analysis",
                            plotlyOutput("boxplot"),
                            plotlyOutput("histogram")),
                   tabPanel("Analysis per continent",
                            plotlyOutput("continent_box"),
                            plotlyOutput("continent_density"))
                 )
               )
             )
    ),
    tabPanel("Multivariate analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("xvar", "X-axis variable:", choices = nice_names),
                 selectInput("yvar", "Y-axis variable:", choices = nice_names),
                 selectInput("sizevar", "Point size by:", choices = c("population", "area"))
               ),
               mainPanel(plotlyOutput("scatter"))
             )
    )
  )
)

# define server
server <- function(input, output, session) {
  selected_data <- reactive({
    req(input$var)
    data %>% select(country, continent, value = all_of(input$var)) %>% 
      filter(!is.na(value))
  })
  
  output$datatable <- renderDT({
    req(input$show)
    isolate(
      datatable(selected_data(), options = list(pageLength = 15))
    )
  })
  
  output$map <- renderPlotly({
    gg <- ggplot(data_map, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes_string(fill = input$var), color = "white") +
      scale_fill_viridis_c() +
      theme_void()
    ggplotly(gg)
  })
  
  output$boxplot <- renderPlotly({
    ggplotly(
      ggplot(data, aes_string(y = input$var)) +
        geom_boxplot(fill = "skyblue") +
        theme_minimal()
    )
  })
  
  output$histogram <- renderPlotly({
    ggplotly(
      ggplot(data, aes_string(x = input$var)) +
        geom_histogram(fill = "tomato", bins = 30, alpha = 0.7) +
        geom_density(aes_string(x = input$var, y = "..count.."), color = "black") +
        theme_minimal()
    )
  })
  
  output$continent_box <- renderPlotly({
    ggplotly(
      ggplot(data, aes_string(x = "continent", y = input$var)) +
        geom_boxplot(aes(fill = continent)) +
        theme_minimal()
    )
  })
  
  output$continent_density <- renderPlotly({
    ggplotly(
      ggplot(data, aes_string(x = input$var, fill = "continent")) +
        geom_density(alpha = 0.5) +
        theme_minimal()
    )
  })
  
  output$scatter <- renderPlotly({
    xcol <- reverse_names[input$xvar]
    ycol <- reverse_names[input$yvar]
    ggplotly(
      ggplot(data, aes_string(x = xcol, y = ycol)) +
        geom_point(aes_string(color = "continent", size = input$sizevar)) +
        geom_smooth(aes_string(color = "continent"), method = "loess", se = FALSE, size = 0.5) +
        theme_minimal()
    )
  })
}

shinyApp(ui, server)