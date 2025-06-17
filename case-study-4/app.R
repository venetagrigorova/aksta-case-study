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

# rename columns for display
nice_names <- c(
  "education" = "Education expenditure (% GDP)",
  "youth" = "Youth unemployment (%)",
  "net_migration" = "Net migration rate",
  "electricity_fossil" = "Electricity from fossil fuels (%)",
  "growth" = "Population growth (%)",
  "life" = "Life expectancy at birth"
)

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
                             choices = nice_names),
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
                 selectInput("xvar", "X-axis variable:", choices = names(nice_names)),
                 selectInput("yvar", "Y-axis variable:", choices = names(nice_names)),
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
    col <- names(nice_names)[nice_names == input$var]
    data %>% select(country, continent, value = all_of(col)) %>% 
      filter(!is.na(value))
  })
  
  output$datatable <- renderDT({
    req(input$show)
    isolate(
      datatable(selected_data(), options = list(pageLength = 15))
    )
  })
  
  output$map <- renderPlotly({
    col <- names(nice_names)[nice_names == input$var]
    gg <- ggplot(data_map, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes_string(fill = col), color = "white") +
      scale_fill_viridis_c() +
      theme_void()
    ggplotly(gg)
  })
  
  output$boxplot <- renderPlotly({
    col <- names(nice_names)[nice_names == input$var]
    ggplotly(
      ggplot(data, aes_string(y = col)) +
        geom_boxplot(fill = "skyblue") +
        theme_minimal()
    )
  })
  
  output$histogram <- renderPlotly({
    col <- names(nice_names)[nice_names == input$var]
    ggplotly(
      ggplot(data, aes_string(x = col)) +
        geom_histogram(fill = "tomato", bins = 30, alpha = 0.7) +
        geom_density(aes_string(x = col, y = "..count.."), color = "black") +
        theme_minimal()
    )
  })
  
  output$continent_box <- renderPlotly({
    col <- names(nice_names)[nice_names == input$var]
    ggplotly(
      ggplot(data, aes_string(x = "continent", y = col)) +
        geom_boxplot(aes(fill = continent)) +
        theme_minimal()
    )
  })
  
  output$continent_density <- renderPlotly({
    col <- names(nice_names)[nice_names == input$var]
    ggplotly(
      ggplot(data, aes_string(x = col, fill = "continent")) +
        geom_density(alpha = 0.5) +
        theme_minimal()
    )
  })
  
  output$scatter <- renderPlotly({
    xcol <- input$xvar
    ycol <- input$yvar
    ggplotly(
      ggplot(data, aes_string(x = xcol, y = ycol)) +
        geom_point(aes_string(color = "continent", size = input$sizevar)) +
        geom_smooth(aes_string(color = "continent"), method = "loess", se = FALSE, size = 0.5) +
        theme_minimal()
    )
  })
}

shinyApp(ui, server)
