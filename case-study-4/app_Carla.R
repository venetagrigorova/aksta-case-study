library(shiny)
library(tidyverse)
library(plotly)
library(viridis)
library(DT)
library(countrycode)
library(jsonlite)
library(shinycssloaders)


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
  p("Welcome to my shiny app, which allows you to visualize variables from the CIA 2020 factbook on the world map, generate descriptive statistics and statistical graphics."),
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
                   tabPanel("Map",
                            p("The map contains values of the selected variables. The countries with gray areas have a missing value for the visualized variable."),
                            div(style = "height: 600px;", withSpinner(plotlyOutput("map", height = "100%")))
                   ),
                   tabPanel("Global analysis",
                            fluidRow(
                              column(6, withSpinner(plotlyOutput("boxplot"))),
                              column(6, withSpinner(plotlyOutput("histogram")))
                            )
                   ),
                   tabPanel("Analysis per continent",
                            fluidRow(
                              column(6, withSpinner(plotlyOutput("continent_box"))),
                              column(6, withSpinner(plotlyOutput("continent_density")))
                            )
                   )
                 )
               )
             )
    ),
    tabPanel("Multivariate analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("xvar", "X-axis variable:", choices = setNames(names(nice_names), nice_names)),
                 selectInput("yvar", "Y-axis variable:", choices = setNames(names(nice_names), nice_names)),
                 selectInput("sizevar", "Point size by:", choices = c("population", "area"))
               ),
               mainPanel(withSpinner(plotlyOutput("scatter")))
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
      datatable(
        selected_data() %>%
          rename(
            Country = country,
            Continent = continent,
            `Selected Value` = value
          ),
        options = list(pageLength = 15)
      )
    )
  })
  
  output$map <- renderPlotly({
    variable <- input$var
    
    data_map$tooltip_text <- paste0(
      "Country: ", data_map$country,
      "<br>", nice_names[[variable]], ": ",
      round(data_map[[variable]], 2)
    )
    
    gg <- ggplot(data_map, aes(x = long, y = lat, group = group, fill = .data[[variable]], text = tooltip_text)) +
      geom_polygon(color = "white") +
      scale_fill_viridis_c(option = "plasma", direction = -1, name = nice_names[[variable]]) +
      coord_quickmap() +
      theme_minimal()
    
    ggplotly(gg, tooltip = "text")
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
    req(input$xvar, input$yvar, input$sizevar)  # make sure inputs exist
    ggplotly(
      ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
        geom_point(aes_string(color = "continent", size = input$sizevar)) +
        geom_smooth(aes_string(color = "continent"), method = "loess", se = FALSE, size = 0.5) +
        theme_minimal()
    )
  })
  
}

shinyApp(ui, server)