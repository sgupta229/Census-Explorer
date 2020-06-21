library(shiny)
library(shinythemes) # add a theme: https://rstudio.github.io/shinythemes/
library(tidyverse)
library(mapview)
library(leaflet)
library(maps)
library(mapproj)

#census_api_key("d780304953ca8d5be8b1878b96b27aeaf557e62d")
options(tigris_use_cache = TRUE)

counties <- readRDS("../data/counties.rds")
states <- get_acs(geography = "state", 
                  variables = "B19013_001",
                  survey = "acs5")$NAME %>% 
  unique()

income_time_data <- readRDS("../data/income_time.rds")

source("helpers.R")

theme_custom <- function() {
  theme_bw() +
    theme (
      axis.title = element_text(size = 16),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
}

# Define the user interface
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  navbarPage(title = "Exploring Quality of Life Using US Census Data",
             tabPanel(title = "About",
                      titlePanel("About The App"),
                      h4("General Info"),
                      p("Welcome! This is a US Census Bureau data explorer created
                        using R Shiny. The purpose of this app is to give users a glimpse
                        into the vast amount of data the US Census Burea has to offer.
                        In particular, the app aims to provide some background about the
                        quality of life across the united states using population, 
                        income, and poverty metrics. Enjoy!"),
                      h4("Population"),
                      p("This app allows users to interact with two different graphics
                        inovlving population. The first map allows users to view the population 
                        density at both the state level and county level (given a particular
                        state). The data is pulled from the American Community Survey, which is an 
                        ongoing surrvey that is used to measure changing social and economic characteristics.
                        The second map
                        allows users to see the percentage of a certain race across the United States
                        for each county. This data is pulled from the 2010 Decennial census, which is 
                        conducted every 10 years and is used to get specific counts for metrics.", a("Click here to learn more", href="https://www.census.gov/programs-surveys/decennial-census/about/census-acs.html")),
                      h4("Income"),
                      h4("Poverty")),
             tabPanel(title = "Population",
                      sidebarLayout(
                        
                        sidebarPanel(
                          selectInput(
                            inputId =  "select_year", 
                            label = "Select year:", 
                            choices = 2014:2018,
                            selected = 2018,
                            width = "40%"
                          ),
                          
                          radioButtons(inputId = "geo", 
                                       label = "Geography",
                                       choices = list("State" = "s", 
                                                      "Country" = "c"), 
                                       selected = "c",
                                       inline = TRUE),
                          
                          textInput(inputId = "state_input",
                                    label = "State: ",
                                    width = "22%",
                                    value = "NC"),
                          
                          radioButtons(inputId = "span", 
                                       label = strong(h5("Estimate Span ",
                                                 (a(h6("what is this?"), 
                                                   href = "https://www.census.gov/programs-surveys/acs/guidance/estimates.html", 
                                                   )))),
                                       choices = list("1 year" = 1, 
                                                      "5 year" = 5), 
                                       selected = 1,
                                       inline = TRUE),
                          actionButton(inputId = "get_gen_pop",
                                       label = "Pull data"),
                          
                          hr(),
                          
                          selectInput("select_race", 
                                      label = "Choose a variable to display",
                                      choices = c("Percent White", "Percent Black",
                                                  "Percent Hispanic", "Percent Asian"),
                                      selected = "Percent White"),
                          
                          sliderInput("pop_range", 
                                      label = "Range of interest:",
                                      min = 0, max = 100, value = c(0, 100)),
                          
                          actionButton(inputId = "get_race_pop",
                                       label = "Pull data")
                          
                        ),
                        
                        mainPanel(
                          h4("Population Density for States and Counties"),
                          leafletOutput(outputId = "gen_population", height = 300),
                          h4("Percentage of County Populations By Race"),
                          plotOutput(outputId = "race_map")
                        )
  
                      )),
             
             tabPanel(title = "Income",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput(
                            inputId =  "select_year_income", 
                            label = "Select year:", 
                            choices = 2014:2018,
                            selected = 2018,
                            width = "40%"
                          ),
                          
                          radioButtons(inputId = "geo_income", 
                                       label = "Geography",
                                       choices = list("State" = "s", 
                                                      "Country" = "c"), 
                                       selected = "c",
                                       inline = TRUE),
                          
                          textInput(inputId = "state_input_income",
                                    label = "State: ",
                                    width = "22%",
                                    value = "NC"),
                          
                          radioButtons(inputId = "span_income", 
                                       label = strong(h5("Estimate Span ",
                                                         (a(h6("what is this?"), 
                                                            href = "https://www.census.gov/programs-surveys/acs/guidance/estimates.html", 
                                                         )))),
                                       choices = list("1 year" = 1, 
                                                      "5 year" = 5), 
                                       selected = 1,
                                       inline = TRUE),
                          
                          actionButton(inputId = "get_income_map",
                                       label = "Pull data"),
                          hr(),
                          
                          selectInput(inputId = "income_states_select", 
                                      label = "Select states: ", 
                                      choices = states,
                                      multiple = TRUE),
                          
                          actionButton(inputId = "get_income_timeline",
                                       label = "Pull data"),
                        ),
                      
                        mainPanel(
                          h4("Median Annual Income for States and Counties"),
                          leafletOutput(outputId = "income_map", height = 300),
                          h4("Median Annual Income Across Time"),
                          plotOutput(outputId = "income_timeline"),
                        )
                      
                      )),
             
             tabPanel(title = "Poverty"))
)

# Server function
server <- function(input, output) {
  
  observe({
    if(input$geo == "c") {
      shinyjs::hide("state_input", time = 0.0)
    }
    else {
      shinyjs::show("state_input", time = 0.0)
    }
    if(input$geo_income == "c") {
      shinyjs::hide("state_input_income", time = 0.0)
    }
    else {
      shinyjs::show("state_input_income", time = 0.0)
    }
  })
  
  gen_data <- eventReactive(input$get_gen_pop, {
    
    get_acs(geography = if(input$geo == "s") "county" else "state", 
            variables = "B01003_001",
            shift_geo = if(input$geo == "c") TRUE else FALSE,
            geometry = TRUE,
            year = as.integer(input$select_year), 
            survey = if(input$span == 1) "acs1" else "acs5",
            state = if(input$geo == "s") input$state_input else NULL)
  })
  
  output$gen_population <- renderLeaflet({
    mapview(gen_data(), zcol = "estimate")@map
  })
  
  race_data <- eventReactive(input$get_race_pop, {
    
    race <- switch(input$select_race, 
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$select_race, 
                    "Percent White" = "blue",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "orange",
                    "Percent Asian" = "red")
    
    legend <- switch(input$select_race, 
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    percent_map(var = race, 
                color = color, 
                legend.title = legend,
                min = input$pop_range[1],
                max = input$pop_range[2])
  })
  
  output$race_map <- renderPlot({
    race_data()
  })
  
  income_data <- eventReactive(input$get_income_map, {
    
    get_acs(geography = if(input$geo_income == "s") "county" else "state", 
            variables = "B19013_001",
            shift_geo = if(input$geo_income == "c") TRUE else FALSE,
            geometry = TRUE,
            year = as.integer(input$select_year_income), 
            survey = if(input$span_income == 1) "acs1" else "acs5",
            state = if(input$geo_income == "s") input$state_input_income else NULL)
  })
  
  output$income_map <- renderLeaflet({
    mapview(income_data(), zcol = "estimate")@map
  })
  
  income_time_data_display <- eventReactive(input$get_income_timeline, {
    income_time_data %>% 
      filter(name %in% input$income_states_select)
  })
  
  output$income_timeline <- renderPlot({
    income_time_data_display() %>% 
      ggplot(mapping = aes(x = year, y = estimate, color = name)) +
      geom_line(size = 1.5) +
      labs(x = "Year", y = "Median Annual Income") + 
      theme_custom()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
