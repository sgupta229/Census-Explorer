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
                          leafletOutput(outputId = "gen_population", height = 300),
                          plotOutput(outputId = "race_map")
                        )
  
                      )),
             
             tabPanel(title = "Income",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId =  "year_income", 
                            label = "Select year:", 
                            choices = 2005:2018,
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
                          
                          hr()
                      ),
                      
                      mainPanel(
                        leafletOutput(outputId = "map_income", height = 300)
                      ))),
             
             tabPanel(title = "Poverty"))
)

# Server function
server <- function(input, output) {
  
  observe({
    shinyjs::toggleElement("state_input", input$geo == "c", time = 0.0)
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
    county_income <- get_acs(geography = if(input$geo_income == "s") "county" else "state", 
                             variables = "B19013_001", 
                             shift_geo = if(input$geo_income == "c") TRUE else FALSE, 
                             geometry = TRUE,
                             year = as.integer(input$year_income), 
                             survey = if(input$span_income == 1) "acs1" else "acs5",
                             state = if(input$geo_income == "s") input$state_input else NULL)
  })
  
  output$map_income <- renderPlot({
    mapview(income_data(), zcol = "estimate")@map
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
