library(shiny)
library(shinythemes) # add a theme: https://rstudio.github.io/shinythemes/
library(tidyverse)
library(mapview)
library(leaflet)

source("visualizing_qol/helpers.R")

#census_api_key("d780304953ca8d5be8b1878b96b27aeaf557e62d")
options(tigris_use_cache = TRUE)

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
  navbarPage(title = "Exploring Quality of Life Using US Census Data",
             tabPanel(title = "Population",
                      sidebarLayout(
                        
                        sidebarPanel(
                          selectInput(
                            inputId =  "select_year", 
                            label = h5("Select year:"), 
                            choices = 2014:2018,
                            selected = 2018,
                            width = "40%"
                          ),
                          
                          radioButtons(inputId = "geo", 
                                       label = h5("Geography"),
                                       choices = list("State" = "s", 
                                                      "Country" = "c"), 
                                       selected = "c",
                                       inline = TRUE),
                          
                          textInput(inputId = "state_input",
                                    label = "State: ",
                                    width = "22%",
                                    value = "NC"),
                          
                          radioButtons(inputId = "span", 
                                       label = h5("Estimate Span ",
                                                 (a(h6("what is this?"), 
                                                   href = "https://www.census.gov/programs-surveys/acs/guidance/estimates.html", 
                                                   ))),
                                       choices = list("1 year" = 1, 
                                                      "5 year" = 5), 
                                       selected = 1,
                                       inline = TRUE),
                          actionButton(inputId = "get_gen_pop",
                                       label = "Pull data"),
                          hr()
                        ),
                        
                        mainPanel(
                          leafletOutput(outputId = "gen_population")
                        )
  
                      )),
             tabPanel(title = "Income"))
)

# Server function
server <- function(input, output) {
  
  observe({
    shinyjs::toggleElement("state_input", input$geo == "c", time = 0.0)
  })
  
  data <- eventReactive(input$get_gen_pop, {
    
    get_acs(geography = "state", variables = "B01003_001", 
            shift_geo = TRUE, geometry = TRUE, year = 2018, survey = "acs5")
    
    get_acs(geography = if(input$geo == "s") "county" else "state", 
            variables = "B01003_001",
            shift_geo = if(input$geo == "c") TRUE else FALSE,
            geometry = TRUE,
            year = as.integer(input$select_year), 
            survey = if(input$span == 1) "acs1" else "acs5",
            state = if(input$geo == "s") input$state_input else NULL)
  })
  
  output$gen_population <- renderLeaflet({
    mapview(data(), zcol = "estimate")@map
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
