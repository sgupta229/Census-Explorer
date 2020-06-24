library(shiny)
library(shinythemes) # add a theme: https://rstudio.github.io/shinythemes/
library(tidyverse)
library(mapview)
library(leaflet)
library(maps)
library(mapproj)
library(tidycensus)
library(sf)

source("helpers.R")

#PRE PROCESS DATA

census_api_key("d780304953ca8d5be8b1878b96b27aeaf557e62d")
options(tigris_use_cache = TRUE)

counties <- readRDS("../data/counties.rds")
states <- get_acs(geography = "state", 
                  variables = "B19013_001",
                  survey = "acs5")$NAME %>% 
  unique()

income_time_data <- readRDS("../data/income_time.rds")

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

#males and females in the labor force
male_labor_tables <- paste("B23001_", formatC(seq(4,67,7), width=3, flag="0"), "E", sep="")
female_labor_tables <- paste("B23001_", formatC(seq(90,153,7), width=3, flag="0"), "E", sep="")

#males and females that are unemployed
male_unempl <- paste("B23001_", formatC(seq(8,71,7), width=3, flag="0"), "E", sep="")
female_unempl <- paste("B23001_", formatC(seq(94,157,7), width=3, flag="0"), "E", sep="")

# SHINY APP 

# Define the user interface
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  theme = shinytheme("darkly"),
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  navbarPage(title = "Exploring Quality of Life Using US Census Data",
             tabPanel(title = "About",
                      titlePanel("About The App"),
                      br(),
                      h4("General Information"),
                      p("Welcome! This is a US Census Bureau data explorer created
                        using R Shiny. The purpose of this app is to give users a glimpse
                        into the vast amount of data the US Census Burea has to offer.
                        In particular, the app aims to provide some background about the
                        quality of life across the United States using population, 
                        income, and poverty metrics. Some more information about each
                        section is provided below. When specifying 'State' inputs, please
                        provide the state's abbreviation (e.g. CA). Enjoy!"),
                      br(),
                      h4("Population"),
                      p("This app allows users to interact with two different graphics
                        involving population. The first map allows users to view the population 
                        density at both the state level and county level (given a particular
                        state). The data is pulled from the American Community Survey, which is an 
                        ongoing survey that is used to measure changing social and economic characteristics.
                        The second map
                        allows users to see the percentage of a certain race across the United States
                        for each county. This data is pulled from the 2010 Decennial census, which is 
                        conducted every 10 years and is used to get specific counts for metrics. Click", 
                        a("here", href="https://www.census.gov/programs-surveys/decennial-census/about/census-acs.html", target="_blank"),
                      "to learn more about the difference between the American Community Survey and the
                        Decennial Census."),
                      br(),
                      h4("Income"),
                      p("Similar to population, the app includes a map of the United States
                        that allows users to view the median household income across states and
                        counties. The second graph allows users to compares states by viewing
                        how their median annual household income has changed over the past few
                        years. Both graphs pull data from the American Community Survey. The second
                        graph does not let users choose the estimate span, and it defaults to the 5
                        year span for better accuracy."),
                      br(),
                      h4("Poverty"),
                      p("The poverty tab is slightly different. Given a year and state,
                        the poverty tab will output 3 grahpics. The first two are 'tract' maps, which is similar
                        to a 'county' map but it splits the counties into smaller areas. The first graphic
                        maps percent of people whose 12 month income was below the poverty level. The second
                        graphic is the percent of people between 16-64 (working age) that do not have a job. The third
                        graphic simply makes a plot of poverty vs. unemployment rate and presents a linear model
                        on the scatter plot. This allows the user to see how many outliers there are and note
                        if there are any tracts with interesting values (very high unemployment rate but low poverty). NOTE:
                        since this tab is pulling 3 graphics at once, the data takes 10-15 seconds to fully load.
                        "),
                      br(),
                      h4("Additional Notes"),
                      p("Most of the data in the app is pulled from the web in real time using APIs. 
                        If you have poor internet connection or request large amounts of data,
                        the app could appear to freeze (although it is just taking a long time
                        to fetch the data. If this happens,
                        stopping the app and restarting it should fix the issue. Please avoid
                        pulling more data for the same graphic if the previous 'pull data' query hasn't
                        fully loaded yet. If you do, you may have to restart the application.")),
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
                                                   target="_blank"
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
                                                            target="_blank"
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
                          h4("Median Annual Household Income for States and Counties"),
                          leafletOutput(outputId = "income_map", height = 300),
                          h4("Median Annual Household Income Across Time"),
                          plotOutput(outputId = "income_timeline"),
                        )
                      
                      )),
             
             tabPanel(title = "Poverty",
               sidebarLayout(
                 sidebarPanel(
                   
                   selectInput(
                     inputId =  "select_year_pov", 
                     label = "Select year:", 
                     choices = 2014:2017,
                     selected = 2017,
                     width = "40%"
                   ),
                   
                   textInput(inputId = "state_input_pov",
                             label = "State: ",
                             width = "22%",
                             value = "NC"),
          
                   actionButton(inputId = "get_pov_map",
                                label = "Pull data")
                 ),
                 
                 mainPanel(
                   h4("Percent of People Whose Income In the Last 12 Months Was Below Poverty Level"),
                   leafletOutput(outputId = "pov_map", height = 300),
                   h4("Unemployment Rate for Males and Females Between Ages 16-64"),
                   leafletOutput(outputId = "unempl_map", height = 300),
                   h4("Correlation between Poverty and Unemployment (Linear Model)"),
                   plotOutput(outputId = "linear_model")
                 )
             ))
))

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
  
  pov_data <- eventReactive(input$get_pov_map, {
      get_acs(geography = "tract", 
              variables = "B17001_002",  
              summary_var = 'B17001_001', 
              state = input$state_input_pov, 
              geometry = TRUE, 
              year = 2017) %>%
        rename(pop = summary_est) %>%
        filter(pop>0)%>%
        mutate(pov_rate = estimate/pop) %>%
        select(GEOID, NAME, pov_rate, pop) %>% 
        mutate(pov_rate_perc = pov_rate*100)
  })
  
  unempl_data <- eventReactive(input$get_pov_map, {
    total_labor_force <- 
      get_acs(geography='tract', 
              variables = c(male_labor_tables, female_labor_tables), 
              state=input$state_input_pov, 
              year=2017)%>% 
      group_by(GEOID) %>%
      summarize(labor_force_est = sum(estimate, na.rm=T))
    
    total_unempl <- 
      get_acs(geography='tract', 
              variables = c(male_unempl, female_unempl), 
              state=input$state_input_pov, 
              year=2017)%>%
      group_by(GEOID) %>%
      summarize(unempl_est = sum(estimate, na.rm=T))
    
      left_join(total_labor_force, 
                total_unempl, 
                by=c('GEOID'='GEOID')) %>%
        filter(labor_force_est >0) %>%
        mutate(unempl_rate = (unempl_est/labor_force_est) * 100) %>% 
        left_join(pov_data(), by=c("GEOID"="GEOID")) %>% 
        st_as_sf()
    
  })
  
  output$pov_map <- renderLeaflet({
    mapview(pov_data(), zcol = "pov_rate_perc")@map
  })
  
  output$unempl_map <- renderLeaflet({
    mapview(unempl_data(), zcol = "unempl_rate")@map
  })
  
  output$linear_model <- renderPlot({
    ggplot() + 
      geom_point(aes(x=unempl_data()$unempl_rate, y=unempl_data()$pov_rate_perc), alpha=.5) +
      geom_smooth(aes(x=unempl_data()$unempl_rate, y=unempl_data()$pov_rate_perc), method='lm') +
      ylab("Poverty Rate") + xlab("Unemployment Rate") + 
      theme_custom()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
