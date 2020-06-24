# Quality of Life Census Data Explorer

## Link to App

## Introduction

###General Information
Welcome! This is a US Census Bureau data explorer created
using R Shiny. The purpose of this app is to give users a glimpse
into the vast amount of data the US Census Burea has to offer.
In particular, the app aims to provide some background about the
quality of life across the United States using population, 
income, and poverty metrics. Some more information about each
section is provided below. When specifying 'State' inputs, please
provide the state's abbreviation (e.g. CA). Enjoy!

###Population
This app allows users to interact with two different graphics
involving population. The first map allows users to view the population 
density at both the state level and county level (given a particular
state). The data is pulled from the American Community Survey, which is an 
ongoing survey that is used to measure changing social and economic characteristics.
The second map allows users to see the percentage of a certain race across the United States
for each county. This data is pulled from the 2010 Decennial census, which is 
conducted every 10 years and is used to get specific counts for metrics.

###Income
Similar to population, the app includes a map of the United States
that allows users to view the median household income across states and
counties. The second graph allows users to compares states by viewing
how their median annual household income has changed over the past few
years. Both graphs pull data from the American Community Survey. The second
graph does not let users choose the estimate span, and it defaults to the 5
year span for better accuracy.

###Poverty
p("The poverty tab is slightly different. Given a year and state,
the poverty tab will output 3 grahpics. The first two are 'tract' maps, which is similar
to a 'county' map but it splits the counties into smaller areas. The first graphic
maps percent of people whose 12 month income was below the poverty level. The second
graphic is the percent of people between 16-64 (working age) that do not have a job. The third
graphic simply makes a plot of poverty vs. unemployment rate and presents a linear model
on the scatter plot. This allows the user to see how many outliers there are and note
if there are any tracts with interesting values (very high unemployment rate but low poverty). NOTE: since this tab is pulling 3 graphics at once, the data takes 10-15 seconds to fully load.

###Additional Notes
Most of the data in the app is pulled from the web in real time using APIs. 
If you have poor internet connection or request large amounts of data,
the app could appear to freeze (although it is just taking a long time
to fetch the data. If this happens,
stopping the app and restarting it should fix the issue. Please avoid
pulling more data for the same graphic if the previous 'pull data' query hasn't
fully loaded yet. If you do, you may have to restart the application.

## References

* All information and data is pulled from [www.census.gov](https://www.census.gov/) using
their APIs.
* I got inspiration for the race population map (second map under the population tab) from
[R Shiny tutorial](https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/). In particular, I used the helpers.R script to plot percent maps (the rest of the maps
were all developed by me)