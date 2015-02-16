# UI script

library('shiny')
library('ggplot2')
library('googleVis')

# read provided, cleaned data in
# note that in a production system we would not have the ui read from the datasource directly
# a webservice/restful endpoit should provide this data
mort <- read.csv('cleaned-cdc-mortality-1999-2010.csv')

cause <- as.character(unique(mort$ICD.Chapter))

# use shiny to create a basic page layout.  Sidebar that isolates our main conceptual filter (cause of death) 
# and a main panel for our visualizations.

shinyUI(pageWithSidebar(headerPanel('State Mortality by Cause'),sidebarPanel(selectInput("cause", "Cause: ", cause, selected='Neoplasms')),
  mainPanel(
    tabsetPanel(tabPanel("2010", htmlOutput('values')), tabPanel("By Year", htmlOutput('motion'))
    )
  )
))