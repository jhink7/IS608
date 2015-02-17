# Server side script

library('shiny')
library('plyr')
library('ggplot2')

# load data
mort <- read.csv('cleaned-cdc-mortality-1999-2010.csv')

shinyServer(function(input, output){
  
  q1Plot <- function(){
    # subset
    cause <- input$cause
    filtered <- mort[mort$Year == 2010 & mort$ICD.Chapter==cause, 
                   c('State','Crude.Rate')
                   ]
    filtered <- filtered[with(filtered, order(-Crude.Rate)),]
    
    bar=gvisBarChart(filtered,
                 "State","Crude.Rate",
                 options=list(width = 400,height=800,fontSize=12,legend="none"))

    geochart=gvisGeoChart(filtered,
                          locationvar="State", colorvar="Crude.Rate",
                          options=list(region="US", displayMode="regions", height = 400,
                                       resolution="provinces",
                                       colorAxis="{colors:['#4daf4a','#fc8d62','red']}"
                          ))
    
    gt <- gvisMerge(geochart,bar,horizontal=TRUE)
    return(gt)
  }
  
  byYearPlot <- function(){
    # gather input
    cause <- input$cause
    filtered <- mort[mort$ICD.Chapter==cause, 
                   c('State','Crude.Rate', 'Year','Population', 'Deaths')
                   ]
    
    # get a weighted average for each year
    filtered$Average <- daply(filtered,.(Year),function(x) weighted.mean(x$Crude.Rate, x$Population))

    filtered$Delta <- filtered$Crude.Rate - filtered$Average
    
    myStateSettings <-'
{"showTrails":false,
    "xAxisOption":"_ALPHABETICAL",
    "yZoomedIn":false,
    "iconKeySettings":[{"key":{"dim0":"New York"}}],
    "xLambda":1,
    "playDuration":15000,
    "time":"1999",
    "orderedByY":false,
    "yZoomedDataMax":80,
    "sizeOption":"_UNISIZE",
    "xZoomedDataMax":51,
    "orderedByX":true,
    "colorOption":"4",
    "xZoomedDataMin":0,
    "yAxisOption":"6",
    "dimensions":{"iconDimensions":["dim0"]},
    "yZoomedDataMin":-100,
    "xZoomedIn":false,
    "yLambda":1,
    "nonSelectedAlpha":0.4,
    "iconType":"VBAR",
    "duration":{"timeUnit":"Y","multiplier":1},
    "uniColorForNonSelected":false}'
    
    
    gvisMotionChart(filtered, 
                    idvar='State', 
                    timevar='Year',
                    xvar='Deaths', 
                    yvar='Delta',
                    sizevar='Population',
                    options= list(chartArea='{left:0,top:0,width:600,height:800}', 
                                  #colorAxis.legend.position='none',
                                  state=myStateSettings)
    )
    
  }
  
  output$rank2010 <- renderGvis(q1Plot())
  output$byYear <- renderGvis(byYearPlot())
})