library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(sp)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
    checkboxGroupInput("well_names", "Wells to include", unique(dattbl$well_name), selected=unique(dattbl$well_name)),
  selectInput("agg_method","Aggregation Method",c("Date Range")),
  dateRangeInput("date_range", "Date range:",
                 start = "2013-10-09",
                 end   = "2021-11-09")
    ),
  mainPanel(
    dataTableOutput("b"),
    hr(),
    dataTableOutput("z"),
    hr(),
    plotOutput("y")
    )
  )
)
server <-
  function(input,output,session){
    re <- reactive({
      dattbl %>% 
        filter(well_name %in% input$well_names, timestamp>min(input$date_range), timestamp < max(input$date_range))
      })
    re2 <- reactive({
      re() %>% 
        group_by(well_name) %>% summarize(lat=mean(lat), long=mean(long), ps_elev=mean(ps_elev), n_obs=n())
    })
    re3 <- reactive({
      SpatialPointsDataFrame(data.frame(re2()$long, re2()$lat), 
                                      proj4string=CRS("+init=EPSG:4326"),
                                      data=re2())
    })
    
      output$b <- renderDataTable({re()})
      output$z <- renderDataTable({re2()})
      output$y <- renderPlot({plot(re3())})
  }
shinyApp(ui, server)