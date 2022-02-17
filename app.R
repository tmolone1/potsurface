library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)
library(sp)
library(gstat)
library(rgdal)
library(raster)
library(fields)
library(stars)
library(sf)
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
    plotOutput("contour_map"),
    hr(),
    plotOutput("hydrograph")
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
    re4 <- reactive({
      mygrid(re3())
    })
    re5 <- reactive({
      myTPS(re3(),re4())
    })
    re6 <- reactive({
      mycontours(re5())
    })
    
    
      output$b <- renderDataTable({re()})
      output$z <- renderDataTable({re2()})
      output$contour_map <- renderPlot({
        suppressWarnings(
        ggplot() +
          geom_point(data = st_as_sf(re3()), aes(x = long, y = lat), color = "red", fill = "grey") + 
          geom_sf(data = st_as_sf(re6())) + 
          geom_sf_text(data = st_as_sf(re6()), aes(label = level), size = 2) +
          geom_sf_text(data = st_as_sf(re3()), aes(label = well_name), size = 2, nudge_y = 0.008) +
          geom_sf_text(data = st_as_sf(re3()), aes(label = round(ps_elev,2)), size = 2,nudge_y = 0.004) +
          theme_classic()
        )
      })
      output$hydrograph <- renderPlot({
        ggplot(re(), aes(x=timestamp, y=level_ft)) +
          geom_line(aes(color=well_name)) + 
          geom_point(aes(color=well_name)) +
          xlab("") +
          ylab("Depth Below Measuring Point (ft)") +
          theme_classic() +
          theme(axis.text.x=element_text(angle=60, hjust=1)) +
          scale_y_reverse() + 
          scale_x_date(date_breaks = "1 year", date_labels = "%Y")
      })
      
  }
shinyApp(ui, server)