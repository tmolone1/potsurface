library(shiny)

function(input, output, session) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$program,
           "E" = e_program)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    datasetInput() %>% filter(Workout==paste0("E",input$day))
  })
  
  # plot
  output$p <- renderPlot({
    isolate(p)
        
})

  # table
  output$sum<- renderTable({
    sum
    
  })
  
  
}