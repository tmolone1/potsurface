library(shiny)
library(shinycssloaders)

fluidPage(
  
  # App title ----
  titlePanel("Potentiometric Surface App"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "agg_method",
                  label = "Choose an aggregation method:",
                  choices = c("Nearest data to date", "Aggregate range"),
                  selected = "Nearest data to date"),
      
      # Input: Numeric entry for number of obs to view ----
      dateInput(inputId = "date",
                   label = "input date (yyyy-mm-dd):",
                   value = '2022-01-01'),
      
      # Input: Numeric entry for number of obs to view ----
      dateInput(inputId = "date2",
                label = "input date (yyyy-mm-dd):",
                value = '2022-02-01')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
        column(3, offset = 9,
               
               radioButtons(inputId = "show_NamesFinder",
                            label = "Display:",
                            choices = c("School Names", "City Names", "Neither"),
                            selected = "School Names")
        )),
      # hr(),
      withSpinner(plotOutput(outputId = "p", click = "click_plotFinder"
      )),
      hr(),
      fluidRow(
        tableOutput("sum"))
    )
  )
)
