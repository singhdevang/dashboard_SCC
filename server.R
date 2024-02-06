# server.R
library(shiny)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)

source('functions.R') # functions are in this script

server <- function(input, output, session) {
  
  # Your existing reactive for processing data for the "Plots and Line" tab
  data_reactive <- reactive({
    process_data(
      workstream_names = input$workstreamName, 
      start_month_year = input$startMonthYear,
      end_month_year = input$endMonthYear,
      health_board_trusts = input$healthBoardTrust
    )
  })
  
  # Existing outputs for the "Plots and Line" tab
  output$linePlot <- renderPlotly({
    df <- data_reactive()
    p_line <- plot_line_graph(df, input$workstreamName)
    ggplotly(p_line)
  })
  
  output$boxPlot <- renderPlotly({
    df <- data_reactive()
    p_box <- plot_box_plot(df, input$workstreamName)
    ggplotly(p_box)
  })
  
  # Reactive expression for processed data for the "Future Development" tab
  processed_data <- reactive({
    clean_and_rename(
      Workstream_names = input$WorkstreamName,
      Start_month_year = input$startMonthYear2,
      End_month_year = input$endMonthYear2,
      Health_board_trusts = input$healthBoardTrust2
    )
  })
  
  # Dynamically populate the choices for the unique ID dropdown based on the processed data
  observe({
    updateSelectInput(session, "uniqueId",
                      choices = unique(processed_data()$`Unique ID`))
  })
  
  # Create a reactive subset of data based on the selected unique ID
  selected_data <- reactive({
    req(input$uniqueId)  # Ensure that input$uniqueId is not NULL
    filter(processed_data(), `Unique ID` == input$uniqueId)
  })
  
  # Render the line plot as a Plotly plot based on the selected unique ID
  output$lineChart <- renderPlotly({
    req(selected_data())  # Ensure that selected_data is not NULL
    p_line2 <- create_run_chart_by_id(selected_data(), input$uniqueId)
    ggplotly(p_line2)  # Convert the ggplot object to a Plotly object
  })
}
