library(shiny)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(likert)


source('coherent.R')
# Assuming 'functions.R' includes necessary data processing and plotting functions
# Make sure to adjust the function calls if necessary
server <- function(input, output, session) {
  
  # Existing data reactive remains unchanged
  data_reactive <- reactive({
    process_and_clean_data(
      workstream_names = input$workstreamName, 
      start_month_year = input$startMonthYear,
      end_month_year = input$endMonthYear,
      health_board_trusts = input$healthBoardTrust
    )
  })
  
  # Updated renderPlotly for Box Plot to include health_board_trusts
  output$boxPlot <- renderPlotly({
    df <- data_reactive()
    # Now passing health_board_trusts to the plotting function
    p_box <- plot_box_plot(df, input$workstreamName, input$healthBoardTrust)
    ggplotly(p_box)
  })
  
  # Updated renderPlotly for Line Plot to include health_board_trusts
  output$linePlot <- renderPlotly({
    df <- data_reactive()
    # Now passing health_board_trusts to the plotting function
    p_line <- plot_line_graph(df, input$workstreamName, input$healthBoardTrust)
    ggplotly(p_line)
  })
  
  # Reactive expression for processed data for the "LINE CHARTS" tab
  processed_data <- reactive({
    process_and_clean_data(
      workstream_names = input$WorkstreamName2,
      start_month_year = input$startMonthYear2,
      end_month_year = input$endMonthYear2,
      health_board_trusts = input$healthBoardTrust2
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
  
  # Render line chart based on the selected unique ID
  output$lineChart <- renderPlotly({
    req(selected_data())  # Ensure that selected_data is not NULL
    p_line2 <- create_line_chart_by_id(selected_data(), input$uniqueId)
    ggplotly(p_line2)  # Convert the ggplot object to a Plotly object
  })
  
  # Reactive for "ENGAGEMENT" tab
  engagement <- reactive({
    plot_scc_sessions(
      session_type = input$session
    )
  })
  
  # Render engagement plot
  output$engagementPlot <- renderPlotly({
    df <- engagement()
    ggplotly(df)
  })
  
  # Reactive for "PSYCHOLOGICAL SAFETY" tab
  psych_safety <- reactive({
    plot_likert(
      scc_session = input$scc
    )
  })
  
  # Render psychological safety plot
  output$likertChart <- renderPlotly({
    req(psych_safety())
    ggplotly(psych_safety())
  })
  
}


