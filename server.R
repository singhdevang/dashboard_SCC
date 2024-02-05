# server.R
library(shiny)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)

source('functions.R') # functions are in this script

server <- function(input, output) {
  
  data_reactive <- reactive({
    process_data(
      workstream_names = input$workstreamName, 
      start_month_year = input$startMonthYear,
      end_month_year = input$endMonthYear,
      health_board_trusts = input$healthBoardTrust
    )
  })
  
  
  output$linePlot <- renderPlotly({
    df <- data_reactive()
    p_line <- plot_line_graph(df, input$workstreamName)
    ggplotly(p_line)
  })
  
  output$boxPlot <- renderPlotly({
    df <- data_reactive()
    p_box <- plot_box_plot(df, input$workstreamName)  # Ensure plot_box_plot is adapted similarly
    ggplotly(p_box)
  })
}