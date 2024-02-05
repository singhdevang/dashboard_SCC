# app.R
library(shiny)

# Source the separate UI, server, and functions scripts
source('ui.R')
source('server.R')
source('functions.R')

# Run the app
shinyApp(ui = ui, server = server)
