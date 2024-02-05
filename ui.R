library(shiny)

library(shinyWidgets)

ui <- fluidPage(
  titlePanel("Workstream Progress Dashboard"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "workstreamName",
        label = "Choose your Workstream:",
        choices = c("Workstream 1 - Leadership", "Workstream 2 - Community", "Workstream 3 - Ambulatory", "Workstream 4 - Acute"),
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Unselect", `select-all-text` = "Select all", `none-selected-text` = "None Selected", `dropup-auto` = FALSE),
        multiple = TRUE
      ),
      pickerInput(
        inputId = "healthBoardTrust",
        label = "Choose Health Board/Trust:",
        choices = c("Aneurin Bevan University Health Board",
                    "Betsi Cadwaladr University Health Board",
                    "Cardiff & Vale University Health Board",
                    "Cwm Taf Morgannwg University Health Board",
                    "Hywel Dda University Health Board",
                    "Powys Teaching Health Board",
                    "Swansea Bay University Health Board",
                    "Velindre NHS Trust",
                    "Welsh Ambulance Service Trust",
                    "Welsh Blood Service"),
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Unselect", `select-all-text` = "Select all", `none-selected-text` = "None Selected", `dropup-auto` = FALSE),
        multiple = TRUE
      ),
      selectInput(
        inputId = "startMonthYear",
        label = "Start Month:",
        choices = c("February 2023", "March 2023", "April 2023", "May 2023", "June 2023", "July 2023", "August 2023", "September 2023", "October 2023", "November 2023", "December 2023", "January 2024", "February 2024"),
        selected = "February 2023"
      ),
      selectInput(
        inputId = "endMonthYear",
        label = "End Month:",
        choices = c("February 2023", "March 2023", "April 2023", "May 2023", "June 2023", "July 2023", "August 2023", "September 2023", "October 2023", "November 2023", "December 2023", "January 2024", "February 2024"),
        selected = "November 2023"
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "boxPlot"),
      plotlyOutput(outputId = "linePlot")
    )
  )
)
