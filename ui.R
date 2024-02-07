library(shiny)
library(shinyWidgets)
library(plotly)
library(shinythemes)

ui <- navbarPage(theme = shinytheme("flatly"), title = "Safe Care Collaborative Dashboard",
                 
                 # BOX PLOTS tab
                 tabPanel("BOX PLOTS",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                pickerInput(
                                  "workstreamName",
                                  "Choose your Workstream:",
                                  choices = c("Workstream 1 - Leadership", "Workstream 2 - Community", 
                                              "Workstream 3 - Ambulatory", "Workstream 4 - Acute"),
                                  options = list(`actions-box` = TRUE, `deselect-all-text` = "Unselect",
                                                 `select-all-text` = "Select all", `none-selected-text` = "None Selected",
                                                 `dropup-auto` = FALSE),
                                  multiple = TRUE
                                ),
                                pickerInput(
                                  "healthBoardTrust",
                                  "Choose Health Board/Trust:",
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
                                  options = list(`actions-box` = TRUE, `deselect-all-text` = "Unselect",
                                                 `select-all-text` = "Select all", `none-selected-text` = "None Selected",
                                                 `dropup-auto` = FALSE),
                                  multiple = TRUE
                                ),
                                selectInput(
                                  "startMonthYear",
                                  "Start Month:",
                                  choices = c("February 2023", "March 2023", "April 2023", "May 2023", "June 2023",
                                              "July 2023", "August 2023", "September 2023", "October 2023",
                                              "November 2023", "December 2023", "January 2024", "February 2024"),
                                  selected = "February 2023"
                                ),
                                selectInput(
                                  "endMonthYear",
                                  "End Month:",
                                  choices = c("February 2023", "March 2023", "April 2023", "May 2023", "June 2023",
                                              "July 2023", "August 2023", "September 2023", "October 2023",
                                              "November 2023", "December 2023", "January 2024", "February 2024"),
                                  selected = "November 2023"
                                )
                              ),
                              mainPanel(
                                plotlyOutput("boxPlot"),
                                plotlyOutput("linePlot")
                              )
                            )
                          )
                 ),
                 
                 # LINE CHARTS tab
                 tabPanel("LINE CHARTS",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  "WorkstreamName2",
                                  "Choose your Workstream:",
                                  choices = c("Workstream 1 - Leadership", "Workstream 2 - Community",
                                              "Workstream 3 - Ambulatory", "Workstream 4 - Acute"),
                                  selected = "Workstream 1 - Leadership"
                                ),
                                pickerInput(
                                  "healthBoardTrust2",
                                  "Choose Health Board/Trust:",
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
                                  options = list(`actions-box` = TRUE, `deselect-all-text` = "Unselect",
                                                 `select-all-text` = "Select all", `none-selected-text` = "None Selected",
                                                 `dropup-auto` = FALSE),
                                  multiple = TRUE
                                ),
                                selectInput(
                                  "startMonthYear2",
                                  "Start Month:",
                                  choices = c("February 2023", "March 2023", "April 2023", "May 2023", "June 2023",
                                              "July 2023", "August 2023", "September 2023", "October 2023",
                                              "November 2023", "December 2023", "January 2024", "February 2024"),
                                  selected = "February 2023"
                                ),
                                selectInput(
                                  "endMonthYear2",
                                  "End Month:",
                                  choices = c("February 2023", "March 2023", "April 2023", "May 2023", "June 2023",
                                              "July 2023", "August 2023", "September 2023", "October 2023",
                                              "November 2023", "December 2023", "January 2024", "February 2024"),
                                  selected = "November 2023"
                                ),
                                # Inside the Future Development tabPanel, add a new selectInput for Unique IDs
                                selectInput(
                                  inputId = "uniqueId",
                                  label = "Choose a Unique ID:",
                                  choices = NULL  # Dynamically populated from the server
                                )
                              ),
                              mainPanel(
                                plotlyOutput("lineChart")
                              )
                            )
                          )
                 ),
                 
                 # ENGAGEMENT tab
                 tabPanel("ENGAGEMENT",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  "session",
                                  "Choose your Session",
                                  choices = c("Coaching Call", "Learning Session")
                                )
                              ),
                              mainPanel(
                                plotlyOutput("engagementPlot", height = "600px")
                              )
                            )
                          )
                 ),
                 
                 # PSYCHOLOGICAL SAFETY tab
                 tabPanel("PSYCHOLOGICAL SAFETY",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  "scc",
                                  "Choose your Session",
                                  choices = c("Coaching Call (Oct 2023)", "Learning Session 4 (Nov 2023)", "Coaching Call (Jan 2024)")
                                )
                              ),
                              mainPanel(
                                plotlyOutput("likertChart")
                              )
                            )
                          )
                 )
)

# Note: The server function should be adjusted to handle inputs and outputs defined here.
