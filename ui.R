library(shiny)
library(shinythemes)
library(DT)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Mycorrhization rate indexes"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parameters"),
      fileInput("file1", "Select CSV file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      helpText("Column separator must be semicolon (;)."),
      tags$hr(),
      actionButton("runScript", "Start analysis", class = "btn-primary", icon = icon("play")),
      tags$hr(),
      downloadButton("downloadData", "Downlowd results", class = "btn-success") 
    ),
    mainPanel(
      DTOutput("summary")
    )
  )
))