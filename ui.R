library(shiny)
library(shinythemes)
library(DT)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Analyse de Mycorhization"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Paramètres"),
      fileInput("file1", "Choisir un fichier CSV",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      helpText("Le séparateur doit être le point-virgule (;)."),
      tags$hr(),
      actionButton("runScript", "Lancer l'analyse", class = "btn-primary", icon = icon("play")),
      tags$hr(),
      downloadButton("downloadData", "Télécharger les résultats", class = "btn-success") 
    ),
    mainPanel(
      DTOutput("summary")
    )
  )
))