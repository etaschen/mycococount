library(shiny)
shinyUI(fluidPage(
  
  # --- Ajout de CSS flashy ici ---
  tags$head(
    tags$style(HTML("
      /* Style pour le corps de la page */
      body {
        background-color: #f0f8ff; /* Bleu clair */
      }
      
      /* Style pour le titre principal */
      .titlePanel {
        background-color: #FF1493; /* Rose fluo */
        color: white; /* Texte blanc */
        padding: 15px;
        border-radius: 10px;
        text-align: center;
        font-size: 3em; /* Grande taille de police */
        font-weight: bold;
        text-shadow: 2px 2px 4px #000000; /* Ombre de texte */
      }
      
      /* Style pour le panneau latéral (sidebar) */
      .sidebarPanel {
        background-color: #32CD32; /* Vert citron fluo */
        color: #FFFFFF; /* Texte blanc */
        border-radius: 15px;
        padding: 20px;
        box-shadow: 5px 5px 15px #888888; /* Ombre */
      }
      
      /* Style pour les boutons (Exécuter et Télécharger) */
      .btn {
        background-color: #FFD700; /* Or vif */
        color: #8B0000; /* Rouge foncé */
        font-weight: bold;
        border: none;
        padding: 10px 20px;
        margin-top: 10px;
        border-radius: 8px;
        cursor: pointer;
        transition: background-color 0.3s ease; /* Transition douce au survol */
      }
      
      .btn:hover {
        background-color: #FF4500; /* Orange fluo au survol */
        color: white;
      }
      
      /* Style pour le panneau principal (mainPanel) */
      .mainPanel {
        background-color: red /* Bleu violet */
        color: #FFFFFF; /* Texte blanc */
        border-radius: 15px;
        padding: 20px;
        box-shadow: 5px 5px 15px #888888;
      }
      
      /* Style pour le texte de sortie (summary) */
      pre {
        background-color: #000000; /* Fond noir */
        color: #00FF00; /* Texte vert fluo */
        border-radius: 8px;
        padding: 15px;
        overflow-x: auto; /* Pour les longs textes */
      }

      /* Style pour les labels des inputs (Choisir un fichier CSV) */
      label {
        color: #FFFF00; /* Jaune fluo */
        font-weight: bold;
      }
    "))
  ),
  # --- Fin de l'ajout de CSS flashy ---
  
  titlePanel("Exécuter un script R et télécharger les résultats"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choisir un fichier CSV",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      actionButton("runScript", "Exécuter le script"),
      downloadButton("downloadData", "Télécharger les résultats") 
    ),
    mainPanel(
      verbatimTextOutput("summary")
    )
  )
))