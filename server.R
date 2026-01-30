library(shiny)
shinyServer(function(input, output, session) {
  
  # Variable réactive pour stocker le résultat du script
  resultat_script <- reactiveVal(NULL)
  
  # Logique pour exécuter le script lors du clic sur le bouton
  observeEvent(input$runScript, {
    req(input$file1)
    
    # Lit le fichier CSV déposé par l'utilisateur
    df <- read.csv(input$file1$datapath,sep=";")
    
    print(df)
    
    #############################################################
    # Exécute votre script R. Ici, un simple résumé
    tablo <- na.omit(df)
    
    print("tablo : ")
    print(tablo)
    
    resultat <- summary(tablo)
    
    #### Lancer le script ####
    
    listeEch <- levels(as.factor(tablo$Echantillon))
    nivInfec <- c(0,1,2,3,4,5)
    multipInfec <- c(0,1,5,30,70,95)
    # Créer une colonne InfectionPourcentage dans le tableau tablo en remplaçant les niveaux d'infection par les pourcentages
    tablo$InfectionPourcentage <- multipInfec[match(tablo$Infection, nivInfec)]
    
    # Créer une colonne InfecMuc pour calculer le pourcentage d'infection pour Mucoro
    tablo$InfecMucoro <- (tablo$Mucoro * tablo$InfectionPourcentage) / 100
    
    # Créer une colonne InfecGlomero pour calculer le pourcentage d'infection pour Glomero
    tablo$InfecGlomero <- (tablo$Glomero * tablo$InfectionPourcentage) / 100
    
    # Définir la structure du tableau de sortie avec 13 colonnes pour les nouveaux indices
    sortieEch <- array(NA, c(length(listeEch), 13))
    
    for (ii in 1:length(listeEch)) {
      
      nbFrag <- length(tablo[tablo$Echantillon == listeEch[ii], 1])
      nbFragMyco <- length(tablo[(tablo$Echantillon == listeEch[ii]) & (tablo$Infection > 0) & (is.na(tablo$Infection) == FALSE), 1])
      
      ## Fréquence de mycorhization
      Fpourcent <- nbFragMyco / nbFrag * 100
      
      ## Calcul des n1 à n5
      minterm <- rep(NA, 6) # 6=long niveau infect
      for (jj in 1:length(nivInfec)){
        nbFragInfec <- length(tablo[(tablo$Echantillon == listeEch[ii]) & (tablo$Infection == nivInfec[jj]) & (is.na(tablo$Infection) == FALSE), 1])    
        minterm[jj] <- nbFragInfec * multipInfec[jj]
      }
      
      ## Intensité globale de mycorhization
      Mpourcent <- sum(minterm) / nbFrag
      
      ## Intensité de mycorhization des fragments mycorhizés
      petitmpourcent <- (Mpourcent * nbFrag / nbFragMyco)
      
      
      ## Calcul des mA1 à mA3 (a_glo)
      mA <- rep(NA, 4) # 4= nb niveau arb
      for (kk in 1:4){
        minterm <- rep(NA, 6)
        for (jj in 1:length(nivInfec)){
          nbFragA <- length(tablo[(tablo$Echantillon == listeEch[ii]) & (tablo$Infection == nivInfec[jj]) & (tablo$a_glo == kk) & (is.na(tablo$Infection) == FALSE), 1])
          minterm[jj] <- nbFragA * multipInfec[jj]
        }
        mA[kk] <- (sum(minterm) / nbFragMyco) * 100 / petitmpourcent
      }
      
      ## Intensité arbusculaire de la partie mycorhizée
      petitapourcent <- (100 * mA[3] + 50 * mA[2] + 10 * mA[1]) / 100
      
      ## Intensité arbusculaire dans le système radiculaire
      Apourcent <- petitapourcent * (Mpourcent / 100)
      
      ## Calcul des mV1 à mV3 (v_glo)
      mV <- rep(NA, 4)
      for (kk in 1:4){
        minterm <- rep(NA, 6)
        for (jj in 1:length(nivInfec)){
          nbFragA <- length(tablo[(tablo$Echantillon == listeEch[ii]) & (tablo$Infection == nivInfec[jj]) & (tablo$v_glo == kk) & (is.na(tablo$Infection) == FALSE), 1])    
          minterm[jj] <- nbFragA * multipInfec[jj]
        }
        mV[kk] <- (sum(minterm) / nbFragMyco) * 100 / petitmpourcent
      }
      
      ## Intensité vesicules de la partie mycorhizée
      petitvpourcent <- (100 * mV[3] + 50 * mV[2] + 10 * mV[1]) / 100
      
      ## Intensité vesicules dans le système radiculaire
      Vpourcent <- petitvpourcent * (Mpourcent / 100)
      
      ## Calcul des mA1 à mA3 pour a_muc (a%_Muc et A%_Muc)
      mA_muc <- rep(NA, 4) # 4= nb niveau arb_muc
      for (kk in 1:4){
        minterm <- rep(NA, 6)
        for (jj in 1:length(nivInfec)){
          nbFragA <- length(tablo[(tablo$Echantillon == listeEch[ii]) & (tablo$Infection == nivInfec[jj]) & (tablo$a_muc == kk) & (is.na(tablo$Infection) == FALSE), 1])
          minterm[jj] <- nbFragA * multipInfec[jj]
        }
        mA_muc[kk] <- (sum(minterm) / nbFragMyco) * 100 / petitmpourcent
      }
      
      ## Intensité arbusculaire de la partie mycorhizée pour a_muc
      petitapourcent_muc <- (100 * mA_muc[3] + 50 * mA_muc[2] + 10 * mA_muc[1]) / 100
      
      ## Intensité arbusculaire dans le système radiculaire pour a_muc
      Apourcent_muc <- petitapourcent_muc * (Mpourcent / 100)
      
      ## Calcul de M%_Glomero en se basant sur InfecGlomero
      Mpourcent_Glomo <- sum(tablo[(tablo$Echantillon == listeEch[ii]), "InfecGlomero"]) / nbFrag
      
      ## Calcul de M%_Mucoro en se basant sur InfecMucoro
      Mpourcent_Mucoro <- sum(tablo[(tablo$Echantillon == listeEch[ii]), "InfecMucoro"]) / nbFrag
      
      ## Remplacer NaN par 0 si Mpourcent est 0
      if (Mpourcent == 0) {
        Fpourcent <- 0
        Mpourcent <- 0
        petitmpourcent <- 0
        petitapourcent <- 0
        Apourcent <- 0
        petitvpourcent <- 0
        Vpourcent <- 0
        Apourcent_muc <- 0
        petitapourcent_muc <- 0
      }
      
      ## Récupérer toutes les valeurs calculées pour cet échantillon dans le tableau sortieEch
      sortieEch[ii, 1] <- listeEch[ii]
      sortieEch[ii, 2:13] <- signif(c(nbFrag, Fpourcent, Mpourcent, petitmpourcent, petitapourcent, Apourcent, petitvpourcent, Vpourcent, petitapourcent_muc, Apourcent_muc, Mpourcent_Glomo,Mpourcent_Mucoro), 2)
    }
    
    ## On écrit le tableau sortieEch dans un fichier  
    colnames(sortieEch) <- c("Echantillon", "NombreFragments", "F%", "M%", "m%", "a%_Glo", "A%_Glo", "v%_Glo", "V%_Glo", "a%_Muc", "A%_Muc","M%_Glomero","M%_Mucoro")
    
    ### Indiquer le nom du fichier de sortie ####
    write.table(sortieEch, file="SyntheseEndoMyco_MG_fini.csv", row.names=FALSE, sep=";")
    
    
    ###############################################################
    
    # Met à jour la variable réactive avec le résultat
    resultat_script(sortieEch)
    
    # Affiche le résumé dans l'interface pour l'utilisateur
    output$summary <- renderPrint({
      sortieEch
    })
    
  })
  
  # Logique pour le bouton de téléchargement
  output$downloadData <- downloadHandler(
    # Nom du fichier à télécharger
    filename = function() {
      paste("resultats-", Sys.Date(), ".csv", sep = "")
    },
    
    # Contenu du fichier
    content = function(file) {
      # Assurez-vous que le script a été exécuté et qu'il y a des données
      req(resultat_script()) 
      
      # Convertissez le résultat en data frame si nécessaire (le résumé est une liste)
      # Puis écrivez le data frame dans le fichier CSV pour le téléchargement
      # Si le résultat est déjà un data frame, vous n'avez pas besoin de cette conversion
      write.csv(as.data.frame(resultat_script()), file, row.names = TRUE)
    }
  )
})