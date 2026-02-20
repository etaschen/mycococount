library(shiny)
library(DT)
shinyServer(function(input, output, session) {
  
  # Variable réactive pour stocker le résultat du script
  resultat_script <- reactiveVal(NULL)
  
  # Logique pour exécuter le script lors du clic sur le bouton
  observeEvent(input$runScript, {
    req(input$file1)
    
    # Lit le fichier CSV déposé par l'utilisateur
    df <- read.csv(input$file1$datapath,sep=";")
    
    # Vérification de la structure du fichier (colonnes obligatoires)
    cols_requises <- c("Sample", "Global_colonisation", "Mucoro_rate", "Glomero_rate", "a_glo", "v_glo", "a_muc")
    cols_manquantes <- setdiff(cols_requises, colnames(df))
    
    if (length(cols_manquantes) > 0) {
      showNotification(paste("Error : Missing columns or incorrect separator. Miss :", paste(cols_manquantes, collapse = ", ")), type = "error", duration = NULL)
      return()
    }
    
    print(df)
    
    #############################################################
    listeEch <- levels(as.factor(data$Sample))
    nivColo <- c(0,1,2,3,4,5)
    multipColo <- c(0,1,5,30,70,95)
    # Create a column Global_colonisationPourcentage in the data table by replacing the levels of Global_colonisation with percentages
    data$Global_colonisationPourcentage <- multipColo[match(data$Global_colonisation, nivColo)]
    
    # Create a column ColoMucoro to calculate the percentage of Global_colonisation for Mucoro_rate
    data$ColoMucoro <- (data$Mucoro * data$Global_colonisationPourcentage) / 100
    
    # Create a column ColoGlomero_rate to calculate the percentage of Global_colonisation for Glomero_rate
    data$ColoGlomero_rate <- (data$Glomero_rate * data$Global_colonisationPourcentage) / 100
    
    # Define the structure of the output table with 13 columns for the new indices
    sortieEch <- array(NA, c(length(listeEch), 13))
    
    for (ii in 1:length(listeEch)) {
      
      nbFrag <- length(data[data$Sample == listeEch[ii], 1])
      nbFragMyco <- length(data[(data$Sample == listeEch[ii]) & (data$Global_colonisation > 0) & (is.na(data$Global_colonisation) == FALSE), 1])
      
      ## Frequency of mycorrhization
      Fpourcent <- nbFragMyco / nbFrag * 100
      
      ## Calculation of n1 to n5
      minterm <- rep(NA, 6) # 6=length of Colonisation levels
      for (jj in 1:length(nivColo)){
        nbFragColo <- length(data[(data$Sample == listeEch[ii]) & (data$Global_colonisation == nivColo[jj]) & (is.na(data$Global_colonisation) == FALSE), 1])    
        minterm[jj] <- nbFragColo * multipColo[jj]
      }
      
      ## Overall mycorrhization intensity
      Mpourcent <- sum(minterm) / nbFrag
      
      ## Mycorrhization intensity of colonized fragments
      petitmpourcent <- (Mpourcent * nbFrag / nbFragMyco)
      
      
      ## Calculation of mA1 to mA3 (a_glo)
      mA <- rep(NA, 4) # 4= number of a_glo levels
      for (kk in 1:4){
        minterm <- rep(NA, 6)
        for (jj in 1:length(nivColo)){
          nbFragA <- length(data[(data$Sample == listeEch[ii]) & (data$Global_colonisation == nivColo[jj]) & (data$a_glo == kk) & (is.na(data$Global_colonisation) == FALSE), 1])
          minterm[jj] <- nbFragA * multipColo[jj]
        }
        mA[kk] <- (sum(minterm) / nbFragMyco) * 100 / petitmpourcent
      }
      
      ## Arbuscular intensity of the colonized part
      petitapourcent <- (100 * mA[3] + 50 * mA[2] + 10 * mA[1]) / 100
      
      ## Arbuscular intensity in the root system
      Apourcent <- petitapourcent * (Mpourcent / 100)
      
      ## Calculation of mV1 to mV3 (v_glo)
      mV <- rep(NA, 4)
      for (kk in 1:4){
        minterm <- rep(NA, 6)
        for (jj in 1:length(nivColo)){
          nbFragA <- length(data[(data$Sample == listeEch[ii]) & (data$Global_colonisation == nivColo[jj]) & (data$v_glo == kk) & (is.na(data$Global_colonisation) == FALSE), 1])    
          minterm[jj] <- nbFragA * multipColo[jj]
        }
        mV[kk] <- (sum(minterm) / nbFragMyco) * 100 / petitmpourcent
      }
      
      ## Vesicle intensity of the colonized part
      petitvpourcent <- (100 * mV[3] + 50 * mV[2] + 10 * mV[1]) / 100
      
      ## Vesicle intensity in the root system
      Vpourcent <- petitvpourcent * (Mpourcent / 100)
      
      ## Calculation of mA1 to mA3 for a_muc (a%_Muc and A%_Muc)
      mA_muc <- rep(NA, 4) # 4= number of a_muc levels
      for (kk in 1:4){
        minterm <- rep(NA, 6)
        for (jj in 1:length(nivColo)){
          nbFragA <- length(data[(data$Sample == listeEch[ii]) & (data$Global_colonisation == nivColo[jj]) & (data$a_muc == kk) & (is.na(data$Global_colonisation) == FALSE), 1])
          minterm[jj] <- nbFragA * multipColo[jj]
        }
        mA_muc[kk] <- (sum(minterm) / nbFragMyco) * 100 / petitmpourcent
      }
      
      ## Arbuscular intensity of the colonized part for a_muc
      petitapourcent_muc <- (100 * mA_muc[3] + 50 * mA_muc[2] + 10 * mA_muc[1]) / 100
      
      ## Arbuscular intensity in the root system for a_muc
      Apourcent_muc <- petitapourcent_muc * (Mpourcent / 100)
      
      ## Calculation of M%_Glomero_rate based on ColoGlomero_rate
      Mpourcent_Glomo <- sum(data[(data$Sample == listeEch[ii]), "ColoGlomero_rate"]) / nbFrag
      
      ## Calculation of M%_Mucoro based on ColoMucoro
      Mpourcent_Mucoro <- sum(data[(data$Sample == listeEch[ii]), "ColoMucoro"]) / nbFrag
      
      ## Replace NaN with 0 if Mpourcent is 0
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
      
      ## Store all calculated values for this sample in the sortieEch table
      sortieEch[ii, 1] <- listeEch[ii]
      sortieEch[ii, 2:13] <- signif(c(nbFrag, Fpourcent, Mpourcent, petitmpourcent, petitapourcent, Apourcent, petitvpourcent, Vpourcent, petitapourcent_muc, Apourcent_muc, Mpourcent_Glomo,Mpourcent_Mucoro), 2)
    }
    
    ## Write the sortieEch table to a file  
    colnames(sortieEch) <- c("Sample", "NombreFragments", "F%", "M%", "m%", "a%_Glo", "A%_Glo", "v%_Glo", "V%_Glo", "a%_Muc", "A%_Muc","M%_Glomero_rate","M%_Mucoro")
    
    ### Specify the name of the output file ####
    write.table(sortieEch, file="SyntheseEndoMyco_MG_fin_eng.csv", row.names=FALSE, sep=";")
    
    
    
    ###############################################################
    
    # Met à jour la variable réactive avec le résultat
    resultat_script(sortieEch)
    
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
  
  # Affiche le tableau de résultats avec une mise en forme interactive (DT)
  output$summary <- renderDataTable({
    req(resultat_script())
    datatable(as.data.frame(resultat_script()), options = list(scrollX = TRUE), rownames = FALSE)
  })
})