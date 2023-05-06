library(shiny)
library(shinythemes)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c( "ggplot2", "groupdata2", "randomForest")


ipak(packages)

# data Loading

vaccine_dataset <- read.csv("/home/kassi/Desktop/Chrani/Data/vaccine.csv") # , header = TRUE, sep = ";", stringsAsFactors = FALSE


dim(vaccine_dataset)

# Looking to the structure of my dataset
str(vaccine_dataset)

# Getting the summary of my dataset
summary(vaccine_dataset)

# Preprocessing

# get the names of the columns
col_names <- colnames(vaccine_dataset)
old_col_names <- col_names # save the old column names, we might need them later

# unicode the col_names automatically
col_names <- iconv(col_names, "latin1", "ASCII", sub="")

# replace the old column names with the new ones
colnames(vaccine_dataset) <- col_names

# Get the name of columns that have NA values
NA_columns <- names(vaccine_dataset)[colSums(is.na(vaccine_dataset)) > 0]

# Get the count of NAs in these columns 
NA_count <- colSums(is.na(vaccine_dataset))

# print the name of the columns and the number of NA in each column
print(paste(NA_columns, NA_count[NA_columns]))

# replace the na values with the mode
for (col in NA_columns) {
  vaccine_dataset[,col][is.na(vaccine_dataset[,col])] <- mode(vaccine_dataset[,col])
}


# unique_gender             = unique(vaccine_dataset$Genre)
unique_age                = unique(vaccine_dataset$Age)
unique_socio_profession   = unique(vaccine_dataset$Socio.professionelle)
unique_taille_ville       = unique(vaccine_dataset$Taille.ville)
unique_position_politique = unique(vaccine_dataset$Positionnement.politique)
unique_entourage          = unique(vaccine_dataset$Entourage)
unique_passif_covid       = unique(vaccine_dataset$Passif.Covid)
unique_mesure_gene        = unique(vaccine_dataset$Gne.des.Mesures)
unique_confiance_gouverne = unique(vaccine_dataset$Confiance.Gouvernement)
unique_scepticisme_mesure = unique(vaccine_dataset$Scepticisme.Mesure)
unique_scepticisme_pandem = unique(vaccine_dataset$Scepticisme.Pandmie)
unique_scepticisme_vaccin = unique(vaccine_dataset$Scepticisme.Vaccin)


shiny::shinyApp(
    ui = fluidPage(
      tags$head(
    tags$style(
      HTML("
           .btn-primary {
             background-color: #FF6347;
             color : white;

           }
           ")
    )
  ),
      theme = shinytheme("cerulean"),
                navbarPage(
                  "Vaccin Prediction",
                  tabPanel("Home",
                           sidebarPanel(
                             tags$h3("Input:"),
                             
                            selectInput(inputId = "dropdownGender", label = "Select your gender:", choices = unique_gender),
                            selectInput(inputId = "dropdownAge", label = "Select your age category:", choices = unique_age),
                            selectInput(inputId = "dropdownSocio", label = "Select your socio profession category:", choices = unique_socio_profession),
                            selectInput(inputId = "dropdownCity", label = "Select your ville size category:", choices = unique_taille_ville),
                            selectInput(inputId = "dropdownPosPoli", label = "Select your politic position category:", choices = unique_position_politique),
                            selectInput(inputId = "dropdownEntourage", label = "Select your entourage category:", choices = unique_entourage),
                            selectInput(inputId = "dropdownPassifCovid", label = "Select your passif covid category:", choices = unique_passif_covid),
                            selectInput(inputId = "dropdownGeneMesures", label = "Select your genes mesures category:", choices = unique_mesure_gene ),
                            selectInput(inputId = "dropdownConfianceGov", label = "Select your confidance to gverment category:", choices = unique_confiance_gouverne),
                            selectInput(inputId = "dropdownScepMesu", label = "Select your scepticisme mesure category:", choices = unique_scepticisme_mesure),
                            selectInput(inputId = "dropdownScepPand", label = "Select your scepticisme pandemy category:", choices = unique_scepticisme_pandem),
                            selectInput(inputId = "dropdownScepVacc", label = "Select your scepticisme vaccin category:", choices = unique_scepticisme_vaccin),

                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Results : "),
                             
                              actionButton("pred_button", "Predict Now",class = "btn-primary"),
                            #  verbatimTextOutput("txtout"),
                              h4("Result: "),
                              verbatimTextOutput("prediction_result")
                           ) 
                           
                  ), 
                  tabPanel("Graphs",  mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      br(),
      plotOutput(outputId="sexPlot"),
      br(),
      plotOutput(output="sexCategoryPlot")
      
    ))

                ) # navbarPage
),
server = function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })


  output$distPlot <- renderPlot({ 
      ggplot(vaccine_dataset, aes(x=Catgorie, fill=Catgorie)) + 
      geom_bar() +
      scale_fill_manual(values = c("#bcbddc", "#756bb1", "#6e7191"), name = "Categories") +
      labs(title = "Distribution of Categories", x = "Category", y = "Count") +
      theme_minimal()
  })


  output$sexPlot <- renderPlot({ 
      ggplot(vaccine_dataset, aes(x=Genre)) + 
      geom_bar() +
      scale_fill_manual(values = c("#00bfff", "#008080", "#ff8c00"), name = "Categories") +
      labs(title = "Distribution of Sex", x = "Sex", y = "Count") +
      theme_minimal()
  })


  # Sex for each category

  Catgorie_values <- unique(vaccine_dataset$Catgorie)
  # get Homme count for each Catgorie from dataset
  Homme <- vaccine_dataset[vaccine_dataset$Genre == "Homme",]
  Homme <- table(Homme$Catgorie)


  output$sexCategoryPlot <- renderPlot({ 
    ggplot(vaccine_dataset, aes(x=Genre, fill=Catgorie)) +
    geom_bar(stat='count', position=position_dodge(width = 0.9)) +
    scale_x_discrete(labels=c("Autre", "Female", "Male")) +
    scale_fill_manual(values=c("lightblue", "#08C698", "#00a9b5"),
                      name="Death Event",
                      labels=c(Catgorie_values)) +
    labs(x="Sex",
        y="Count",
        title="Deaths by Gender and Category") +
    theme_minimal(base_size=12) +
    geom_label(stat="count", aes(label=..count..),
              position=position_dodge(width = 0.9))

  })




    ##value of input text

  # output$selectedGender <- renderText({
  #   paste("You selected", input$dropdownGender)
  # })
  
  selected_gender <- reactive({
    return(input$dropdownGender)
  })

  # output$selectedAge <- renderText({
  #   paste("You selected", input$dropdownAge)
  # })

  selected_age <- reactive({
    return(input$dropdownAge)
  })  

  # output$selectedSocio <- renderText({
  #   paste("You selected", input$dropdownSocio)
  # })

  selected_socio <- reactive({
    return(input$dropdownSocio)
  })


  # output$selectedCity <- renderText({
  #   paste("You selected", input$dropdownCity)
  # })

  selected_city <- reactive({
    return(input$dropdownCity)
  })

  # output$selectedPosPoli <- renderText({
  #   paste("You selected", input$dropdownPosPoli)
  # })

  selected_pos_poli <- reactive({
    return(input$dropdownPosPoli)
  })

  # output$selectedEntourage <- renderText({
  #   paste("You selected", input$dropdownEntourage)
  # })

  selected_entourage <- reactive({
    return(input$dropdownEntourage)
  })

  # output$selectedPassifCovid <- renderText({
  #   paste("You selected", input$dropdownPassifCovid)
  # })

  selected_passif_covid <- reactive({
    return(input$dropdownPassifCovid)
  })

  # output$selectedGeneMesures <- renderText({
  #   paste("You selected", input$dropdownGeneMesures)
  # })

  selected_gene_mesures <- reactive({
    return(input$dropdownGeneMesures)
  })

  # output$selectedConfianceGov <- renderText({
  #   paste("You selected", input$dropdownConfianceGov)
  # })

  selected_confiance <- reactive({
    return(input$dropdownConfianceGov)
  })

  # output$selectedScepMesu <- renderText({
  #   paste("You selected", input$dropdownScepMesu)
  # })

  selected_scep_mesu <- reactive({
    return(input$dropdownScepMesu)
  })

  # output$selectedScepPand <- renderText({
  #   paste("You selected", input$dropdownScepPand)
  # })

  selected_scep_pand <- reactive({
    return(input$dropdownScepPand)
  })

  # output$selectedScepVacc <- renderText({
  #   paste("You selected", input$dropdownScepVacc)
  # })

  selected_scep_vacc <- reactive({
    return(input$dropdownScepVacc)
  })




  #prediction

  output$prediction_result <- renderText({

      # Feature Engineering
      # get the columns that are not numeric.
      non_numeric_cols <- col_names[!sapply(vaccine_dataset, is.numeric)]
      # non_numeric_cols <- non_numeric_cols[-which(non_numeric_cols == "Catgorie")]
      
      # get the unique values of each non_numeric_cols then use them to create binary columns 
      for (col in non_numeric_cols) {
        unique_values <- unique(vaccine_dataset[,col])
        for (value in unique_values) {
          vaccine_dataset[,paste(col, value, sep = "_")] <- ifelse(vaccine_dataset[,col] == value, 1, 0)
        }
      }
      vaccine_dataset <- vaccine_dataset[c(1,18:ncol(vaccine_dataset))]
      
      colnames(vaccine_dataset) <- gsub(", ", "_", colnames(vaccine_dataset))
      
      colnames(vaccine_dataset) <- gsub("-", "_", colnames(vaccine_dataset))
      
      colnames(vaccine_dataset) <- gsub(" ", "_", colnames(vaccine_dataset))
      
      # replace - with _ in Catgorie column
      vaccine_dataset$Catgorie <- gsub("-", "_", vaccine_dataset$Catgorie)
      
      
      all_col_names <- names(vaccine_dataset)
      
      
      # keep only the columns that are in the feature_cols 
      vaccine_dataset <- vaccine_dataset[,all_col_names]
      
      
      for (fact in all_col_names) {
        vaccine_dataset[,fact] <- as.factor(vaccine_dataset[,fact])
      }
      
      vaccine_dataset$Catgorie <- make.names(vaccine_dataset$Catgorie)
      vaccine_dataset$Catgorie <- as.factor(vaccine_dataset$Catgorie)
      
      
      # we can see that we have 77 column
      dim(vaccine_dataset)
      
      # Machine Learning 
      
      # Split the dataset into training and test sets
      all_col_names_no_cat <- all_col_names[-which(non_numeric_cols == "Catgorie")]
      nRuns = 100
      rf_accuracy = c()
      for (run in 1:nRuns) {
        train_indices <- sample(nrow(vaccine_dataset), nrow(vaccine_dataset) * 0.8)
        
        
        # upSample 
        
        
        train_data <- vaccine_dataset[train_indices, ]
        x_test_data <- vaccine_dataset[-train_indices, -1]
        y_test_data <- vaccine_dataset[-train_indices, 1]
        
        
        
        # train_data <- upSample(train_data[,-1], train_data$Catgorie, yname="Catgorie")
        # table(train_data$Catgorie)
        
        # # Train random forest model
        
        # train and control with grid for hyperparameter tunning
        
        
        rf_model <- randomForest(x = train_data[, all_col_names_no_cat], y = train_data$Catgorie, ntree = 100, importance = TRUE)
        
        
        # # Make predictions on the test set
        predictions <- predict(rf_model, newdata = test_data)
        
        # # Evaluate the accuracy of the model
        accuracy <- sum(predictions == y_test_data) / nrow(test_data)
        
        rf_accuracy = c(rf_accuracy, accuracy)
      }
      
      acc_rf = mean(rf_accuracy)
      
      
      row_without_cat = vaccine_dataset[9, all_col_names_no_cat]
      #here we will get our value and change all of them in this row
     


      if(!identical(names(row_without_cat), names(test_data))) {
        stop("Predictor variables in new data do not match those in training data.")
      }


      # selected_gender
      # selected_age
      # selected_socio
      # selected_city
      # selected_pos_poli
      # selected_entourage
      # selected_passif_covid
      # selected_gene_mesures
      # selected_confiance
      # selected_scep_mesu
      # selected_scep_vacc
      
      

    
      
      #print(selected_gender())
      
      # pre_value = predict(rf_model,vaccine_dataset[9,all_col_names[-which(all_col_names == "Catgorie")]])
      pre_value = predict(rf_model, row_without_cat)
      
      observeEvent(input$pred_button, {
        
        binary_levels <- c("0","1")
        #before assign get levels of every variable
        if(selected_gender() == "Homme"){
          row_without_cat$Genre_Homme <- factor(1, levels = binary_levels)
        }
        else if(selected_gender() == "Femme" ){
          row_without_cat$Genre_Femme <- factor(1, levels = binary_levels )
        }
        else if(selected_gender() == "Autre"){
          row_without_cat$Genre_Autre <- factor(1, levels = binary_levels)
        }
        if(selected_age() == "18-25 ans"){
          row_without_cat$Age_18_25_ans <- factor(1, levels = binary_levels)
        }
        if(selected_age() == "16-35 ans"){
          row_without_cat$Age_26_35_ans <- factor(1, levels = binary_levels)
        }
        if(selected_age() == "36-45 ans"){
          row_without_cat$Age_36_45_ans <- factor(1, levels = binary_levels)
        }
        if(selected_age() == "46-55 ans"){
          row_without_cat$Age_46_55_ans <- factor(1, levels = binary_levels)
        }
        if(selected_age() == "Moins de 18 ans"){
          row_without_cat$Age_Moins_de_18_ans <- factor(1, levels = binary_levels)
        }
        if(selected_socio() == "Etudiants"){
          row_without_cat$Socio.professionelle_Etudiants <- factor(1, levels = binary_levels)
        }
        if(selected_socio() == "Cadres supérieurs, Professions intellectuelles supérieures"){
          row_without_cat$Socio.professionelle_Cadres_supérieurs_Professions_intellectuelles_supérieures <- factor(1, levels = binary_levels)
        }
        if(selected_socio() == "Employés"){
          row_without_cat$Socio.professionelle_Employés <- factor(1, levels = binary_levels)
        }
        if(selected_city() == "Moins de 100 000 habitants"){
          row_without_cat$Taille.ville_Moins_100_000_habitants <- factor(1, levels = binary_levels)
        }
        if(selected_city() == "Moins de 1 000 habitants"){
          row_without_cat$Taille.ville_Moins_de_1_000_habitants<- factor(1, levels = binary_levels)
        }
        if(selected_city() == "Moins de 10 000 habitants"){
          row_without_cat$Taille.ville_Moins_de_10_000_habitants<- factor(1, levels = binary_levels)
        }
        if(selected_city() == "Moins de 1 000 000 habitants"){
          row_without_cat$Taille.ville_Moins_de_1_000_000_habitants <- factor(1, levels = binary_levels)
        }
        if(selected_entourage() == "Pro-Vax"){
          row_without_cat$Entourage_Pro_vax<- factor(1, levels = binary_levels)
        }
        if(selected_entourage() == "Anti-Vax"){
          row_without_cat$Entourage_Anti_Vax<- factor(1, levels = binary_levels)
        }
        if(selected_entourage() == "Neutre"){
          row_without_cat$Entourage_Neutre<- factor(1, levels = binary_levels)
        }
        
        

          
          pre_value = predict(rf_model, row_without_cat)
          print(row_without_cat$Genre_Homme)
        
          #print(selected_entourage())

      })

      paste("The prediction value is :",pre_value)
  })
  

  
})