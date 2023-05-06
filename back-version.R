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

vaccine_dataset <- read.csv("./Data/vaccine.csv") # , header = TRUE, sep = ";", stringsAsFactors = FALSE

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
# unique_age                = unique(vaccine_dataset$Age)
# unique_socio_profession   = unique(vaccine_dataset$Socio.professionelle)
# unique_taille_ville       = unique(vaccine_dataset$Taille.ville)
# unique_position_politique = unique(vaccine_dataset$Positionnement.politique)
# unique_entourage          = unique(vaccine_dataset$Entourage)
# unique_passif_covid       = unique(vaccine_dataset$Passif.Covid)
# unique_mesure_gene        = unique(vaccine_dataset$Gne.des.Mesures)
# unique_confiance_gouverne = unique(vaccine_dataset$Confiance.Gouvernement)
# unique_scepticisme_mesure = unique(vaccine_dataset$Scepticisme.Mesure)
# unique_scepticisme_pandem = unique(vaccine_dataset$Scepticisme.Pandmie)
# unique_scepticisme_vaccin = unique(vaccine_dataset$Scepticisme.Vaccin)

shiny::shinyApp(
    ui = fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Vaccin Prediction",
                  tabPanel("Home",
                           sidebarPanel(
                            tags$h3("Input:"),
  #                           selectInput(inputId = "dropdownGender", label = "Select your gender:", choices = unique_gender),
  # # verbatimTextOutput(outputId = "selectedGender"),
  #                           selectInput(inputId = "dropdownAge", label = "Select your age category:", choices = unique_age),
  #                           selectInput(inputId = "dropdownSocio", label = "Select your socio profession category:", choices = unique_socio_profession),
  #                           selectInput(inputId = "dropdownCity", label = "Select your ville size category:", choices = unique_taille_ville),
  #                           selectInput(inputId = "dropdownPosPoli", label = "Select your politic position category:", choices = unique_position_politique),
  #                           selectInput(inputId = "dropdownEntourage", label = "Select your entourage category:", choices = unique_entourage),
  #                           selectInput(inputId = "dropdownPassifCovid", label = "Select your passif covid category:", choices = unique_passif_covid),
  #                           selectInput(inputId = "dropdownGeneMesures", label = "Select your genes mesures category:", choices = unique_mesure_gene ),
  #                           selectInput(inputId = "dropdownConfianceGov", label = "Select your confidance to gverment category:", choices = unique_confiance_gouverne),
  #                           selectInput(inputId = "dropdownScepMesu", label = "Select your scepticisme mesure category:", choices = unique_scepticisme_mesure),
  #                           selectInput(inputId = "dropdownScepPand", label = "Select your scepticisme pandemy category:", choices = unique_scepticisme_pandem),
  #                           selectInput(inputId = "dropdownScepVacc", label = "Select your scepticisme vaccin category:", choices = unique_scepticisme_vaccin),




                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Header 1"),
                             
                             h4("Output 1"),
                             verbatimTextOutput("txtout"),
                             
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
  
  output


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


  # vaccine_dataset$Catgorie <- as.numeric(vaccine_dataset$Catgorie) 


  # we can see that we have 77 column
  dim(vaccine_dataset)


  # Machine Learning 

  # Split the dataset into training and test sets

  nRuns = 100
  rf_accuracy = c()
  for (run in 1:nRuns) {
    train_indices <- sample(nrow(vaccine_dataset), nrow(vaccine_dataset) * 0.8)
    train_data <- vaccine_dataset[train_indices, ]
    test_data <- vaccine_dataset[-train_indices, ]

    # train_data <- upSample(train_data[,-1], train_data$Catgorie, yname="Catgorie")
    # table(train_data$Catgorie)
    
    
    
    # # Train random forest model
    rf_model <- randomForest(x = train_data[, all_col_names], y = train_data$Catgorie, ntree = 100, importance = TRUE)
    
    
    # # Make predictions on the test set
    predictions <- predict(rf_model, newdata = test_data[, all_col_names])
    
    # # Evaluate the accuracy of the model
    accuracy <- sum(predictions == test_data[, "Catgorie"]) / nrow(test_data)
    
    rf_accuracy = c(rf_accuracy, accuracy)
  }

  acc_rf = mean(rf_accuracy)

    predict(rf_model,vaccine_dataset[6,:])







  ##value of input text

  # output$selectedGender <- renderText({
  #   paste("You selected", input$dropdownGender)
  # })

  # output$selectedAge <- renderText({
  #   paste("You selected", input$dropdownAge)
  # })

  # output$selectedSocio <- renderText({
  #   paste("You selected", input$dropdownSocio)
  # })

  # output$selectedCity <- renderText({
  #   paste("You selected", input$dropdownCity)
  # })

  # output$selectedPosPoli <- renderText({
  #   paste("You selected", input$dropdownPosPoli)
  # })

  # output$selectedEntourage <- renderText({
  #   paste("You selected", input$dropdownEntourage)
  # })

  # output$selectedPassifCovid <- renderText({
  #   paste("You selected", input$dropdownPassifCovid)
  # })

  # output$selectedGeneMesures <- renderText({
  #   paste("You selected", input$dropdownGeneMesures)
  # })

  # output$selectedConfianceGov <- renderText({
  #   paste("You selected", input$dropdownConfianceGov)
  # })

  # output$selectedScepMesu <- renderText({
  #   paste("You selected", input$dropdownScepMesu)
  # })

  # output$selectedScepPand <- renderText({
  #   paste("You selected", input$dropdownScepPand)
  # })

  # output$selectedScepVacc <- renderText({
  #   paste("You selected", input$dropdownScepVacc)
  # })

  


# dropdownAge
# dropdownSocio
# dropdownCity
# dropdownPosPoli
# dropdownEntourage
# dropdownPassifCovid
# dropdownGeneMesures
# dropdownConfianceGov
# dropdownScepMesu
# dropdownScepPand
# dropdownScepVacc

  
})