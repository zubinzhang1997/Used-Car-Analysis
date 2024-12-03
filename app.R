library(shiny)
library(caret)
library(xgboost)
library(dplyr)
library(randomForest)

selected_features <- read.csv("cleaned_data_for_random_forest.csv")

dummies_model <- readRDS("dummies_model.rds")
rf_model <- readRDS("rf_model.rds")
xgb_model <- readRDS("xgb_model.rds")
stacked_model <- readRDS("stacked_model.rds")

# Recreate the training data transformation
categorical_columns <- c("manufacturer", "model")
selected_features[categorical_columns] <- lapply(selected_features[categorical_columns], as.factor)

# One-hot encode using dummyVars
dummies_model <- dummyVars(" ~ .", data = selected_features, fullRank = FALSE)
train_data_transformed <- predict(dummies_model, newdata = selected_features)
train_data_transformed <- as.data.frame(train_data_transformed)

train_data_transformed$price <- selected_features$price
train_data_cols <- colnames(train_data_transformed)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$style(HTML(
      "
      body {
        font-family: 'Helvetica Neue', sans-serif;
        background: linear-gradient(to right, #ffecd2, #fcb69f);
        color: #2c3e50;
        margin: 0;
        padding: 0;
      }
      .title {
        color: #ffffff;
        background-color: #2c3e50;
        text-align: center;
        padding: 20px;
        font-size: 2.5em;
        font-weight: bold;
        border-radius: 5px;
        margin-bottom: 30px;
      }
      .container {
        max-width: 900px;
        margin: 0 auto;
      }
      .input-section {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.1);
      }
      .price-section {
        background: linear-gradient(145deg, #e0e5ec, #ffffff);
        box-shadow: 20px 20px 60px #d1d9e6, -20px -20px 60px #ffffff;
        padding: 30px;
        border-radius: 20px;
        text-align: center;
        animation: fadeIn 1.5s ease-in-out;
      }
      .price-icon {
        font-size: 4em;
        color: #2980b9;
        margin-bottom: 15px;
        animation: pulse 1.5s infinite;
      }
      .prediction {
        font-size: 4em; /* Increased font size for better visibility */
        font-weight: bold;
        color: #e74c3c;
        animation: bounceIn 1.5s ease-in-out;
        text-shadow: 0px 4px 8px rgba(231, 76, 60, 0.8); /* Subtle glow effect */
      }
      .price-title {
        font-size: 1.8em;
        font-weight: 600;
        color: #34495e;
        margin-bottom: 20px;
      }
      .btn-primary {
        background-color: #3498db;
        border-color: #2980b9;
        color: white;
        font-size: 1.1em;
        padding: 10px 20px;
        border-radius: 5px;
        margin-top: 20px;
        width: 100%;
      }
      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #1c5980;
      }
      h3 {
        color: #34495e;
        margin-bottom: 20px;
      }
      .row {
        display: flex;
        justify-content: space-between;
        gap: 20px;
      }
      .col {
        flex: 1;
      }
      @keyframes fadeIn {
        0% { opacity: 0; transform: translateY(20px); }
        100% { opacity: 1; transform: translateY(0); }
      }
      @keyframes pulse {
        0% { transform: scale(1); }
        50% { transform: scale(1.1); }
        100% { transform: scale(1); }
      }
      @keyframes bounceIn {
        0% { transform: scale(0.8); opacity: 0; }
        50% { transform: scale(1.2); opacity: 1; }
        100% { transform: scale(1); }
      }
      "
    ))
  ),
  
  div(class = "title", "Car Price Prediction App"),
  
  div(class = "container",
      div(class = "row",
          div(class = "col input-section",
              h3("Enter Car Details"),
              selectInput(
                "manufacturer",
                "Select Manufacturer:",
                choices = unique(selected_features$manufacturer),
                selected = unique(selected_features$manufacturer)[1]
              ),
              uiOutput("model_ui"),
              numericInput(
                "year",
                "Enter Year:",
                value = 2018,
                min = 1999,
                max = 2021
              ),
              numericInput(
                "odometer",
                "Enter Odometer Value (miles):",
                value = 50000,
                min = 0
              ),
              actionButton("predict", "Predict Price", class = "btn btn-primary")
          ),
          div(class = "col price-section",
              tags$div(class = "price-icon", tags$i(class = "fas fa-car")),
              tags$div(class = "price-title", "Estimated Price:"),
              tags$div(class = "prediction", verbatimTextOutput("prediction"))
          )
      )
  )
)

# Define Server
server <- function(input, output, session) {
  output$model_ui <- renderUI({
    req(input$manufacturer)
    models_for_manufacturer <- unique(selected_features$model[selected_features$manufacturer == input$manufacturer])
    selectInput(
      "model",
      "Select Model:",
      choices = models_for_manufacturer,
      selected = models_for_manufacturer[1]
    )
  })
  
  predict_price <- eventReactive(input$predict, {
    input_data <- data.frame(
      year = input$year,
      manufacturer = input$manufacturer,
      odometer = input$odometer,
      model = input$model,
      id = NA,
      price = NA,
      stringsAsFactors = FALSE
    )
    
    input_data[c("manufacturer", "model")] <- lapply(input_data[c("manufacturer", "model")], as.factor)
    input_data_transformed <- predict(dummies_model, newdata = input_data)
    input_data_transformed <- as.data.frame(input_data_transformed)
    
    missing_cols <- setdiff(train_data_cols, colnames(input_data_transformed))
    for (col in missing_cols) {
      input_data_transformed[[col]] <- 0
    }
    
    input_data_transformed <- input_data_transformed[, train_data_cols[train_data_cols != "price"]]
    
    rf_prediction <- predict(rf_model, newdata = input_data_transformed)
    xgb_input <- xgb.DMatrix(data = as.matrix(input_data_transformed))
    xgb_prediction <- predict(xgb_model, newdata = xgb_input)
    
    stacked_input <- data.frame(
      rf = rf_prediction,
      xgb = xgb_prediction
    )
    
    final_pred <- predict(stacked_model, newdata = stacked_input)
    paste("$", format(round(final_pred, 2), big.mark = ","), sep = "")
  })
  
  output$prediction <- renderText({
    predict_price()
  })
}

shinyApp(ui = ui, server = server)
