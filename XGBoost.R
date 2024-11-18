library(caret)
library(xgboost)
library(dplyr)

data <- read.csv("cleaned_data_for_random_forest.csv")

# Convert categorical columns to factors
categorical_columns <- c("manufacturer", "model")  # Adjust as necessary
data[categorical_columns] <- lapply(data[categorical_columns], as.factor)

# One-hot encode using caret's dummyVars (ensure no categories are missing)
dummies_model <- dummyVars(" ~ .", data = data, fullRank = FALSE)
data_transformed <- predict(dummies_model, newdata = data)
data_transformed <- as.data.frame(data_transformed)

# Ensure 'price' column is included in transformed data (if not already)
if ("price" %in% colnames(data)) {
  data_transformed$price <- data$price
}

# Train-Test Split (80-20)
set.seed(123)
train_index <- createDataPartition(data$price, p = 0.8, list = FALSE)
train_data <- data_transformed[train_index, ]
test_data <- data_transformed[-train_index, ]

# Prepare data for XGBoost (convert data frame to DMatrix format)
train_matrix <- xgb.DMatrix(data = as.matrix(train_data[ , -which(names(train_data) == "price")]),
                            label = train_data$price)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data[ , -which(names(test_data) == "price")]))

# Set parameters for XGBoost
params <- list(
  objective = "reg:squarederror",  
  eta = 0.1,                       
  max_depth = 6,                   
  subsample = 0.8,                
  colsample_bytree = 0.8           
)

# Train the XGBoost model
set.seed(123)
xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 100,                   # Number of boosting rounds
  watchlist = list(train = train_matrix),
  verbose = 1
)

# Predict on test data
xgb_predictions <- predict(xgb_model, newdata = test_matrix)

# Model Evaluation
rmse_xgb <- sqrt(mean((xgb_predictions - test_data$price)^2))
mape_xgb <- mean(abs((xgb_predictions - test_data$price) / test_data$price)) 
r2_xgb <- 1 - sum((xgb_predictions - test_data$price)^2) / sum((mean(train_data$price) - test_data$price)^2)

cat("XGBoost Model - RMSE:", rmse_xgb, ", MAPE:", mape_xgb, ", R-squared:", r2_xgb, "\n")

# Feature Importance (Plot)
importance <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix = importance)






### Model Stacking (Ensemble) ###
# Combine predictions from Random Forest and XGBoost in a data frame
stacked_data <- data.frame(
  rf = rf_predictions,  
  xgb = xgb_predictions,
  actual = test_data$price
)

# Train a linear model on the predictions to create a meta-model
stacked_model <- lm(actual ~ rf + xgb, data = stacked_data)

# Make final predictions using the stacked model
stacked_predictions <- predict(stacked_model, newdata = stacked_data)

# Evaluate the Stacked Model
rmse_stacked <- sqrt(mean((stacked_predictions - test_data$price)^2))
mape_stacked <- mean(abs((stacked_predictions - test_data$price) / test_data$price))
r2_stacked <- 1 - sum((stacked_predictions - test_data$price)^2) / sum((mean(train_data$price) - test_data$price)^2)
cat("Stacked Model - RMSE:", rmse_stacked, ", MAPE:", mape_stacked, ", R-squared:", r2_stacked, "\n")




# User Input Prediction
year <- as.numeric(readline(prompt = "Enter the year: "))
manufacturer <- readline(prompt = "Enter the manufacturer (e.g., Audi, Ford, etc.): ")
odometer <- as.numeric(readline(prompt = "Enter the odometer value: "))
model <- readline(prompt = "Enter the model (e.g., A4, F-150, etc.): ")

input_data <- data.frame(
  year = year,
  manufacturer = manufacturer,
  odometer = odometer,
  model = model,
  id = NA,      
  price = NA,  
  stringsAsFactors = FALSE
)

# Process and encode input_data
input_data[c("manufacturer", "model")] <- lapply(input_data[c("manufacturer", "model")], as.factor)
input_data_transformed <- predict(dummies_model, newdata = input_data)
input_data_transformed <- as.data.frame(input_data_transformed)

# Ensure input_data_transformed has the same columns as the training data
missing_cols <- setdiff(colnames(train_data), colnames(input_data_transformed))
for (col in missing_cols) {
  input_data_transformed[[col]] <- 0
}
input_data_transformed <- input_data_transformed[, colnames(train_data)]

# Predictions
rf_prediction <- predict(rf_model, newdata = input_data_transformed)

# Align input data columns to match training data (excluding the 'price' column)
input_data_transformed <- input_data_transformed[, colnames(train_data)[colnames(train_data) != "price"]]

# Predictions
rf_prediction <- predict(rf_model, newdata = input_data_transformed)

# Prepare input data for XGBoost
xgb_prediction <- predict(xgb_model, newdata = xgb.DMatrix(data = as.matrix(input_data_transformed)))

stacked_input <- data.frame(
  rf = rf_prediction,
  xgb = xgb_prediction
)
stacked_prediction <- predict(stacked_model, newdata = stacked_input)

cat("Stacked Model Prediction:", stacked_prediction, "\n")
