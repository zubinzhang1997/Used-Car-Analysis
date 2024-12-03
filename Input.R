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

# Align input data columns to match training data
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

