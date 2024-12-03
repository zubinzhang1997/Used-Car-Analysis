library(caret)
library(xgboost)
library(dplyr)

data <- read.csv("cleaned_data_for_random_forest.csv")

# Convert categorical columns to factors
categorical_columns <- c("manufacturer", "model") 
data[categorical_columns] <- lapply(data[categorical_columns], as.factor)

# One-hot encode using caret's dummyVars
dummies_model <- dummyVars(" ~ .", data = data, fullRank = FALSE)
data_transformed <- predict(dummies_model, newdata = data)
data_transformed <- as.data.frame(data_transformed)

# Ensure price column is included in transformed data
if ("price" %in% colnames(data)) {
  data_transformed$price <- data$price
}

# Train-Test Split (80-20)
set.seed(123)
train_index <- createDataPartition(data$price, p = 0.8, list = FALSE)
train_data <- data_transformed[train_index, ]
test_data <- data_transformed[-train_index, ]

# Prepare data for XGBoost and convert data frame to DMatrix format
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
  nrounds = 100,                   
  watchlist = list(train = train_matrix),
  verbose = 1
)

# Save the XGBoost model
saveRDS(xgb_model, "xgb_model.rds")

# Predict on test data
xgb_predictions <- predict(xgb_model, newdata = test_matrix)

# Model Evaluation
rmse_xgb <- sqrt(mean((xgb_predictions - test_data$price)^2))
mape_xgb <- mean(abs((xgb_predictions - test_data$price) / test_data$price)) 
r2_xgb <- 1 - sum((xgb_predictions - test_data$price)^2) / sum((mean(train_data$price) - test_data$price)^2)

cat("XGBoost Model - RMSE:", rmse_xgb, ", MAPE:", mape_xgb, ", R-squared:", r2_xgb, "\n")






#===============================================================================

# Model Stacking
# Combine predictions from Random Forest and XGBoost in a data frame
stacked_data <- data.frame(
  rf = rf_predictions,  
  xgb = xgb_predictions,
  actual = test_data$price
)

# Train a linear model on the predictions to create a meta-model
stacked_model <- lm(actual ~ rf + xgb, data = stacked_data)

# Save the Stacked model
saveRDS(stacked_model, "stacked_model.rds")

# Make final predictions using the stacked model
stacked_predictions <- predict(stacked_model, newdata = stacked_data)

# Evaluate the Stacked Model
rmse_stacked <- sqrt(mean((stacked_predictions - test_data$price)^2))
mape_stacked <- mean(abs((stacked_predictions - test_data$price) / test_data$price))
r2_stacked <- 1 - sum((stacked_predictions - test_data$price)^2) / sum((mean(train_data$price) - test_data$price)^2)
cat("Stacked Model - RMSE:", rmse_stacked, ", MAPE:", mape_stacked, ", R-squared:", r2_stacked, "\n")

# Save the DummyVars model
saveRDS(dummies_model, "dummies_model.rds")



