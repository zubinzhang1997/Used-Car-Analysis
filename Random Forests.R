library(randomForest)
library(caret)

data <- read.csv("cleaned_data_for_random_forest.csv")

# Train-Test Split (80-20)
set.seed(123)
train_index <- createDataPartition(data$price, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train a Random Forest Model
set.seed(123)
rf_model <- randomForest(price ~ ., data = train_data, importance = TRUE, ntree = 500)

# Save the Random Forest model
saveRDS(rf_model, "rf_model.rds")

print(rf_model)

# Predict on Test Data
rf_predictions <- predict(rf_model, newdata = test_data)

# Model Evaluation
rmse_rf <- sqrt(mean((rf_predictions - test_data$price)^2))  
mape_rf <- mean(abs((rf_predictions - test_data$price) / test_data$price))  
r2_rf <- 1 - sum((rf_predictions - test_data$price)^2) / sum((mean(train_data$price) - test_data$price)^2)

cat("Random Forest Model - RMSE:", rmse_rf, ", MAPE:", mape_rf, ", R-squared:", r2_rf, "\n")

varImpPlot(rf_model)
