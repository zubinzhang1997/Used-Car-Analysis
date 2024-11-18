library(dplyr)
library(ggplot2)
library(caret)
library(Metrics)
library(car)

vehicles <- read.csv("Model Cleaned.csv")

# Step 1: IQR Filtering for Outliers
Q1 <- quantile(vehicles$price, 0.25)
Q3 <- quantile(vehicles$price, 0.75)
IQR_value <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter out the outliers using IQR
vehicles_filtered <- vehicles %>%
  filter(price >= lower_bound & price <= upper_bound)

cat("Number of rows before IQR filtering:", nrow(vehicles), "\n")
cat("Number of rows after IQR filtering:", nrow(vehicles_filtered), "\n")

# Step 2: Cook's Distance for Influential Points
initial_model <- lm(price ~ ., data = vehicles_filtered)
cooks_distance <- cooks.distance(initial_model)
threshold <- 4 / nrow(vehicles_filtered)
influential_points <- which(cooks_distance > threshold)
vehicles_no_influential <- vehicles_filtered[-influential_points, ]

cat("Number of rows after removing influential points:", nrow(vehicles_no_influential), "\n")

# Step 3: Train-Test Split
set.seed(123)
train_index <- createDataPartition(vehicles_no_influential$price, p = 0.8, list = FALSE)
train_data <- vehicles_no_influential[train_index, ]
test_data <- vehicles_no_influential[-train_index, ]

# Step 4: High-Correlation Features Model
correlation_matrix <- cor(train_data %>% select(price, year, odometer))
print(correlation_matrix)

model1 <- lm(price ~ year + odometer, data = train_data)
predictions1 <- predict(model1, newdata = test_data)
rmse1 <- rmse(test_data$price, predictions1)
mape1 <- mape(test_data$price, predictions1)
r2_1 <- 1 - sum((predictions1 - test_data$price)^2) / sum((mean(train_data$price) - test_data$price)^2)
cat("Model 1 (High-Correlation Features) - RMSE:", rmse1, ", MAPE:", mape1, ", R-squared:", r2_1, "\n")

# Step 5: All Features Model
model2 <- lm(price ~ ., data = train_data)
predictions2 <- predict(model2, newdata = test_data)
rmse2 <- rmse(test_data$price, predictions2)
mape2 <- mape(test_data$price, predictions2)
r2_2 <- 1 - sum((predictions2 - test_data$price)^2) / sum((mean(train_data$price) - test_data$price)^2)
cat("Model 2 (All Features) - RMSE:", rmse2, ", MAPE:", mape2, ", R-squared:", r2_2, "\n")

# Step 6: Backward Selection Model
control <- trainControl(method = "cv", number = 10)
step_model <- train(price ~ ., data = train_data, method = "leapBackward", trControl = control)
selected_features <- predictors(step_model)
cat("Selected features (Backward Selection):", selected_features, "\n")

model3 <- lm(as.formula(paste("price ~", paste(selected_features, collapse = "+"))), data = train_data)
predictions3 <- predict(model3, newdata = test_data)
rmse3 <- rmse(test_data$price, predictions3)
mape3 <- mape(test_data$price, predictions3)
r2_3 <- 1 - sum((predictions3 - test_data$price)^2) / sum((mean(train_data$price) - test_data$price)^2)
cat("Model 3 (Backward Selection) - RMSE:", rmse3, ", MAPE:", mape3, ", R-squared:", r2_3, "\n")

# Step 7: Forward Selection Model
step_model_forward <- train(price ~ ., data = train_data, method = "leapForward", trControl = control)
selected_features_forward <- predictors(step_model_forward)
cat("Selected features (Forward Selection):", selected_features_forward, "\n")

model4 <- lm(as.formula(paste("price ~", paste(selected_features_forward, collapse = "+"))), data = train_data)
predictions4 <- predict(model4, newdata = test_data)
rmse4 <- rmse(test_data$price, predictions4)
mape4 <- mape(test_data$price, predictions4)
r2_4 <- 1 - sum((predictions4 - test_data$price)^2) / sum((mean(train_data$price) - test_data$price)^2)
cat("Model 4 (Forward Selection) - RMSE:", rmse4, ", MAPE:", mape4, ", R-squared:", r2_4, "\n")

# Step 8: Visualization of the Best Model
best_predictions <- predictions3
ggplot(data = data.frame(actual = test_data$price, predicted = best_predictions), aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Prices", x = "Actual Prices", y = "Predicted Prices") +
  theme_minimal()

# Step 9: Residual Diagnostics
model_refit <- lm(price ~ ., data = train_data)
par(mfrow = c(2, 2))
plot(model_refit)
residuals <- residuals(model_refit)
ggplot(data = data.frame(actual = train_data$price, residuals = residuals), aes(x = actual, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Actual Prices", x = "Actual Prices", y = "Residuals") +
  theme_minimal()

# Step 10: Cross-Validated Model
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(price ~ ., data = train_data, method = "lm", trControl = train_control)
cv_predictions <- predict(cv_model, newdata = test_data)
rmse_cv <- rmse(test_data$price, cv_predictions)
mape_cv <- mape(test_data$price, cv_predictions)
r2_cv <- 1 - sum((cv_predictions - test_data$price)^2) / sum((mean(train_data$price) - test_data$price)^2)
cat("Cross-Validated Model - RMSE:", rmse_cv, ", MAPE:", mape_cv, ", R-squared:", r2_cv, "\n")



write.csv(vehicles_no_influential, "cleaned_data_for_random_forest.csv", row.names = FALSE)
