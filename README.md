### README for Car Price Prediction Project

Welcome to the Car Price Prediction Project! This guide explains how to set up, run, and interact with the project. The project predicts used car prices using machine learning models and provides an interactive Shiny app for user-friendly predictions.

This project uses a cleaned dataset of used car listings to build predictive models, including Linear Regression, Random Forest, XGBoost, and a Stacked Model combining Random Forest and XGBoost. The final step includes a Shiny app where users can input car details and get a predicted price.

Before running the project, ensure you have R and RStudio installed. You also need the following R packages: `dplyr`, `ggplot2`, `caret`, `randomForest`, `xgboost`, and `shiny`. Install any missing packages by running `install.packages(c("dplyr", "ggplot2", "caret", "randomForest", "xgboost", "shiny"))`. The key files in this project include `Data Cleaning.R` for cleaning the raw dataset, `Visualize.R` for data exploration, `Model Cleaning.R` for preparing the dataset, `Linear Regression.R` for linear regression models, `Random Forests.R` for Random Forest training, `XGBoost.R` for XGBoost and Stacked Models, `input.R` for user-defined input predictions, and `app.R` for launching the Shiny app.

Since the original dataset is too large to upload to GitHub, you will start with the pre-cleaned dataset (`cleaned_vehicles.csv`). Download and save it to your working directory. Open the `Visualize.R` file and run it to generate data visualizations such as price distribution and price vs. odometer. These visualizations help in understanding the dataset.

For model training, run `Model Cleaning.R` to filter and prepare the dataset, `Linear Regression.R` to evaluate regression models, `Random Forests.R` to train the Random Forest model, and `XGBoost.R` to train the XGBoost and Stacked Models. Run the files in order, as each script depends on the previous one. To test custom predictions, open `input.R` and replace placeholders for year, manufacturer, odometer, and model with your values. For example, set `year <- 2018`, `manufacturer <- "Ford"`, `odometer <- 30000`, and `model <- "F-150"`. Run the file, and the predicted price will appear in the console.

After testing predictions, run the Shiny app by opening `app.R` and executing the script. The app interface will open in a browser, allowing you to input car details like manufacturer, model, year, and odometer value. Use the dropdown menus and input fields to specify your details and click the "Predict Price" button to get a real-time price prediction. Make sure to run `input.R` before launching the app to ensure all models and variables are correctly defined.

Important notes include starting with the cleaned dataset (`cleaned_vehicles.csv`) as the raw dataset is too large to provide. If you wish to clean the raw dataset yourself, save it as `vehicles.csv` and run `Data Cleaning.R`. Always run `input.R` before using the Shiny app, as it ensures all models and variables are ready. You can test predictions for any car details using `input.R` before launching the app.

For troubleshooting, ensure all scripts are in the same directory, and the cleaned dataset is downloaded. If the Shiny app throws errors, confirm that `input.R` was run beforehand and that input values are in the correct format (e.g., numeric year, valid model for the selected manufacturer).

If you have any questions or issues, feel free to reach out at zhangzubin1997@gmail.com. Happy coding!
