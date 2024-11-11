library(dplyr)

vehicles <- read.csv("vehicles.csv")

# Define function to capitalize the first letter of each word
capitalize_first <- function(x) {
  ifelse(is.na(x), NA, paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x)))))
}

columns_to_clean <- c("region", "manufacturer", "model", "condition", "cylinders", 
                      "fuel", "odometer", "title_status", "drive", "type", "paint_color")

# Remove rows where the price is 0
vehicles <- vehicles %>%
  filter(price != 0)

# Remove rows where the miles is 0
vehicles <- vehicles %>%
  filter(odometer != 0)

vehicles <- vehicles %>% 
  # Replace empty strings with NA
  mutate(across(all_of(columns_to_clean), ~ ifelse(. == "", NA, .))) %>%  
  # Capitalize the first letter
  mutate(across(all_of(columns_to_clean), capitalize_first)) %>%   
  # Capitalize all letters in state column
  mutate(state = toupper(state))                                           

# Filter for MA, NY, CA
three_states <- vehicles %>%
  filter(state %in% c("MA", "NY", "CA"))

# Remove rows with missing values in manufacturer or model column
three_states <- three_states %>%
  filter(!is.na(manufacturer) & !is.na(model))


# Ensure odometer is numeric by converting non-numeric values to NA
three_states <- three_states %>%
  mutate(odometer = as.numeric(odometer))

# Calculate the IQR for price and odometer after ensuring numeric values
Q1_price <- quantile(three_states$price, 0.25, na.rm = TRUE)
Q3_price <- quantile(three_states$price, 0.75, na.rm = TRUE)
IQR_price <- Q3_price - Q1_price

Q1_odometer <- quantile(three_states$odometer, 0.25, na.rm = TRUE)
Q3_odometer <- quantile(three_states$odometer, 0.75, na.rm = TRUE)
IQR_odometer <- Q3_odometer - Q1_odometer

# Define thresholds for identifying outliers in price and odometer
lower_bound_price <- Q1_price - 1.5 * IQR_price
upper_bound_price <- Q3_price + 1.5 * IQR_price

lower_bound_odometer <- Q1_odometer - 1.5 * IQR_odometer
upper_bound_odometer <- Q3_odometer + 1.5 * IQR_odometer

# Filter out rows with price and odometer outliers
three_states <- three_states %>%
  filter(price >= lower_bound_price & price <= upper_bound_price) %>%
  filter(odometer >= lower_bound_odometer & odometer <= upper_bound_odometer)

# Update Land Rover name
three_states$manufacturer <- ifelse(three_states$manufacturer %in% c("Rover", "Land rover"), 
                                    "Land Rover", 
                                    three_states$manufacturer)

# Add an ID column
three_states <- three_states %>%
  mutate(id = row_number()) %>%
  select(id, everything())

write.csv(three_states, "cleaned_vehicles.csv", row.names = FALSE)

