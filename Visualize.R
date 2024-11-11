library(ggplot2)
library(dplyr)
library(scales)  

vehicles <- read.csv("cleaned_vehicles.csv")

# 1. Price distribution
ggplot(vehicles, aes(x = price)) + 
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Price Distribution of Vehicles", x = "Price", y = "Count")

# 2. Price vs. Year - Box Plot (2010 to 2020)
ggplot(subset(vehicles, year >= 2010 & year < 2021), aes(x = as.factor(year), y = price)) + 
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Price Distribution by Year (2010 to 2020)", x = "Year", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3. Average Price by Manufacturer - Bar plot
vehicles %>% 
  group_by(manufacturer) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(manufacturer, avg_price), y = avg_price)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "Average Price by Manufacturer", x = "Manufacturer", y = "Average Price")


# 4. Price vs. Miles - Scatter plot with smooth line
ggplot(vehicles, aes(x = odometer, y = price)) + 
  geom_point(alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(labels = comma) + 
  labs(title = "Price vs. Miles", x = "Miles", y = "Price")

# 5. Price by Fuel Type - Box plot
ggplot(subset(vehicles, !is.na(fuel) & fuel != "Other"), aes(x = fuel, y = price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Price by Fuel Type", x = "Fuel Type", y = "Price")


# 6. Number of Vehicles by Fuel Type - Bar plot
ggplot(subset(vehicles, !is.na(fuel) & fuel != "Other"), aes(x = fuel)) +
  geom_bar(fill = "plum") +
  labs(title = "Number of Vehicles by Fuel Type", x = "Fuel Type", y = "Count")


# 7. Average Price by Drive Type - Bar plot
vehicles %>%
  filter(!is.na(drive) & drive != "Other") %>%
  group_by(drive) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = drive, y = avg_price)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  labs(title = "Average Price by Drive Type", x = "Drive Type", y = "Average Price")


# 8. Price by Region - Bar plot
vehicles %>%
  group_by(region) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(region, -avg_price), y = avg_price)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Average Price by Region", x = "Region", y = "Average Price") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 9. Distribution of Vehicle Age
ggplot(subset(vehicles, (2023 - year) <= 20), aes(x = 2023 - year)) + 
  geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Vehicle Age (0-20 Years)", x = "Vehicle Age (Years)", y = "Count")


# 10. Price vs. Paint Color - Box plot
ggplot(subset(vehicles, paint_color %in% c("Black", "White", "Blue", "Brown", "Grey", "Silver")), 
       aes(x = paint_color, y = price)) + 
  geom_boxplot(fill = "lavender", color = "black") +
  labs(title = "Price by Paint Colors", x = "Paint Color", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 11. Count of Vehicle Types by Fuel Type - Stacked bar plot
ggplot(subset(vehicles, !is.na(type) & type != "Other"), aes(x = type, fill = fuel)) +
  geom_bar(position = "stack") +
  labs(title = "Count of Vehicle Types by Fuel Type", x = "Vehicle Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 12. Count of vehicles by manufacturer - Bar plot
ggplot(vehicles, aes(x = reorder(manufacturer, -table(manufacturer)[manufacturer]))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Count of Vehicles by Manufacturer", x = "Manufacturer", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#=========================================================================================================






example_model_counts <- selected_features %>%
  filter(manufacturer == "Ford") %>%
  count(model, sort = TRUE)

print(example_model_counts)


