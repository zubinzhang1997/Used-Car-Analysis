library(dplyr)
library(stringr)

vehicles <- read.csv("cleaned_vehicles.csv")


# Select specified features
selected_features <- vehicles %>%
  select(price, year, manufacturer, odometer, model)

# Check the number of missing values in each column
colSums(is.na(selected_features))

length(unique(selected_features$model))


# Summarize the number of unique models for each manufacturer
summary_table <- selected_features %>% 
  group_by(manufacturer) %>% 
  summarise(unique_models = n_distinct(model)) %>% 
  arrange(desc(unique_models))

# Display the summary table
print(summary_table)



# Count the number of occurrences of each model for Ford
ford_model_counts <- selected_features %>% 
  filter(manufacturer == "Ford") %>% 
  count(model, sort = TRUE)

print(ford_model_counts)

# Define the target models to keep for Ford
ford_target_models <- c("F-150", "Escape", "Explorer", "Mustang",
                        "Fusion", "Focus", "Edge", "F-250",
                        "Ranger", "F-350", "Taurus", "Expedition")

# Function to standardize Ford models based on keywords
standardize_ford_models <- function(df) {
  df %>%
    mutate(model = case_when(
      manufacturer == "Ford" & grepl("(?i)f[- ]?150", model, perl = TRUE) ~ "F-150",
      manufacturer == "Ford" & grepl("(?i)escape", model) ~ "Escape",
      manufacturer == "Ford" & grepl("(?i)explorer", model) ~ "Explorer",
      manufacturer == "Ford" & grepl("(?i)mustang", model) ~ "Mustang",
      manufacturer == "Ford" & grepl("(?i)fusion", model) ~ "Fusion",
      manufacturer == "Ford" & grepl("(?i)focus", model) ~ "Focus",
      manufacturer == "Ford" & grepl("(?i)edge", model) ~ "Edge",
      manufacturer == "Ford" & grepl("(?i)f[- ]?250", model, perl = TRUE) ~ "F-250",
      manufacturer == "Ford" & grepl("(?i)ranger", model) ~ "Ranger",
      manufacturer == "Ford" & grepl("(?i)f[- ]?350", model, perl = TRUE) ~ "F-350",
      manufacturer == "Ford" & grepl("(?i)taurus", model) ~ "Taurus",
      manufacturer == "Ford" & grepl("(?i)expedition", model) ~ "Expedition",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Ford models
selected_features <- standardize_ford_models(selected_features)

# Verify changes for Ford models
updated_model_counts <- selected_features %>% 
  filter(manufacturer == "Ford") %>% 
  count(model, sort = TRUE)

print(updated_model_counts)

# Filter the dataset to retain only specified Ford models
selected_features <- selected_features %>%
  filter(!(manufacturer == "Ford" & !(model %in% ford_target_models)))

# Count the number of occurrences of each model for Ford
ford_model_counts <- selected_features %>% 
  filter(manufacturer == "Ford") %>% 
  count(model, sort = TRUE)

print(ford_model_counts)





# Define the target models to keep for Chevrolet
chevrolet_target_models <- c("Silverado", "Tahoe", "Corvette", "Malibu", 
                             "Impala", "Equinox", "Camaro", "Cruze", 
                             "Suburban", "Colorado", "Traverse")

# Function to standardize Chevrolet models based on keywords
standardize_chevrolet_models <- function(df) {
  df %>%
    mutate(model = case_when(
      manufacturer == "Chevrolet" & grepl("(?i)silverado", model) ~ "Silverado",
      manufacturer == "Chevrolet" & grepl("(?i)tahoe", model) ~ "Tahoe",
      manufacturer == "Chevrolet" & grepl("(?i)corvette", model) ~ "Corvette",
      manufacturer == "Chevrolet" & grepl("(?i)malibu", model) ~ "Malibu",
      manufacturer == "Chevrolet" & grepl("(?i)impala", model) ~ "Impala",
      manufacturer == "Chevrolet" & grepl("(?i)equinox", model) ~ "Equinox",
      manufacturer == "Chevrolet" & grepl("(?i)camaro", model) ~ "Camaro",
      manufacturer == "Chevrolet" & grepl("(?i)cruze", model) ~ "Cruze",
      manufacturer == "Chevrolet" & grepl("(?i)suburban", model) ~ "Suburban",
      manufacturer == "Chevrolet" & grepl("(?i)colorado", model) ~ "Colorado",
      manufacturer == "Chevrolet" & grepl("(?i)traverse", model) ~ "Traverse",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Chevrolet models
selected_features <- standardize_chevrolet_models(selected_features)

# Verify changes for Chevrolet models
updated_chevrolet_model_counts <- selected_features %>%
  filter(manufacturer == "Chevrolet") %>%
  count(model, sort = TRUE)

print(updated_chevrolet_model_counts)

# Filter the dataset to retain only specified Chevrolet models
selected_features <- selected_features %>%
  filter(!(manufacturer == "Chevrolet" & !(model %in% chevrolet_target_models)))

# Count the number of occurrences of each model for Chevrolet
chevrolet_model_counts <- selected_features %>%
  filter(manufacturer == "Chevrolet") %>%
  count(model, sort = TRUE)

print(chevrolet_model_counts)










# Define the target models to keep for Toyota
toyota_target_models <- c("Camry", "Tacoma", "Prius", "Corolla", 
                          "Rav4", "Tundra", "Sienna", "4runner", 
                          "Highlander", "Avalon")

# Function to standardize Toyota models based on keywords
standardize_toyota_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Toyota" & grepl("(?i)camry", model) ~ "Camry",
      manufacturer == "Toyota" & grepl("(?i)tacoma", model) ~ "Tacoma",
      manufacturer == "Toyota" & grepl("(?i)prius", model) ~ "Prius",
      manufacturer == "Toyota" & grepl("(?i)corolla", model) ~ "Corolla",
      manufacturer == "Toyota" & grepl("(?i)rav[- ]?4", model, perl = TRUE) ~ "Rav4", # Matches "Rav4", "RAV 4", etc.
      manufacturer == "Toyota" & grepl("(?i)tundra", model) ~ "Tundra",
      manufacturer == "Toyota" & grepl("(?i)sienna", model) ~ "Sienna",
      manufacturer == "Toyota" & grepl("(?i)4runner", model) ~ "4runner",
      manufacturer == "Toyota" & grepl("(?i)highlander", model) ~ "Highlander",
      manufacturer == "Toyota" & grepl("(?i)avalon", model) ~ "Avalon",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Toyota models
selected_features <- standardize_toyota_models(selected_features)

# Verify changes for Toyota models
updated_toyota_model_counts <- selected_features %>% 
  filter(manufacturer == "Toyota") %>% 
  count(model, sort = TRUE)

print(updated_toyota_model_counts)

# Filter the dataset to retain only specified Toyota models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Toyota" & !(model %in% toyota_target_models)))

# Count the number of occurrences of each model for Toyota
toyota_model_counts <- selected_features %>% 
  filter(manufacturer == "Toyota") %>% 
  count(model, sort = TRUE)

print(toyota_model_counts)









# Define the target models to keep for Honda
honda_target_models <- c("Accord", "Civic", "Cr-v", "Odyssey", 
                         "Pilot", "Fit", "Element")

# Function to standardize Honda models based on keywords
standardize_honda_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Honda" & grepl("(?i)accord", model) ~ "Accord",
      manufacturer == "Honda" & grepl("(?i)civic", model) ~ "Civic",
      manufacturer == "Honda" & grepl("(?i)cr[- ]?v", model, perl = TRUE) ~ "Cr-v", # Matches "Cr-v", "CR V", etc.
      manufacturer == "Honda" & grepl("(?i)odyssey", model) ~ "Odyssey",
      manufacturer == "Honda" & grepl("(?i)pilot", model) ~ "Pilot",
      manufacturer == "Honda" & grepl("(?i)fit", model) ~ "Fit",
      manufacturer == "Honda" & grepl("(?i)element", model) ~ "Element",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Honda models
selected_features <- standardize_honda_models(selected_features)

# Verify changes for Honda models
updated_honda_model_counts <- selected_features %>% 
  filter(manufacturer == "Honda") %>% 
  count(model, sort = TRUE)

print(updated_honda_model_counts)

# Filter the dataset to retain only specified Honda models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Honda" & !(model %in% honda_target_models)))

# Count the number of occurrences of each model for Honda
honda_model_counts <- selected_features %>% 
  filter(manufacturer == "Honda") %>% 
  count(model, sort = TRUE)

print(honda_model_counts)









# Define the target models to keep for Nissan
nissan_target_models <- c("Altima", "Sentra", "Rogue", "Frontier", 
                          "Maxima", "Versa", "Pathfinder", "Murano")

# Function to standardize Nissan models based on keywords
standardize_nissan_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Nissan" & grepl("(?i)altima", model) ~ "Altima",
      manufacturer == "Nissan" & grepl("(?i)sentra", model) ~ "Sentra",
      manufacturer == "Nissan" & grepl("(?i)rogue", model) ~ "Rogue",
      manufacturer == "Nissan" & grepl("(?i)frontier", model) ~ "Frontier",
      manufacturer == "Nissan" & grepl("(?i)maxima", model) ~ "Maxima",
      manufacturer == "Nissan" & grepl("(?i)versa", model) ~ "Versa",
      manufacturer == "Nissan" & grepl("(?i)pathfinder", model) ~ "Pathfinder",
      manufacturer == "Nissan" & grepl("(?i)murano", model) ~ "Murano",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Nissan models
selected_features <- standardize_nissan_models(selected_features)

# Verify changes for Nissan models
updated_nissan_model_counts <- selected_features %>% 
  filter(manufacturer == "Nissan") %>% 
  count(model, sort = TRUE)

print(updated_nissan_model_counts)

# Filter the dataset to retain only specified Nissan models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Nissan" & !(model %in% nissan_target_models)))

# Count the number of occurrences of each model for Nissan
nissan_model_counts <- selected_features %>% 
  filter(manufacturer == "Nissan") %>% 
  count(model, sort = TRUE)

print(nissan_model_counts)









# Define the target models to keep for BMW
bmw_target_models <- c("3 Series", "5 Series", "X5", "X3", 
                       "X1", "7 Series", "4 Series", "X6")

# Function to standardize BMW models based on keywords
standardize_bmw_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Bmw" & grepl("(?i)3 series|328|335|325|330", model) ~ "3 Series",
      manufacturer == "Bmw" & grepl("(?i)5 series|528|535|530|540|550", model) ~ "5 Series",
      manufacturer == "Bmw" & grepl("(?i)x5", model) ~ "X5",
      manufacturer == "Bmw" & grepl("(?i)x3", model) ~ "X3",
      manufacturer == "Bmw" & grepl("(?i)x1", model) ~ "X1",
      manufacturer == "Bmw" & grepl("(?i)7 series|750", model) ~ "7 Series",
      manufacturer == "Bmw" & grepl("(?i)4 series|430|440", model) ~ "4 Series",
      manufacturer == "Bmw" & grepl("(?i)x6", model) ~ "X6",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize BMW models
selected_features <- standardize_bmw_models(selected_features)

# Verify changes for BMW models
updated_bmw_model_counts <- selected_features %>% 
  filter(manufacturer == "Bmw") %>% 
  count(model, sort = TRUE)

print(updated_bmw_model_counts)

# Filter the dataset to retain only specified BMW models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Bmw" & !(model %in% bmw_target_models)))

# Count the number of occurrences of each model for BMW
bmw_model_counts <- selected_features %>% 
  filter(manufacturer == "Bmw") %>% 
  count(model, sort = TRUE)

print(bmw_model_counts)









# Define the target models to keep for Jeep
jeep_target_models <- c("Wrangler", "Grand Cherokee", "Cherokee", 
                        "Liberty", "Patriot", "Compass")

# Function to standardize Jeep models based on keywords
standardize_jeep_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Jeep" & grepl("(?i)wrangler", model) ~ "Wrangler",
      manufacturer == "Jeep" & grepl("(?i)grand cherokee", model) ~ "Grand Cherokee",
      manufacturer == "Jeep" & grepl("(?i)cherokee", model) & 
        !grepl("(?i)grand", model) ~ "Cherokee", # Exclude Grand Cherokee
      manufacturer == "Jeep" & grepl("(?i)liberty", model) ~ "Liberty",
      manufacturer == "Jeep" & grepl("(?i)patriot", model) ~ "Patriot",
      manufacturer == "Jeep" & grepl("(?i)compass", model) ~ "Compass",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Jeep models
selected_features <- standardize_jeep_models(selected_features)

# Verify changes for Jeep models
updated_jeep_model_counts <- selected_features %>% 
  filter(manufacturer == "Jeep") %>% 
  count(model, sort = TRUE)

print(updated_jeep_model_counts)

# Filter the dataset to retain only specified Jeep models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Jeep" & !(model %in% jeep_target_models)))

# Count the number of occurrences of each model for Jeep
jeep_model_counts <- selected_features %>% 
  filter(manufacturer == "Jeep") %>% 
  count(model, sort = TRUE)

print(jeep_model_counts)










# Define the target models to keep for Mercedes-Benz
mercedes_target_models <- c("C-class", "E-class", "S-class", "Gl-class", 
                            "Sl-class", "M-class", "Cls-class")

# Function to standardize Mercedes-Benz models based on keywords
standardize_mercedes_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Mercedes-benz" & grepl("(?i)c[- ]?class|c300", model) ~ "C-class",
      manufacturer == "Mercedes-benz" & grepl("(?i)e[- ]?class|e350|e550|e320|e 350|e400", model) ~ "E-class",
      manufacturer == "Mercedes-benz" & grepl("(?i)s[- ]?class|s550|s 550", model) ~ "S-class",
      manufacturer == "Mercedes-benz" & grepl("(?i)sl[- ]?class|sl 550", model) ~ "Sl-class",
      manufacturer == "Mercedes-benz" & grepl("(?i)m[- ]?class|ml 350", model) ~ "M-class",
      manufacturer == "Mercedes-benz" & grepl("(?i)gl[- ]?class|gl450", model) ~ "Gl-class",
      manufacturer == "Mercedes-benz" & grepl("(?i)cls[- ]?class", model) ~ "Cls-class",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Mercedes-Benz models
selected_features <- standardize_mercedes_models(selected_features)

# Verify changes for Mercedes-Benz models
updated_mercedes_model_counts <- selected_features %>% 
  filter(manufacturer == "Mercedes-benz") %>% 
  count(model, sort = TRUE)

print(updated_mercedes_model_counts)

# Filter the dataset to retain only specified Mercedes-Benz models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Mercedes-benz" & !(model %in% mercedes_target_models)))

# Count the number of occurrences of each model for Mercedes-Benz
mercedes_model_counts <- selected_features %>% 
  filter(manufacturer == "Mercedes-benz") %>% 
  count(model, sort = TRUE)

print(mercedes_model_counts)












# Define the target models to keep for GMC
gmc_target_models <- c("Sierra 1500", "Sierra 2500", "Acadia", "Yukon", "Terrain")

# Function to standardize GMC models based on keywords, with spaces included
standardize_gmc_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Gmc" & grepl("(?i)sierra ?1500", model) ~ "Sierra 1500",
      manufacturer == "Gmc" & grepl("(?i)sierra ?2500", model) ~ "Sierra 2500",
      manufacturer == "Gmc" & grepl("(?i)acadia", model) ~ "Acadia",
      manufacturer == "Gmc" & grepl("(?i)yukon", model) ~ "Yukon",
      manufacturer == "Gmc" & grepl("(?i)terrain", model) ~ "Terrain",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize GMC models
selected_features <- standardize_gmc_models(selected_features)

# Verify changes for GMC models
updated_gmc_model_counts <- selected_features %>% 
  filter(manufacturer == "Gmc") %>% 
  count(model, sort = TRUE)

print(updated_gmc_model_counts)

# Filter the dataset to retain only specified GMC models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Gmc" & !(model %in% gmc_target_models)))

# Count the number of occurrences of each model for GMC
gmc_model_counts <- selected_features %>% 
  filter(manufacturer == "Gmc") %>% 
  count(model, sort = TRUE)

print(gmc_model_counts)








# Define the target models to keep for Hyundai
hyundai_target_models <- c("Sonata", "Elantra", "Santa Fe", "Tucson", "Accent", "Veloster")

# Function to standardize Hyundai models based on keywords
standardize_hyundai_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Hyundai" & grepl("(?i)sonata", model) ~ "Sonata",
      manufacturer == "Hyundai" & grepl("(?i)elantra", model) ~ "Elantra",
      manufacturer == "Hyundai" & grepl("(?i)santa fe", model) ~ "Santa Fe",
      manufacturer == "Hyundai" & grepl("(?i)tucson", model) ~ "Tucson",
      manufacturer == "Hyundai" & grepl("(?i)accent", model) ~ "Accent",
      manufacturer == "Hyundai" & grepl("(?i)veloster", model) ~ "Veloster",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Hyundai models
selected_features <- standardize_hyundai_models(selected_features)

# Verify changes for Hyundai models
updated_hyundai_model_counts <- selected_features %>% 
  filter(manufacturer == "Hyundai") %>% 
  count(model, sort = TRUE)

print(updated_hyundai_model_counts)

# Filter the dataset to retain only specified Hyundai models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Hyundai" & !(model %in% hyundai_target_models)))

# Count the number of occurrences of each model for Hyundai
hyundai_model_counts <- selected_features %>% 
  filter(manufacturer == "Hyundai") %>% 
  count(model, sort = TRUE)

print(hyundai_model_counts)











# Define the target models to keep for Subaru
subaru_target_models <- c("Outback", "Forester", "Impreza", "Legacy", "WRX", "Crosstrek")

# Function to standardize Subaru models based on keywords
standardize_subaru_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Subaru" & grepl("(?i)outback", model) ~ "Outback",
      manufacturer == "Subaru" & grepl("(?i)forester", model) ~ "Forester",
      manufacturer == "Subaru" & grepl("(?i)impreza", model) ~ "Impreza",
      manufacturer == "Subaru" & grepl("(?i)legacy", model) ~ "Legacy",
      manufacturer == "Subaru" & grepl("(?i)wrx", model) ~ "WRX",
      manufacturer == "Subaru" & grepl("(?i)crosstrek", model) ~ "Crosstrek",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Subaru models
selected_features <- standardize_subaru_models(selected_features)

# Verify changes for Subaru models
updated_subaru_model_counts <- selected_features %>% 
  filter(manufacturer == "Subaru") %>% 
  count(model, sort = TRUE)

print(updated_subaru_model_counts)

# Filter the dataset to retain only specified Subaru models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Subaru" & !(model %in% subaru_target_models)))

# Count the number of occurrences of each model for Subaru
subaru_model_counts <- selected_features %>% 
  filter(manufacturer == "Subaru") %>% 
  count(model, sort = TRUE)

print(subaru_model_counts)

















# Define the target models to keep for Volkswagen
volkswagen_target_models <- c("Jetta", "Passat", "Tiguan", "Beetle", "Golf")

# Function to standardize Volkswagen models based on keywords
standardize_volkswagen_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Volkswagen" & grepl("(?i)jetta", model) ~ "Jetta",
      manufacturer == "Volkswagen" & grepl("(?i)passat", model) ~ "Passat",
      manufacturer == "Volkswagen" & grepl("(?i)tiguan", model) ~ "Tiguan",
      manufacturer == "Volkswagen" & grepl("(?i)beetle", model) ~ "Beetle",
      manufacturer == "Volkswagen" & grepl("(?i)golf", model) ~ "Golf",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Volkswagen models
selected_features <- standardize_volkswagen_models(selected_features)

# Verify changes for Volkswagen models
updated_volkswagen_model_counts <- selected_features %>% 
  filter(manufacturer == "Volkswagen") %>% 
  count(model, sort = TRUE)

print(updated_volkswagen_model_counts)

# Filter the dataset to retain only specified Volkswagen models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Volkswagen" & !(model %in% volkswagen_target_models)))

# Count the number of occurrences of each model for Volkswagen
volkswagen_model_counts <- selected_features %>% 
  filter(manufacturer == "Volkswagen") %>% 
  count(model, sort = TRUE)

print(volkswagen_model_counts)










# Define the target models to keep for Ram
ram_target_models <- c("1500", "2500", "3500")

# Function to standardize Ram models based on keywords
standardize_ram_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Ram" & grepl("(?i)1500", model) ~ "1500",
      manufacturer == "Ram" & grepl("(?i)2500", model) ~ "2500",
      manufacturer == "Ram" & grepl("(?i)3500", model) ~ "3500",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Ram models
selected_features <- standardize_ram_models(selected_features)

# Verify changes for Ram models
updated_ram_model_counts <- selected_features %>% 
  filter(manufacturer == "Ram") %>% 
  count(model, sort = TRUE)

print(updated_ram_model_counts)

# Filter the dataset to retain only specified Ram models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Ram" & !(model %in% ram_target_models)))

# Count the number of occurrences of each model for Ram
ram_model_counts <- selected_features %>% 
  filter(manufacturer == "Ram") %>% 
  count(model, sort = TRUE)

print(ram_model_counts)













# Define the target models to keep for Dodge
dodge_target_models <- c("Charger", "Grand Caravan", "Challenger", 
                         "Durango", "Dart", "Journey")

# Function to standardize Dodge models based on keywords
standardize_dodge_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Dodge" & grepl("(?i)charger", model) ~ "Charger",
      manufacturer == "Dodge" & grepl("(?i)grand\\s*caravan", model, perl = TRUE) ~ "Grand Caravan",
      manufacturer == "Dodge" & grepl("(?i)challenger", model) ~ "Challenger",
      manufacturer == "Dodge" & grepl("(?i)durango", model) ~ "Durango",
      manufacturer == "Dodge" & grepl("(?i)dart", model) ~ "Dart",
      manufacturer == "Dodge" & grepl("(?i)journey", model) ~ "Journey",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Dodge models
selected_features <- standardize_dodge_models(selected_features)

# Verify changes for Dodge models
updated_dodge_model_counts <- selected_features %>% 
  filter(manufacturer == "Dodge") %>% 
  count(model, sort = TRUE)

print(updated_dodge_model_counts)

# Filter the dataset to retain only specified Dodge models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Dodge" & !(model %in% dodge_target_models)))

# Count the number of occurrences of each model for Dodge
dodge_model_counts <- selected_features %>% 
  filter(manufacturer == "Dodge") %>% 
  count(model, sort = TRUE)

print(dodge_model_counts)


















# Define the target models to keep for Lexus
lexus_target_models <- c("Rx", "Es", "Gs", "Is", "Ls", "Nx", "Ct")

# Function to standardize Lexus models based on keywords
standardize_lexus_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Lexus" & grepl("(?i)rx", model) ~ "Rx",
      manufacturer == "Lexus" & grepl("(?i)es", model) ~ "Es",
      manufacturer == "Lexus" & grepl("(?i)gs", model) ~ "Gs",
      manufacturer == "Lexus" & grepl("(?i)is", model) ~ "Is",
      manufacturer == "Lexus" & grepl("(?i)ls", model) ~ "Ls",
      manufacturer == "Lexus" & grepl("(?i)nx", model) ~ "Nx",
      manufacturer == "Lexus" & grepl("(?i)ct", model) ~ "Ct",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Lexus models
selected_features <- standardize_lexus_models(selected_features)

# Verify changes for Lexus models
updated_lexus_model_counts <- selected_features %>% 
  filter(manufacturer == "Lexus") %>% 
  count(model, sort = TRUE)

print(updated_lexus_model_counts)

# Filter the dataset to retain only specified Lexus models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Lexus" & !(model %in% lexus_target_models)))

# Count the number of occurrences of each model for Lexus
lexus_model_counts <- selected_features %>% 
  filter(manufacturer == "Lexus") %>% 
  count(model, sort = TRUE)

print(lexus_model_counts)















# Define the target models to keep for Audi
audi_target_models <- c("A4", "Q5", "Q7", "S5", "A3", "A6", "Q3", "A7", "A8")

# Function to standardize Audi models based on keywords
standardize_audi_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Audi" & grepl("(?i)a4", model) ~ "A4",
      manufacturer == "Audi" & grepl("(?i)q5", model) ~ "Q5",
      manufacturer == "Audi" & grepl("(?i)q7", model) ~ "Q7",
      manufacturer == "Audi" & grepl("(?i)s5", model) ~ "S5",
      manufacturer == "Audi" & grepl("(?i)a3", model) ~ "A3",
      manufacturer == "Audi" & grepl("(?i)a6", model) ~ "A6",
      manufacturer == "Audi" & grepl("(?i)q3", model) ~ "Q3",
      manufacturer == "Audi" & grepl("(?i)a7", model) ~ "A7",
      manufacturer == "Audi" & grepl("(?i)a8", model) ~ "A8",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Audi models
selected_features <- standardize_audi_models(selected_features)

# Verify changes for Audi models
updated_audi_model_counts <- selected_features %>% 
  filter(manufacturer == "Audi") %>% 
  count(model, sort = TRUE)

print(updated_audi_model_counts)

# Filter the dataset to retain only specified Audi models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Audi" & !(model %in% audi_target_models)))

# Count the number of occurrences of each model for Audi
audi_model_counts <- selected_features %>% 
  filter(manufacturer == "Audi") %>% 
  count(model, sort = TRUE)

print(audi_model_counts)


















# Define the target models to keep for Kia
kia_target_models <- c("Optima", "Soul", "Sorento", "Forte", "Sportage")

# Function to standardize Kia models based on keywords
standardize_kia_models <- function(df) {
  df %>% 
    mutate(model = case_when(
      manufacturer == "Kia" & grepl("(?i)optima", model) ~ "Optima",
      manufacturer == "Kia" & grepl("(?i)soul", model) ~ "Soul",
      manufacturer == "Kia" & grepl("(?i)sorento", model) ~ "Sorento",
      manufacturer == "Kia" & grepl("(?i)forte", model) ~ "Forte",
      manufacturer == "Kia" & grepl("(?i)sportage", model) ~ "Sportage",
      TRUE ~ model # Keep the original model if it doesn't match any condition
    ))
}

# Apply the function to standardize Kia models
selected_features <- standardize_kia_models(selected_features)

# Verify changes for Kia models
updated_kia_model_counts <- selected_features %>% 
  filter(manufacturer == "Kia") %>% 
  count(model, sort = TRUE)

print(updated_kia_model_counts)

# Filter the dataset to retain only specified Kia models
selected_features <- selected_features %>% 
  filter(!(manufacturer == "Kia" & !(model %in% kia_target_models)))

# Count the number of occurrences of each model for Kia
kia_model_counts <- selected_features %>% 
  filter(manufacturer == "Kia") %>% 
  count(model, sort = TRUE)

print(kia_model_counts)

# List of manufacturers to keep
target_manufacturers <- c("Ford", "Chevrolet", "Toyota", "Honda", "Nissan", "Bmw", 
                          "Jeep", "Mercedes-benz", "Gmc", "Hyundai", "Subaru", 
                          "Volkswagen", "Ram", "Dodge", "Lexus", "Audi", "Kia")

# Filter the dataset to only include rows with the specified manufacturers
selected_features <- selected_features %>%
  filter(manufacturer %in% target_manufacturers)

# Order by brand (manufacturer) first
selected_features <- selected_features %>%
  arrange(manufacturer) %>%
  mutate(id = row_number()) %>%  # Add ID column after ordering
  select(id, everything())  # Ensure ID is the first column

# Save the filtered and ordered dataset as "Model Cleaned.csv"
write.csv(selected_features, "Model Cleaned.csv", row.names = FALSE)







