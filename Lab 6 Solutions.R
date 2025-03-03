install.packages("lubridate")
# Load required libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr) 
library(readxl)
library(lubridate)



# Read the Excel file
superstore_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.xlsx")

# Print the column names
colnames(superstore_data)

# Question 2

# Read the Excel file
superstore_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.xlsx")

# Convert 'Order Date' to Date format and extract the year
superstore_data <- superstore_data %>%
  mutate(`Order Date` = mdy(`Order Date`),  # Change 'mdy()' if needed
         year = year(`Order Date`))

# Count transactions in 2017
transactions_2017 <- superstore_data %>%
  filter(year == 2017) %>%
  nrow()

# Print the result
print(paste("Number of transactions in 2017:", transactions_2017))



# Question 3

# Read the Excel file
superstore_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.xlsx")

# Convert 'Order Date' to Date format and extract year & month
superstore_data <- superstore_data %>%
  mutate(`Order Date` = mdy(`Order Date`),  # Change 'mdy()' if needed
         year = year(`Order Date`),
         month = month(`Order Date`, label = TRUE, abbr = FALSE))  # Extract full month name

# Count transactions in December
transactions_december <- superstore_data %>%
  filter(month == "December") %>%
  nrow()

# Print the result
print(paste("Number of transactions in December:", transactions_december))



# Question 4

# Read the Excel file
superstore_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.xlsx")

# Convert 'Order Date' to Date format and extract year & month
superstore_data <- superstore_data %>%
  mutate(`Order Date` = mdy(`Order Date`),  # Ensure proper date conversion
         year = year(`Order Date`),
         month = month(`Order Date`),  # Extract month as a number
         year_month = paste(year, month, sep = "/"))  # Combine year and month

# Count transactions in December 2017 (year_month = "2017/12")
transactions_dec_2017 <- superstore_data %>%
  filter(year_month == "2017/12") %>%
  nrow()

# Print the result
print(paste("Number of transactions in December 2017:", transactions_dec_2017))



# Question 5

# Read the Excel file
superstore_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.xlsx")

# Count rows where unit price ends in '9'
rows_unit_price_ending_in_9 <- superstore_data %>%
  filter(grepl("9$", `Unit Price`)) %>%  # Use regex to check if 'Unit Price' ends in '9'
  nrow()

# Print the result
print(paste("Number of rows with unit price ending in '9':", rows_unit_price_ending_in_9))



# Question 6

# Read the Excel file
superstore_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.xlsx")

# Count rows where item name includes 'Kensington'
rows_with_kensington <- superstore_data %>%
  filter(grepl("Kensington", Item, ignore.case = TRUE)) %>%  # Use the correct column name
  nrow()

# Print the result
print(paste("Number of rows with item name including 'Kensington':", rows_with_kensington))



# Question 7

# Load required libraries
library(readxl)
library(dplyr)

# Read the Excel file
superstore_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.xlsx")

# Create 'last.name' column by extracting the last name
superstore_data <- superstore_data %>%
  mutate(last.name = sub(".*\\s", "", `Customer Name`))  # Extracts the last name

# Count rows where last name starts with 'R'
rows_last_name_starting_with_R <- superstore_data %>%
  filter(grepl("^R", last.name, ignore.case = TRUE)) %>%  # Check if last name starts with 'R'
  nrow()

# Print the result
print(paste("Number of rows with last name starting with 'R':", rows_last_name_starting_with_R))




# Question 8

# Read the Excel file
superstore_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.xlsx")

# Convert item descriptions to lowercase
superstore_data <- superstore_data %>%
  mutate(Item = tolower(Item))  # Convert 'Item' column to lowercase

# Count rows where item description includes 'black' or 'blue'
rows_with_black_or_blue <- superstore_data %>%
  filter(grepl("black|blue", Item, ignore.case = TRUE)) %>%  # Check for 'black' or 'blue'
  nrow()

# Print the result
print(paste("Number of rows with item description including 'black' or 'blue':", rows_with_black_or_blue))




# Question 9

# Read the Excel file
superstore_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.xlsx")

# Create 'identifier1' in the format 'item-month-day-year'
superstore_data <- superstore_data %>%
  mutate(
    Item = tolower(Item),  # Convert item names to lower case
    `Order Date` = as.Date(`Order Date`, format = "%m/%d/%Y"),  # Ensure proper date format
    Order_Date = format(`Order Date`, "%m-%d-%Y"),  # Format order date as 'month-day-year'
    identifier1 = paste(Item, Order_Date, sep = "-")  # Create identifier1
  )

# Count the number of unique identifiers
unique_identifiers_count <- superstore_data %>%
  distinct(identifier1) %>%
  nrow()  # Count unique rows

# Print the result
print(paste("Number of unique identifiers created:", unique_identifiers_count))




# Question 10

# Read the Excel file
superstore_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.xlsx")

# Create 'identifier2' in the format 'item-month-day-year-city,state'
superstore_data <- superstore_data %>%
  mutate(
    Item = tolower(Item),  # Convert item names to lower case
    City = tolower(City),  # Convert city names to lower case
    State = tolower(State),  # Convert state names to lower case
    `Order Date` = as.Date(`Order Date`, format = "%m/%d/%Y"),  # Ensure proper date format
    Order_Date = format(`Order Date`, "%m-%d-%Y"),  # Format order date as 'month-day-year'
    identifier1 = paste(Item, Order_Date, sep = "-"),  # Create identifier1
    identifier2 = paste(identifier1, paste(City, State, sep = ","), sep = "-")  # Create identifier2
  )

# Count the number of unique identifiers
unique_identifiers_count <- superstore_data %>%
  distinct(identifier2) %>%
  nrow()  # Count unique rows

# Print the result
print(paste("Number of unique identifiers created:", unique_identifiers_count))






# Credit_card Questions

install.packages("randomForest")
install.packages("pROC")
install.packages("caret")
install.packages("glmnet")


# Load required libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr) 
library(readxl)
library(lubridate)

# Load required libraries
library(readr)
library(randomForest)
library(pROC)
library(ggplot2)

library(readxl)       # For reading Excel files
library(caret)       # For model training
library(boot)        # For bootstrapping
library(dplyr)       # For data manipulation



# Check the column names in the dataset
colnames(credit_card_data)

# Question 11

# Load required libraries
library(readxl)
library(randomForest)
library(pROC)

# Read the Excel file
credit_card_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/credit_card.xlsx")

# Set seed for reproducibility
set.seed(100)

# Create a binary variable 'card' from the target variable
credit_card_data$card <- as.factor(ifelse(credit_card_data$`default payment next month` == 1, 1, 0))

# Split the data into training (70%) and testing (30%) sets
set.seed(100)
train_indices <- sample(1:nrow(credit_card_data), size = 0.7 * nrow(credit_card_data))
train_data <- credit_card_data[train_indices, ]
test_data <- credit_card_data[-train_indices, ]

# Initialize vectors for trees and AUC values
x = seq(20, 200, by = 20)
auc_values = vector(mode = 'numeric', length = length(x))

for (i in seq_along(x)) {
  # Fit the random forest model
  rf_model <- randomForest(card ~ . - ID - `default payment next month`, 
                           data = train_data, 
                           ntree = x[i], 
                           maxnodes = 10)
  
  # Predict on test data
  rf_probs <- predict(rf_model, test_data, type = "prob")[, 2]
  
  # Calculate AUC
  rf_roc <- roc(test_data$card, rf_probs)
  auc_values[i] <- rf_roc$auc
}

# Create a data frame for plotting
data = data.frame(trees = x, auc_values = auc_values)

# Load ggplot2 for plotting
library(ggplot2)

# Create plot showing how AUC varies with the number of trees
ggplot(data, aes(x = trees, y = auc_values)) +
  geom_line() +
  geom_point() +
  labs(title = "AUC vs. Number of Trees", 
       x = "Number of Trees", 
       y = "AUC") +
  theme_minimal()

# Save the plot as an image
ggsave("auc_vs_trees.png")




# Qeustion 12

max_auc_index <- which.max(auc_values)
best_trees <- x[max_auc_index]
best_auc <- auc_values[max_auc_index]

best_trees
best_auc



# Question 13

# Load necessary libraries
library(readxl)       # For reading Excel files
library(caret)       # For model training
library(boot)        # For bootstrapping
library(dplyr)       # For data manipulation

# Read the data from the Excel file
credit_card_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/credit_card.xlsx")

# Convert target variable to a factor
credit_card_data$`default payment next month` <- as.factor(credit_card_data$`default payment next month`)

# Split the data into training and testing sets
set.seed(100)
train_index <- createDataPartition(credit_card_data$`default payment next month`, p = 0.7, list = FALSE)
train_data <- credit_card_data[train_index, ]
test_data <- credit_card_data[-train_index, ]

# Set up training control for cross-validation
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

# Train a boosted generalized linear model
model <- train(`default payment next month` ~ ., data = train_data, method = "glmnet", trControl = fitControl)

# Make predictions on the test data
predictions <- predict(model, test_data)

# Calculate F1-score
confusion_matrix <- confusionMatrix(predictions, test_data$`default payment next month`)
f1_score <- confusion_matrix$byClass["F1"]
f1_score_rounded <- round(f1_score, 3)

# Output the F1-score
f1_score_rounded




# Question 14

# Read the data from the Excel file
credit_card_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/credit_card.xlsx")

# Convert target variable to a factor
credit_card_data$`default payment next month` <- as.factor(credit_card_data$`default payment next month`)

# Set seed for reproducibility
set.seed(100)

# Initialize vectors for training proportions and AUC values
x = seq(0.1, 0.9, by = 0.1)
auc_values = vector(mode = 'list')

# Loop through different training proportions
for (i in x) {
  # Split the data into training and testing sets
  train_index <- createDataPartition(credit_card_data$`default payment next month`, p = i, list = FALSE)
  train_data <- credit_card_data[train_index, ]
  test_data <- credit_card_data[-train_index, ]
  
  # Set up training control for cross-validation
  fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
  
  # Train a boosted generalized linear model
  model <- train(`default payment next month` ~ ., data = train_data, method = "glmnet", trControl = fitControl)
  
  # Make predictions on the test data
  predictions <- predict(model, test_data, type = "prob")
  
  # Calculate AUC
  boosted_glm_roc <- roc(test_data$`default payment next month`, predictions[, 2])
  auc_values = unlist(c(auc_values, boosted_glm_roc$auc)) # Extract AUC value
}

# Create a data frame for plotting
data = data.frame(proportion = x, auc_values = auc_values)

# Plot AUC vs. proportion of training data
library(ggplot2)

ggplot(data, aes(x = proportion, y = auc_values)) +
  geom_line() +
  geom_point() +
  labs(title = "AUC vs. Proportion of Training Data",
       x = "Proportion of Training Data",
       y = "AUC") +
  theme_minimal()




# Question 15

# Find the index of the lowest AUC value
min_auc_index <- which.min(auc_values)

# Get the corresponding proportion of training data
lowest_auc_proportion <- x[min_auc_index]

# Output the result
lowest_auc_proportion




# Question 16

library(readxl)
library(e1071)
library(caret)
library(pROC)

# Load data
credit_card_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/credit_card.xlsx")

# Convert target variable to a factor
credit_card_data$`default payment next month` <- as.factor(credit_card_data$`default payment next month`)

# Set seed
set.seed(100)

# Define a function to train SVM and calculate AUC
calculate_auc <- function(data, formula) {
  # Split the data into training and testing sets
  train_index <- createDataPartition(data$`default payment next month`, p = 0.7, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Train the SVM model
  svm_model <- svm(formula, data = train_data, kernel = "radial", probability = TRUE)
  
  # Make predictions on the test data
  predictions <- predict(svm_model, test_data, probability = TRUE)
  
  # Get the predicted probabilities for the positive class
  prob_predictions <- attr(predictions, "probabilities")[, 2]
  
  # Calculate the AUC
  roc_obj <- roc(test_data$`default payment next month`, prob_predictions)
  return(roc_obj$auc)
}

# Define predictor combinations
predictor_combinations <- list(
  "age + LIMIT_BAL" = `default payment next month` ~ AGE + LIMIT_BAL,
  "age + LIMIT_BAL + PAY_0" = `default payment next month` ~ AGE + LIMIT_BAL + PAY_0,
  "age + LIMIT_BAL + BILL_AMT1 + PAY_0" = `default payment next month` ~ AGE + LIMIT_BAL + BILL_AMT1 + PAY_0
)

# Calculate AUC for each combination
auc_results <- sapply(predictor_combinations, calculate_auc, data = credit_card_data)

# Find the combination with the highest AUC
best_combination <- names(which.max(auc_results))
best_auc <- max(auc_results)

# Output the results
best_combination
best_auc

# Question 16 Updated


# Load necessary libraries
library(e1071)  # For SVM
library(pROC)   # For AUC calculation
library(readr)  # For reading CSV files

# Read the data
credit_card_data <- read_excel("C:/Users/hp/Desktop/MS DAB/R Programming/credit_card.xlsx")


# Convert target variable to factor
credit_card_data$`default payment next month` <- as.factor(credit_card_data$`default payment next month`)

# Set seed for reproducibility
set.seed(100)

# Split the data into training and testing sets
train_index <- createDataPartition(credit_card_data$`default payment next month`, p = 0.7, list = FALSE)
train_data <- credit_card_data[train_index, ]
test_data <- credit_card_data[-train_index, ]

# Define predictor combinations
predictor_combinations <- list(
  a = c("AGE", "LIMIT_BAL"),
  b = c("AGE", "LIMIT_BAL", "PAY_0"),
  c = c("AGE", "LIMIT_BAL", "PAY_0", "BILL_AMT1")
)

# Initialize a list to store AUC values
auc_values <- c()

# Loop through each combination of predictors
for (name in names(predictor_combinations)) {
  # Create the formula
  formula <- as.formula(paste("`default payment next month` ~", paste(predictor_combinations[[name]], collapse = " + ")))
  
  # Train the SVM model
  model <- svm(formula, data = train_data, kernel = "radial", probability = TRUE)
  
  # Make predictions on the test data
  predictions <- predict(model, test_data, probability = TRUE)
  prob_predictions <- attr(predictions, "probabilities")[, 2]  # Get probabilities for the positive class
  
  # Calculate AUC
  roc_curve <- roc(test_data$`default payment next month`, prob_predictions)
  auc_values[name] <- auc(roc_curve)
}

# Print the AUC values
print(auc_values)

# Determine the best combination
best_combination <- names(which.max(auc_values))
best_auc <- max(auc_values)

# Output the results
cat("Best Combination:", best_combination, "\n")
cat("Best AUC:", best_auc, "\n")









# Corrections Updated

# Question 2

library(dplyr)
library(lubridate)

# Convert 'Order Date' to Date format if it is not already
superstore_data$`Order Date` <- as.Date(superstore_data$`Order Date`, format="%m/%d/%Y")

# Create a new column 'year' by extracting the year from 'Order Date'
superstore_data <- superstore_data %>%
  mutate(year = year(`Order Date`))

# Count the number of transactions made in 2017
transactions_2017 <- superstore_data %>%
  filter(year == 2017) %>%
  nrow()

# Print the number of transactions
transactions_2017



# Question 3

library(dplyr)
library(lubridate)

# Convert 'Order Date' to Date format (adjust the format if necessary)
superstore_data$`Order Date` <- as.Date(superstore_data$`Order Date`, format="%m/%d/%Y")

# Check for unique years in the 'Order Date'
unique_years <- unique(year(superstore_data$`Order Date`))
print(unique_years)  # This will show us all the years present in the data

# Create a new column 'year' by extracting the year from 'Order Date'
superstore_data <- superstore_data %>%
  mutate(year = year(`Order Date`))

# Count the number of transactions made in 2017
transactions_2017 <- superstore_data %>%
  filter(year == 2017) %>%
  nrow()

# Print the number of transactions in 2017
print(paste("Transactions in 2017:", transactions_2017))

# Create a new column 'month' by extracting the month from 'Order Date'
superstore_data <- superstore_data %>%
  mutate(month = month(`Order Date`))

# Count the number of transactions made in December (month = 12)
transactions_december <- superstore_data %>%
  filter(month == 12) %>%
  nrow()

# Print the number of transactions in December
print(paste("Transactions in December:", transactions_december))

# Show some example transactions from December 2017
december_transactions <- superstore_data %>%
  filter(year == 2017 & month == 12)

print(head(december_transactions))  # Display the first few transactions in December 2017




# Question 4

library(dplyr)

# Assuming you have already created the 'year' and 'month' columns
# Combine 'year' and 'month' into 'year_month'
superstore_data <- superstore_data %>%
  mutate(year_month = paste(year, month, sep = "/"))

# Count the number of transactions made in December 2017
transactions_december_2017 <- superstore_data %>%
  filter(year == 2017 & month == 12) %>%
  nrow()

# Print the number of transactions in December 2017
print(paste("Transactions in December 2017:", transactions_december_2017))



library(dplyr)
library(lubridate)

# Assuming you have already loaded the Superstore data into a data frame called superstore_data

# Convert 'Order Date' to Date format if not already done
superstore_data$`Order Date` <- as.Date(superstore_data$`Order Date`, format="%m/%d/%Y")

# Create the 'identifier1' column
superstore_data <- superstore_data %>%
  mutate(
    item = tolower(`Item`),  # Convert item name to lowercase
    order_date_formatted = gsub("/", "-", format(`Order Date`, "%m-%d-%Y")),  # Format order date
    identifier1 = paste(item, order_date_formatted, sep = "-")  # Create identifier1
  )

# Count unique identifiers
unique_identifiers_count <- n_distinct(superstore_data$identifier1)

# Print the number of unique identifiers
print(paste("Unique identifiers created:", unique_identifiers_count))
