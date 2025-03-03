#Lab 4
install.packages("AER")
install.packages("ordinal")
install.packages("MASS") 
install.packages("truncreg") 
install.packages("lubridate")
# Load required libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr) 
library(readxl)
library(stringr)
library(AER)
library(ordinal)
library(MASS)
library(truncreg)
library(lubridate)


# Read the CSV file
superstore_data <- read_csv("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.csv")


# Question 2

# Ensure column names are consistent
colnames(superstore_data) <- tolower(gsub(" ", "_", colnames(superstore_data)))  # Standardize column names

# Convert Order Date to Date format
superstore_data <- superstore_data %>%
  mutate(order_date = as.Date(order_date, format = "%m/%d/%Y"))  # Adjust format if needed

# Extract the year from Order Date
superstore_data <- superstore_data %>%
  mutate(year = as.numeric(format(order_date, '%Y')))

# Filter for the year 2015
superstore_2015 <- superstore_data %>%
  filter(year == 2015)

# Summarize total sales by state
sales_by_state <- superstore_2015 %>%
  group_by(state) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
  arrange(state)  # Sort alphabetically by state

# Get the value at row index 30, column 2
value_at_30_2 <- sales_by_state[30, 2]

# Print result
value_at_30_2



# Question 3

# Ensure column names are consistent
colnames(superstore_data) <- tolower(gsub(" ", "_", colnames(superstore_data)))  # Standardize column names

# Convert Order Date to Date format
superstore_data <- superstore_data %>%
  mutate(order_date = as.Date(order_date, format = "%m/%d/%Y"))  # Adjust format if needed

# Extract the year from Order Date
superstore_data <- superstore_data %>%
  mutate(year = as.numeric(format(order_date, '%Y')))

# Filter for the year 2016
superstore_2016 <- superstore_data %>%
  filter(year == 2016)

# Summarize total sales by state
sales_by_state <- superstore_2016 %>%
  group_by(state) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
  arrange(state)  # Sort alphabetically by state

# Get the value at row index 35, column 2
value_at_35_2 <- sales_by_state[35, 2]

# Print result
value_at_35_2


# Question 4

# Standardize column names
colnames(superstore_data) <- tolower(gsub(" ", "_", colnames(superstore_data)))

# Convert Order Date to Date format
superstore_data <- superstore_data %>%
  mutate(order_date = as.Date(order_date, format = "%m/%d/%Y"))  # Adjust format if needed

# Extract the year from Order Date
superstore_data <- superstore_data %>%
  mutate(year = as.numeric(format(order_date, '%Y')))

# Filter & summarize sales for 2015
sales_2015 <- superstore_data %>%
  filter(year == 2015) %>%
  group_by(state) %>%
  summarise(total_sales_2015 = sum(sales, na.rm = TRUE))

# Filter & summarize sales for 2016
sales_2016 <- superstore_data %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  summarise(total_sales_2016 = sum(sales, na.rm = TRUE))

# Inner join on State
df5 <- inner_join(sales_2015, sales_2016, by = 'state')

# Reshape data to long format
df_final <- df5 %>%
  pivot_longer(cols = starts_with("total_sales"), 
               names_to = "year", 
               values_to = "total_sales") %>%
  mutate(year = ifelse(year == "total_sales_2015", 2015, 2016)) %>%
  select(state, year, total_sales) %>%
  arrange(state, year)  # Sort by state and year

# Get the value at row index [35,3]
value_at_35_3 <- df_final[35, 3]

# Print result
value_at_35_3



# Question 5

# Standardize column names
colnames(superstore_data) <- tolower(gsub(" ", "_", colnames(superstore_data)))

# Convert Order Date to Date format
superstore_data <- superstore_data %>%
  mutate(order_date = as.Date(order_date, format = "%m/%d/%Y"))  # Adjust format if needed

# Extract the year from Order Date
superstore_data <- superstore_data %>%
  mutate(year = as.numeric(format(order_date, '%Y')))

# Group by state and year, then calculate total sales
sales_by_state_year <- superstore_data %>%
  filter(year %in% c(2015, 2016)) %>%  # Keep only 2015 and 2016
  group_by(state, year) %>%
  summarise(sales = sum(sales, na.rm = TRUE)) %>%
  arrange(state, year)  # Sort by state and year

# Add 'key' column with value "sales" for all rows
sales_by_state_year$key <- "sales"

# Rename 'sales' column to 'value'
names(sales_by_state_year)[3] <- "value"

# Get the value at row index [33,3]
value_at_33_3 <- sales_by_state_year[33, 3]

# Print result
value_at_33_3



# Question 6

# Standardize column names
colnames(superstore_data) <- tolower(gsub(" ", "_", colnames(superstore_data)))

# Convert Order Date to Date format
superstore_data <- superstore_data %>%
  mutate(order_date = as.Date(order_date, format = "%m/%d/%Y"))  # Adjust format if needed

# Extract the year from Order Date
superstore_data <- superstore_data %>%
  mutate(year = as.numeric(format(order_date, '%Y')))

# Group by state and year, then calculate total sales
sales_by_state_year <- superstore_data %>%
  filter(year %in% c(2015, 2016)) %>%  # Keep only 2015 and 2016
  group_by(state, year) %>%
  summarise(sales = sum(sales, na.rm = TRUE)) %>%
  arrange(state, year)  # Sort by state and year

# Get the value at row index [38,3]
value_at_38_3 <- sales_by_state_year[38, 3]

# Print result
value_at_38_3



# Question 7

# Clean any extra spaces or hidden characters in column names
colnames(superstore_data) <- trimws(colnames(superstore_data))

# Step 1: Convert 'Order Date' to Date format
superstore_data$`Order Date` <- as.Date(superstore_data$`Order Date`, format="%m/%d/%Y")

# Step 2: Extract the year from the 'Order Date'
superstore_data$year <- as.numeric(format(superstore_data$`Order Date`, "%Y"))

# Step 3: Filter data for 2015 and 2016
data_filtered <- superstore_data[superstore_data$year %in% c(2015, 2016), ]

# Step 4: Summarize total order quantity by state and year
order_quantity_by_state_year <- aggregate(`Order Quantity` ~ State + year, data = data_filtered, FUN = sum)

# Step 5: Add a 'key' column with the string 'order.quantity' for all rows
order_quantity_by_state_year$key <- 'order.quantity'

# Step 6: Rename 'Order Quantity' column to 'value'
names(order_quantity_by_state_year)[names(order_quantity_by_state_year) == "Order Quantity"] <- "value"

# Step 7: Sort by state and year
order_quantity_by_state_year_sorted <- order_quantity_by_state_year[order(order_quantity_by_state_year$State, order_quantity_by_state_year$year), ]

# Step 8: Retrieve the value at index [90, 3]
value_at_90_3 <- order_quantity_by_state_year_sorted[90, 3]
print(value_at_90_3)


# Question 8

# Convert Order Date to Date format and extract Year
superstore_data <- superstore_data %>%
  mutate(`Order Date` = as.Date(`Order Date`, format = "%m/%d/%Y"),
         Year = as.numeric(format(`Order Date`, "%Y")))

# ------------------- Q5: Sales Table -------------------
# Create total sales dataframe for 2015
sales_2015 <- superstore_data %>%
  filter(Year == 2015) %>%
  group_by(State, Year) %>%  # Ensure Year is included before summarizing
  summarise(Sales = sum(Sales, na.rm = TRUE), .groups = 'drop') %>%
  mutate(key = "sales")

# Create total sales dataframe for 2016
sales_2016 <- superstore_data %>%
  filter(Year == 2016) %>%
  group_by(State, Year) %>%  # Ensure Year is included before summarizing
  summarise(Sales = sum(Sales, na.rm = TRUE), .groups = 'drop') %>%
  mutate(key = "sales")

# Combine sales data
sales_combined <- bind_rows(sales_2015, sales_2016)

# Rename column 'Sales' to 'Value'
names(sales_combined)[3] <- "Value"

# ------------------- Q7: Order Quantity Table -------------------
# Create total order quantity dataframe for 2015
order_quantity_2015 <- superstore_data %>%
  filter(Year == 2015) %>%
  group_by(State, Year) %>%  # Ensure Year is included before summarizing
  summarise(`Order Quantity` = sum(`Order Quantity`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(key = "order.quantity")

# Create total order quantity dataframe for 2016
order_quantity_2016 <- superstore_data %>%
  filter(Year == 2016) %>%
  group_by(State, Year) %>%  # Ensure Year is included before summarizing
  summarise(`Order Quantity` = sum(`Order Quantity`, na.rm = TRUE), .groups = 'drop') %>%
  mutate(key = "order.quantity")

# Combine order quantity data
order_quantity_combined <- bind_rows(order_quantity_2015, order_quantity_2016)

# Rename 'Order Quantity' to 'Value'
names(order_quantity_combined)[3] <- "Value"

# ------------------- Q9: Concatenate Results -------------------
# Combine sales and order quantity tables
final_combined <- bind_rows(sales_combined, order_quantity_combined) %>%
  arrange(State, Year)  # Sort by State and Year

# Print the final sorted table
print(final_combined)

# Get the value at index [150,3] (150th row, 3rd column)
value_150_3 <- final_combined[150, 3]

# Print the result
print(value_150_3)



# Question 9

# Convert Order Date to Date format
superstore_data <- superstore_data %>%
  mutate(`Order Date` = as.Date(`Order Date`, format = "%m/%d/%Y"))

# Create total sales per order date
sales_by_date <- superstore_data %>%
  group_by(`Order Date`) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE), .groups = 'drop')

# Complete the table with all dates between the earliest and latest dates
date_range <- seq.Date(min(sales_by_date$`Order Date`), max(sales_by_date$`Order Date`), by = "day")

completed_sales <- sales_by_date %>%
  complete(`Order Date` = date_range, fill = list(Total_Sales = NA)) %>%
  arrange(`Order Date`)  # Sort the table by Order Date

# Print the final completed table
print(completed_sales)

# Get the value at index [1000, 2] (1000th row, 2nd column)
value_1000_2 <- completed_sales[1000, 2]

# Print the result
print(value_1000_2)



# Question 10

# Fill NA values in the 'Total_Sales' column upwards
completed_sales_filled <- completed_sales %>%
  fill(Total_Sales, .direction = "up") %>%
  arrange(`Order Date`)  # Ensure the table is sorted by Order Date

# Print the final table with NA values filled upwards
print(completed_sales_filled)

# Get the value at index [890, 2] (890th row, 2nd column)
value_890_2 <- completed_sales_filled[890, 2]

# Print the result
print(value_890_2)



# Question 11

# Fill NA values in the 'Total_Sales' column downwards
completed_sales_filled_down <- completed_sales %>%
  fill(Total_Sales, .direction = "down") %>%
  arrange(`Order Date`)  # Ensure the table is sorted by Order Date

# Print the final table with NA values filled downwards
print(completed_sales_filled_down)

# Get the value at index [890, 2] (890th row, 2nd column)
value_890_2_down <- completed_sales_filled_down[890, 2]

# Print the result
print(value_890_2_down)



# Question 12

# Filter the data to keep only items whose names start with 'Xerox'
xerox_data <- superstore_data %>%
  filter(str_detect(Item, "^Xerox"))

# Extract the product numbers (assuming product numbers are numeric or can be parsed as such)
xerox_data <- xerox_data %>%
  mutate(Product_Number = as.numeric(str_extract(Item, "\\d+"))) %>%
  mutate(Product_Number = ifelse(is.na(Product_Number), 0, Product_Number))  # Fill NA values with 0

# Calculate the sum of all product numbers
sum_product_numbers <- sum(xerox_data$Product_Number)

# Print the result
print(sum_product_numbers)



# Question 13

# Run the Poisson regression
poisson_model <- glm(`Order Quantity` ~ `Unit Price` + `Discount` + `Category`, 
                     data = superstore_data, 
                     family = poisson())

# Summary of the model to see coefficients
summary(poisson_model)

# Extract the coefficient for 'Discount'
coef_discount <- round(coef(poisson_model)["Discount"], 3)

# Print the coefficient for 'Discount'
print(coef_discount)



# Question 14

# Run the dispersion test on the Poisson model
disp_test <- dispersiontest(poisson_model, trafo = 1)

# Print the result of the dispersion test
print(disp_test)

# Extract p-value from the test
p_value_dispersion <- disp_test$p.value

# Check the p-value and interpret
if (p_value_dispersion < 0.05) {
  result <- "Overdispersion exists"
} else {
  result <- "Overdispersion does not exist"
}

# Print the result based on the p-value
print(result)



# Question 15



# Ensure column names are consistent
colnames(superstore_data) <- tolower(gsub(" ", "_", colnames(superstore_data)))  # Standardize column names

# Filter for California transactions and remove missing order priority
superstore_filtered <- superstore_data %>%
  filter(state == "California" & !is.na(order_priority))

# Convert order priority to an ordered factor
superstore_filtered$order_priority <- factor(superstore_filtered$order_priority, 
                                             levels = c("Low", "Medium", "High", "Critical"),
                                             ordered = TRUE)

# Convert category to a factor
superstore_filtered$category <- as.factor(superstore_filtered$category)

# Fit the ordinal logistic regression model
model <- polr(order_priority ~ sales + order_quantity + shipping_cost + category, 
              data = superstore_filtered, Hess = TRUE)

# Extract and round the coefficient for order quantity
coeff_order_quantity <- coef(model)["order_quantity"]
rounded_coeff <- round(coeff_order_quantity, 3)

# Print result
rounded_coeff



# Question 16

# Ensure column names are consistent
colnames(superstore_data) <- tolower(gsub(" ", "_", colnames(superstore_data)))  # Standardize column names

# Filter for order quantity <= 150
superstore_filtered <- superstore_data %>%
  filter(order_quantity <= 150)

# Convert category to a factor
superstore_filtered$category <- as.factor(superstore_filtered$category)

# Fit the truncated regression model
model <- truncreg(order_quantity ~ unit_price + shipping_cost + discount + category, 
                  data = superstore_filtered, 
                  point = 150, direction = "right")  # Truncate at 150

# Extract and round the coefficient for unit price
coeff_unit_price <- coef(model)["unit_price"]
rounded_coeff <- round(coeff_unit_price, 3)

# Print result
rounded_coeff


