#Lab 3
# Load required libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr) 
library(readxl)

# Read the CSV file
superstore_data <- read_csv("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.csv")

# Question 2

# Find the transaction with the highest sales value
max_sales_transaction <- superstore_data %>%
  filter(Sales == max(Sales, na.rm = TRUE)) %>%
  select(State, Sales)

# Display the result
max_sales_transaction



# Question 3

# Count transactions where Order Quantity is greater than 50
high_order_transactions <- superstore_data %>%
  filter(`Order Quantity` > 50) %>%
  nrow()

# Display the result
high_order_transactions



# Question 4

# Count unique items
unique_items_count <- superstore_data %>%
  summarise(unique_items = n_distinct(`Item`))

# Display the result
unique_items_count


# Question 5

# Calculate the inter-quartile range for Unit Price
unit_price_iqr <- IQR(superstore_data$`Unit Price`, na.rm = TRUE)

# Print the result
print(unit_price_iqr)


# Question 6

# Recalculate total sales for Arizona and average without rounding
total_sales_arizona <- superstore_data %>%
  filter(State == "Arizona") %>%
  summarise(total_sales = sum(Sales, na.rm = TRUE),
            num_transactions = n(),
            avg_sales_exact = mean(Sales, na.rm = TRUE))  # Don't round yet

# Print the result with the exact average
print(total_sales_arizona)

# Then, round it to two decimals
rounded_avg_sales <- round(total_sales_arizona$avg_sales_exact, 2)
print(rounded_avg_sales)


# Question 7

# Convert Order Date to Date format (if not already)
superstore_data <- superstore_data %>%
  mutate(`Order Date` = as.Date(`Order Date`, format = "%m/%d/%Y"))

# Find the order date with the highest average sales
highest_avg_sales_date <- superstore_data %>%
  group_by(`Order Date`) %>%
  summarise(avg_sales = mean(Sales, na.rm = TRUE)) %>%
  arrange(desc(avg_sales)) %>%
  slice(1) %>%
  mutate(formatted_date = format(`Order Date`, "%m%d%Y"))

# Display the result
highest_avg_sales_date$formatted_date



# Question 8

# Convert necessary columns to numeric (if they are not already)
superstore_data <- superstore_data %>%
  mutate(`Shipping Cost` = as.numeric(`Shipping Cost`),
         Sales = as.numeric(Sales),
         Shipping_Sales_Ratio = `Shipping Cost` / Sales)

# Find the row with the highest ratio
max_ratio_row <- superstore_data %>%
  filter(Shipping_Sales_Ratio == max(Shipping_Sales_Ratio, na.rm = TRUE))

# Extract the Item ID for that transaction
highest_ratio_item_id <- max_ratio_row$`Item ID`

# Print the result
print(highest_ratio_item_id)



# Question 9

# Calculate average shipping cost by Item ID and Ship Mode
avg_shipping_cost <- superstore_data %>%
  group_by(`Item ID`, `Ship Mode`) %>%
  summarise(Average_Shipping_Cost = mean(`Shipping Cost`, na.rm = TRUE)) %>%
  ungroup()

# Filter for shipments made by Delivery Truck
delivery_truck_data <- avg_shipping_cost %>%
  filter(`Ship Mode` == "Delivery Truck")

# Find the Item ID with the highest average shipping cost for Delivery Truck
max_shipping_item <- delivery_truck_data %>%
  filter(Average_Shipping_Cost == max(Average_Shipping_Cost, na.rm = TRUE))

# Print the result
print(max_shipping_item)



# Question 10

# Convert Order Date to Date format
superstore_data <- superstore_data %>%
  mutate(`Order Date` = as.Date(`Order Date`, format = "%m/%d/%Y"))

# Filter data for Paper category and the required date range
paper_sales <- superstore_data %>%
  filter(Category == "Paper" & `Order Date` >= as.Date("2015-11-30") & `Order Date` <= as.Date("2015-12-05")) %>%
  group_by(`Order Date`) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))

# Create a line plot
ggplot(data = paper_sales, aes(x = `Order Date`, y = Total_Sales)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Total Sales for Paper Category (Nov 30 - Dec 5, 2015)",
       x = "Order Date",
       y = "Total Sales") +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2015-11-30"), as.Date("2015-12-05")))

# Find the order date with the lowest total sales
lowest_sales_date <- paper_sales %>%
  filter(Total_Sales == min(Total_Sales, na.rm = TRUE)) %>%
  pull(`Order Date`)

# Convert to required format MMDDYYYY
lowest_sales_date_formatted <- format(lowest_sales_date, "%m%d%Y")

# Print the result
print(lowest_sales_date_formatted)



# Question 11

# Convert Order Date to Date format if not already done
superstore_data <- superstore_data %>%
  mutate(`Order Date` = as.Date(`Order Date`, format = "%m/%d/%Y"))

# Filter for the required categories and date range
selected_categories <- c("Binders and Binder Accessories", "Telephones and Communication", "Computer Peripherals")

filtered_data <- superstore_data %>%
  filter(Category %in% selected_categories & `Order Date` >= as.Date("2015-08-15") & `Order Date` <= as.Date("2015-08-21")) %>%
  group_by(`Order Date`, Category) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  ungroup()

# Create the line plot
ggplot(data = filtered_data, aes(x = `Order Date`, y = Total_Sales, col = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Total Sales for Selected Categories (Aug 15 - Aug 21, 2015)",
       x = "Order Date",
       y = "Total Sales",
       color = "Category") +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2015-08-15"), as.Date("2015-08-21"))) +
  scale_y_continuous(limits = c(0, max(filtered_data$Total_Sales, na.rm = TRUE) * 1.1))

# Find the category with the highest total sales on 2015-08-21
highest_sales_category <- superstore_data %>%
  filter(`Order Date` == as.Date("2015-08-21")) %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  slice(1) %>%
  pull(Category)

# Print the result
print(highest_sales_category)



# Question 12

# Convert Order Date and Ship Date to Date format if not already done
superstore_data <- superstore_data %>%
  mutate(`Order Date` = as.Date(`Order Date`, format = "%m/%d/%Y"),
         `Ship Date` = as.Date(`Ship Date`, format = "%m/%d/%Y"))

# Calculate Lead Time (time from Order Date to Ship Date)
superstore_data <- superstore_data %>%
  mutate(Lead.Time = as.numeric(difftime(`Ship Date`, `Order Date`, units = "days")))

# Calculate the average Lead Time per unique Item ID
avg_lead_time <- superstore_data %>%
  group_by(`Item ID`) %>%
  summarise(Average_Lead_Time = mean(Lead.Time, na.rm = TRUE)) %>%
  ungroup()

# Find the longest average lead time
longest_avg_lead_time <- max(avg_lead_time$Average_Lead_Time, na.rm = TRUE)

# Print the result
print(longest_avg_lead_time)



# Question 13

# Check for missing values in the relevant columns
sum(is.na(superstore_data$`Unit Price`))  # Check for NAs in Unit Price
sum(is.na(superstore_data$Discount))      # Check for NAs in Discount
sum(is.na(superstore_data$Sales))         # Check for NAs in Sales
sum(is.na(superstore_data$Category))      # Check for NAs in Category

# Remove rows with NAs in any of the relevant columns (optional, if you decide to remove them)
superstore_data <- superstore_data %>%
  filter(!is.na(`Unit Price`) & !is.na(Discount) & !is.na(Sales) & !is.na(Category))

# Ensure Category is a factor
superstore_data$Category <- as.factor(superstore_data$Category)

# Run linear regression
lm_model <- lm(Sales ~ `Unit Price` + Category + Discount, data = superstore_data)

# Display summary to check coefficient names
summary(lm_model)

# Extract and round the coefficient for Unit Price
unit_price_coef <- coef(lm_model)["`Unit Price`"]
rounded_coef <- round(unit_price_coef, 2)

# Print the result
print(rounded_coef)



# Question 14

# Group by item (you might need to adjust based on your actual item column name)
item_level_data <- superstore_data %>%
  group_by(Item) %>%  # Assuming 'Item' is the name of your unique item column
  summarise(
    total_sales = sum(Sales, na.rm = TRUE),
    avg_unit_price = mean(`Unit Price`, na.rm = TRUE),
    avg_discount = mean(Discount, na.rm = TRUE),
    avg_shipping_cost = mean(`Shipping Cost`, na.rm = TRUE),
    category = first(Category)  # Assuming category is the same for all rows within an item
  )

# Check for missing values in the new dataset
sum(is.na(item_level_data$total_sales)) 
sum(is.na(item_level_data$avg_unit_price)) 
sum(is.na(item_level_data$avg_discount)) 
sum(is.na(item_level_data$avg_shipping_cost)) 
sum(is.na(item_level_data$category))

# Remove rows with NA values in any of the relevant columns (optional)
item_level_data <- item_level_data %>%
  filter(!is.na(total_sales) & !is.na(avg_unit_price) & !is.na(avg_discount) & !is.na(avg_shipping_cost) & !is.na(category))

# Ensure category is a factor
item_level_data$category <- as.factor(item_level_data$category)

# Run linear regression
lm_model_item <- lm(total_sales ~ avg_unit_price + avg_discount + avg_shipping_cost + category, data = item_level_data)

# Display summary to check coefficient names
summary(lm_model_item)

# Extract and round the coefficient for avg_unit_price
unit_price_coef_item <- coef(lm_model_item)["avg_unit_price"]
rounded_coef_item <- round(unit_price_coef_item, 2)

# Print the result
print(rounded_coef_item)



# Queswtion 15

# Create the binary variable 'pos_profit'
superstore_data$pos_profit <- ifelse(superstore_data$Profit > 0, 1, 0)

# Check if the new variable is created correctly
table(superstore_data$pos_profit)

# Run logistic regression of pos_profit on sales, shipping cost, discount, and unit price at the transaction level
logistic_model <- glm(pos_profit ~ Sales + `Shipping Cost` + Discount + `Unit Price`, 
                      data = superstore_data, 
                      family = binomial)

# Display summary to check coefficient names
summary(logistic_model)

# Extract and round the coefficient for Discount
discount_coef <- coef(logistic_model)["Discount"]
rounded_discount_coef <- round(discount_coef, 2)

# Print the result
print(rounded_discount_coef)



# Question 16

# Create the binary variable 'high_sales'
superstore_data$high_sales <- ifelse(superstore_data$Sales > 100, 1, 0)

# Check if the new variable is created correctly
table(superstore_data$high_sales)

# Run probit regression of high_sales on category, discount, order quantity, product base margin, state, shipping cost, and unit price at the transaction level
probit_model <- glm(high_sales ~ Category + Discount + `Order Quantity` + `Product Base Margin` + 
                      State + `Shipping Cost` + `Unit Price`, 
                    data = superstore_data, 
                    family = binomial(link = "probit"))

# Display summary to check coefficient names
summary(probit_model)

# Extract and round the coefficient for Discount
discount_coef_probit <- coef(probit_model)["Discount"]
rounded_discount_coef_probit <- round(discount_coef_probit, 2)

# Print the result
print(rounded_discount_coef_probit)
