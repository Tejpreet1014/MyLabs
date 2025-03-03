# Load required libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr) 

# Read the CSV file
superstore_data <- read_csv("C:/Users/hp/Desktop/MS DAB/R Programming/Superstore.csv")

# Ensure the 'Shipping Cost' column is numeric
superstore_data$`Shipping Cost` <- as.numeric(superstore_data$`Shipping Cost`)

# Create the histogram using ggplot2
ggplot(superstore_data, aes(x = `Shipping Cost`)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Shipping Costs",
       x = "Shipping Cost",
       y = "Frequency") +
  theme_minimal()

# Calculate the most common shipping cost   Question 2
most_common_shipping_cost <- as.numeric(names(sort(table(superstore_data$`Shipping Cost`), decreasing = TRUE)[1]))
print(paste("Most common shipping cost:", most_common_shipping_cost))


# Question 3
# Ensure 'Order Quantity' and 'Ship Mode' columns are properly formatted
superstore_data$`Order Quantity` <- as.numeric(superstore_data$`Order Quantity`)
superstore_data$`Ship Mode` <- as.factor(superstore_data$`Ship Mode`)

# Create the histogram with facet_wrap
ggplot(superstore_data, aes(x = `Order Quantity`)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue", alpha = 0.7) +
  facet_wrap(~`Ship Mode`, scales = "free_y") +
  labs(title = "Distribution of Order Quantity by Ship Mode",
       x = "Order Quantity",
       y = "Frequency") +
  theme_minimal()

# Determine which ship mode has the most transactions with order quantity > 100
high_order_transactions <- superstore_data %>%
  filter(`Order Quantity` > 100) %>%
  count(`Ship Mode`, name = "Transaction Count") %>%
  arrange(desc(`Transaction Count`))

print(high_order_transactions)



# Question 4
# Ensure the 'Unit Price' column is numeric
superstore_data$`Unit Price` <- as.numeric(superstore_data$`Unit Price`)

# Create the histogram using ggplot2 and zoom in on the range of unit prices
ggplot(superstore_data, aes(x = `Unit Price`)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue", alpha = 0.7) +
  coord_cartesian(xlim = c(0, 50)) +  # Adjust the range for zooming in
  labs(title = "Distribution of Unit Prices",
       x = "Unit Price",
       y = "Frequency") +
  theme_minimal()

# Calculate the most common unit price (mode)
most_common_unit_price <- as.numeric(names(sort(table(superstore_data$`Unit Price`), decreasing = TRUE)[1]))
print(paste("Most common unit price:", most_common_unit_price))


# Question  5
# Ensure 'Unit Price' and 'Category' columns are properly formatted
superstore_data$`Unit Price` <- as.numeric(superstore_data$`Unit Price`)
superstore_data$Category <- as.factor(superstore_data$Category)

# Create the boxplot using ggplot2
ggplot(superstore_data, aes(x = Category, y = `Unit Price`)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Distribution of Unit Price by Product Category",
       x = "Product Category",
       y = "Unit Price") +
  theme_minimal()

# Count categories with no outliers
outliers_by_category <- superstore_data %>%
  group_by(Category) %>%
  summarise(
    IQR = IQR(`Unit Price`, na.rm = TRUE),
    Q1 = quantile(`Unit Price`, 0.25, na.rm = TRUE),
    Q3 = quantile(`Unit Price`, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  rowwise() %>%
  mutate(
    Has_Outliers = any(superstore_data$`Unit Price`[superstore_data$Category == Category] < Lower_Bound |
                         superstore_data$`Unit Price`[superstore_data$Category == Category] > Upper_Bound, na.rm = TRUE)
  ) %>%
  summarise(No_Outliers_Count = sum(!Has_Outliers))

print(outliers_by_category)


# Question 6
# Ensure 'Customer Segment' and 'Ship Mode' columns are properly formatted
superstore_data$`Customer Segment` <- as.factor(superstore_data$`Customer Segment`)
superstore_data$`Ship Mode` <- as.factor(superstore_data$`Ship Mode`)

# Create the bar chart
ggplot(superstore_data, aes(x = `Customer Segment`, fill = `Ship Mode`)) +
  geom_bar(position = "stack") +
  labs(title = "Count of Transactions by Customer Segment and Ship Mode",
       x = "Customer Segment",
       y = "Count of Transactions") +
  theme_minimal()

# Count the transactions for each customer segment with 'Regular Air' ship mode
regular_air_transactions <- superstore_data %>%
  filter(`Ship Mode` == "Regular Air") %>%
  count(`Customer Segment`, name = "Transaction Count") %>%
  arrange(desc(`Transaction Count`))

print(regular_air_transactions)


# Question 7
# Ensure 'Sales', 'Category', and 'Region' columns are properly formatted
superstore_data$Sales <- as.numeric(superstore_data$Sales)
superstore_data$Category <- as.factor(superstore_data$Category)
superstore_data$Region <- as.factor(superstore_data$Region)

# Create the bar chart of sales by product category for each region
ggplot(superstore_data, aes(x = Category, y = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales by Product Category for Each Region",
       x = "Product Category",
       y = "Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Filter data for the East region and identify the product category with the highest sales
east_region_sales <- superstore_data %>%
  filter(Region == "East") %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

# Output the top category in the East region
top_category_east <- east_region_sales[1, ]
print(top_category_east)



# Question 8
# Ensure 'Customer Segment', 'State' columns are properly formatted
superstore_data$`Customer Segment` <- as.factor(superstore_data$`Customer Segment`)
superstore_data$State <- as.factor(superstore_data$State)

# Create the bar chart of count of customers by state and customer segment
ggplot(superstore_data, aes(x = State, fill = `Customer Segment`)) +
  geom_bar(position = "stack") +
  labs(title = "Count of Customers by State and Customer Segment",
       x = "State",
       y = "Count of Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Count the number of Small Business customers by state
small_business_customers <- superstore_data %>%
  filter(`Customer Segment` == "Small Business") %>%
  count(State, name = "Customer Count") %>%
  arrange(desc(`Customer Count`))

# Output the state with the most Small Business customers
top_state_small_business <- small_business_customers[1, ]
print(top_state_small_business)



# Question 9
# Ensure 'Order Date' is in the correct date format
superstore_data$`Order Date` <- as.Date(superstore_data$`Order Date`, format = "%m/%d/%Y")

# Extract year and month from the 'Order Date'
superstore_data$year <- format(superstore_data$`Order Date`, "%Y")
superstore_data$month <- format(superstore_data$`Order Date`, "%m")

# Create a boxplot of sales by month and zoom in
ggplot(superstore_data, aes(x = month, y = Sales)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(title = "Sales by Month",
       x = "Month",
       y = "Sales") +
  coord_cartesian(ylim = c(0, 20000)) +  # Zooming in on sales range
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the median sales by month
median_sales_by_month <- superstore_data %>%
  group_by(month) %>%
  summarise(median_sales = median(Sales, na.rm = TRUE)) %>%
  arrange(desc(median_sales))

# Output the month with the highest median sales
top_month <- median_sales_by_month[1, ]
print(top_month)



# Question 10
# Ensure 'Order Date' is in the correct date format
superstore_data$`Order Date` <- as.Date(superstore_data$`Order Date`, format = "%m/%d/%Y")

# Extract 'year_month' from the 'Order Date'
superstore_data$year_month <- format(superstore_data$`Order Date`, "%Y/%m")

# Create a boxplot of order quantity by 'year_month' and zoom in
ggplot(superstore_data, aes(x = year_month, y = `Order Quantity`)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(title = "Order Quantity by Year-Month",
       x = "Year-Month",
       y = "Order Quantity") +
  coord_cartesian(ylim = c(0, 100)) +  # Zooming in on order quantity range
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the third quartile (Q3) for order quantity by 'year_month'
order_quantity_quartiles <- superstore_data %>%
  group_by(year_month) %>%
  summarise(Q3 = quantile(`Order Quantity`, 0.75, na.rm = TRUE)) %>%
  arrange(desc(Q3))

# Output the year_month with the highest third quartile for order quantity
top_year_month <- order_quantity_quartiles[1, ]
top_year_month$year_month <- gsub("/", "", top_year_month$year_month)  # Format as YYYYMM
print(top_year_month)



# Question 11
# Ensure 'Customer Segment' and 'Region' columns are properly formatted
superstore_data$`Customer Segment` <- as.factor(superstore_data$`Customer Segment`)
superstore_data$Region <- as.factor(superstore_data$Region)

# Create the bar chart of total number of customers by customer segment for each region
ggplot(superstore_data, aes(x = Region, fill = `Customer Segment`)) +
  geom_bar(position = "stack") +
  labs(title = "Total Number of Customers by Customer Segment for Each Region",
       x = "Region",
       y = "Count of Customers") +
  theme_minimal()

# Summarize sales by region and customer segment (Small Business and Home Office)
sales_by_segment <- superstore_data %>%
  group_by(Region, `Customer Segment`) %>%
  summarise(total_sales = sum(Sales, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Customer Segment`, values_from = total_sales, values_fill = list(total_sales = 0))

# Identify which region has more sales from Small Business than Home Office
sales_by_segment %>%
  filter(`Small Business` > `Home Office`)



# Question 12
# Ensure 'Order Date' is in the correct date format
superstore_data$`Order Date` <- as.Date(superstore_data$`Order Date`, format = "%m/%d/%Y")

# Extract 'year_month' from the 'Order Date'
superstore_data$year_month <- format(superstore_data$`Order Date`, "%Y/%m")

# Create a bar chart of order quantity by 'year_month' and zoom in
ggplot(superstore_data, aes(x = year_month, y = `Order Quantity`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Order Quantity by Year-Month",
       x = "Year-Month",
       y = "Order Quantity") +
  coord_cartesian(ylim = c(0, 500)) +  # Zooming in on order quantity range, adjust if needed
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the total order quantity by 'year_month'
order_quantity_by_month <- superstore_data %>%
  group_by(year_month) %>%
  summarise(total_order_quantity = sum(`Order Quantity`, na.rm = TRUE)) %>%
  arrange(desc(total_order_quantity))

# Output the 'year_month' with the highest order quantity
top_year_month <- order_quantity_by_month[1, ]
top_year_month$year_month <- gsub("/", "", top_year_month$year_month)  # Format as YYYYMM
print(top_year_month)



# Question 13
# Create a density plot of profit by product category
ggplot(superstore_data, aes(x = Profit, fill = Category)) +
  geom_density(alpha = 0.5) +  # alpha for transparency
  labs(title = "Density Plot of Profit by Product Category",
       x = "Profit",
       y = "Density") +
  theme_minimal()

# Calculate the variance of profit for each product category
profit_variance_by_category <- superstore_data %>%
  group_by(Category) %>%
  summarise(profit_variance = var(Profit, na.rm = TRUE)) %>%
  arrange(desc(profit_variance))

# Output the product category with the greatest variance in profit
top_category <- profit_variance_by_category[1, ]
print(top_category)



# Question 14
# Filter the data for the two categories: 'Appliances' and 'Storage & Organization'
filtered_data <- superstore_data %>%
  filter(Category %in% c("Appliances", "Storage & Organization"))

# Perform the two-sample t-test
t_test_result <- t.test(`Unit Price` ~ Category, data = filtered_data)

# Extract the p-value and round it to two decimal places
p_value <- round(t_test_result$p.value, 2)

# Output the p-value
p_value



# Question 15
# Perform the one-sample t-test for mean unit price <= 83
t_test_result <- t.test(superstore_data$`Unit Price`, mu = 83, alternative = "greater")

# Extract the p-value and round it to two decimal places
p_value <- round(t_test_result$p.value, 2)

# Output the p-value
p_value



# Question 16
# Perform the ANOVA
anova_model <- aov(Sales ~ Category * State, data = superstore_data)

# Display the ANOVA table
anova_table <- summary(anova_model)

# Extract the F-statistic for 'State'
f_statistic_state <- anova_table[[1]]$`F value`[2]  # F value for 'State'

# Round the F-statistic to two decimal places
f_statistic_state_rounded <- round(f_statistic_state, 2)

# Output the F-statistic for 'State'
f_statistic_state_rounded