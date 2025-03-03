# Lab 5

install.packages('nycflights13')
install.packages("survival")
install.packages("survminer")


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
library(nycflights13)
library(survival)

lung_disease <- read_csv("C:/Users/hp/Desktop/MS DAB/R Programming/lung_disease.csv")

str(flights)  # Check structure of the flights data
head(flights) # Preview the first few rows

str(lung_disease)  # Check structure of the lung disease data
head(lung_disease) # Preview the first few rows

# Question 2

# Perform an inner join on 'carrier'
flights_airlines <- flights %>%
  inner_join(airlines, by = "carrier")

# Filter for Virgin America flights
virgin_flights <- flights_airlines %>%
  filter(name == "Virgin America")

# Count the number of flights
num_virgin_flights <- nrow(virgin_flights)

# Display the result
num_virgin_flights



# Question 3

# Perform an inner join on 'tailnum'
flights_planes <- flights %>%
  inner_join(planes, by = "tailnum")

# Filter for aircraft manufactured by EMBRAER
embraer_flights <- flights_planes %>%
  filter(manufacturer == "EMBRAER")

# Count the number of flights
num_embraer_flights <- nrow(embraer_flights)

# Display the result
num_embraer_flights



# Question 4

# Perform an inner join on multiple columns
flights_weather <- flights %>%
  inner_join(weather, by = c("year", "month", "day", "hour", "origin"))

# Filter for flights with wind speed > 11
high_wind_flights <- flights_weather %>%
  filter(wind_speed > 11)

# Count the number of flights
num_high_wind_flights <- nrow(high_wind_flights)

# Display the result
num_high_wind_flights



# Question 5

# Perform an inner join on different column names
flights_airports <- flights %>%
  inner_join(airports, by = c("origin" = "faa"))

# Filter for airports with latitude > 40.7
high_latitude_flights <- flights_airports %>%
  filter(lat > 40.7)

# Count the number of flights
num_high_latitude_flights <- nrow(high_latitude_flights)

# Display the result
num_high_latitude_flights



# Question 6

# Join flights with airlines to get airline names
flights_aa <- flights %>%
  inner_join(airlines, by = "carrier") %>%
  filter(name == "American Airlines" & dest == "LGA")

# Count the number of flights
num_aa_lga_flights <- nrow(flights_aa)

# Display the result
num_aa_lga_flights


#Check unique carriers
unique(flights$carrier)

# Check unique destination airports
unique(flights$dest)

# Check the airlines data
airlines %>% filter(carrier == "AA")

# Join flights with airlines to get airline names
flights_aa <- flights %>%
  inner_join(airlines, by = "carrier") %>%
  filter(carrier == "AA" & dest == "LGA")  # Use the carrier code directly

# Count the number of flights
num_aa_lga_flights <- nrow(flights_aa)

# Display the result
num_aa_lga_flights


# Question 7

# Join flights with planes using tailnum
flights_planes <- flights %>%
  inner_join(planes, by = "tailnum")

# Filter for planes manufactured by EMBRAER
embraer_flights <- flights_planes %>%
  filter(manufacturer == "EMBRAER")

# Calculate the total number of seats available
total_seats_embraer <- sum(embraer_flights$seats, na.rm = TRUE)

# Display the result
total_seats_embraer



# Question 8

# Join flights with planes using tailnum
flights_planes <- flights %>%
  inner_join(planes, by = "tailnum")

# Filter for flights using the EMB-145XR model
embraer_145xr_flights <- flights_planes %>%
  filter(model == "EMB-145XR")

# Calculate the total distance flown
total_distance_emb145xr <- sum(embraer_145xr_flights$distance, na.rm = TRUE)

# Display the result
total_distance_emb145xr



# Question 9

# Join flights with planes using tailnum
flights_planes <- flights %>%
  inner_join(planes, by = "tailnum")

# Join with airlines to get airline names
flights_planes_airlines <- flights_planes %>%
  inner_join(airlines, by = "carrier")

# Filter for planes manufactured by EMBRAER and operated by JetBlue Airways
jetblue_embraer_flights <- flights_planes_airlines %>%
  filter(manufacturer == "EMBRAER" & name == "JetBlue Airways")

# Count the number of flights
num_jetblue_embraer_flights <- nrow(jetblue_embraer_flights)

# Display the result
num_jetblue_embraer_flights



# Question 10

# Join flights with airlines to get airline names
flights_airlines <- flights %>%
  inner_join(airlines, by = "carrier")

# Filter for flights operated by Envoy Air and air time > 150 minutes
envoy_flights <- flights_airlines %>%
  filter(name == "Envoy Air" & air_time > 150)

# Count the number of flights
num_envoy_flights <- nrow(envoy_flights)

# Display the result
num_envoy_flights



# Question 11

# Perform a left join on tailnum
flights_planes <- flights %>%
  left_join(planes, by = "tailnum")

# Filter for flights with no corresponding plane model
flights_no_model <- flights_planes %>%
  filter(is.na(model))

# Count the number of flights
num_flights_no_model <- nrow(flights_no_model)

# Display the result
num_flights_no_model



# Question 12

# Create a survival object
surv_object <- Surv(time = lung_disease$time, event = lung_disease$status)

# Fit survival curves for different ph.ecog groups
surv_fit <- survfit(surv_object ~ ph.ecog, data = lung_disease)

# Create an empty data frame to store results
surv_fit_df <- data.frame()

# Loop through each stratum to build the data frame
for (i in 1:length(surv_fit$strata)) {
  strat_data <- data.frame(
    time = surv_fit$time[sum(surv_fit$n[1:(i-1)]) + 1:surv_fit$n[i]],
    surv = surv_fit$surv[sum(surv_fit$n[1:(i-1)]) + 1:surv_fit$n[i]],
    strata = rep(names(surv_fit$strata)[i], surv_fit$n[i])
  )
  surv_fit_df <- rbind(surv_fit_df, strat_data)
}

# Plot the survival curves
ggplot(data = surv_fit_df, aes(x = time, y = surv, color = strata)) +
  geom_step() +
  labs(title = "Survival Probability vs. Time",
       x = "Time",
       y = "Survival Probability",
       color = "ph.ecog Score") +
  theme_minimal()

# Extract median survival times for each ph.ecog group
median_survival_times <- summary(surv_fit)$table[, "median"]
median_survival_times




# Question 13 

# Check the structure of the dataset
str(lung_disease)

# Preview the data
head(lung_disease)

# Check the unique values in ph.ecog and status
unique(lung_disease$ph.ecog)
table(lung_disease$status)

# Count events for each ph.ecog group
table(lung_disease$ph.ecog, lung_disease$status)


# Check for missing values
sum(is.na(lung_disease$ph.ecog))
sum(is.na(lung_disease$time))

# Recalculate cumulative events for debugging
cumulative_events_df <- data.frame()

# Loop through each ph.ecog group to build cumulative events
for (ph_score in unique(lung_disease$ph.ecog)) {
  if (!is.na(ph_score)) {  # Skip NA values
    subset_data <- lung_disease[lung_disease$ph.ecog == ph_score, ]
    if (nrow(subset_data) > 0) {
      events <- Surv(time = subset_data$time, event = subset_data$status == 1)
      fit <- survfit(events ~ 1)  # Fit survival for the current ph.ecog group
      cum_events <- data.frame(time = fit$time, cum_events = cumsum(fit$n.event), ph.ecog = ph_score)
      cumulative_events_df <- rbind(cumulative_events_df, cum_events)
    }
  }
}

ggplot(data = cumulative_events_df, aes(x = time, y = cum_events, color = as.factor(ph.ecog))) +
  geom_step() +
  labs(title = "Cumulative Events vs. Time",
       x = "Time",
       y = "Cumulative Events",
       color = "ph.ecog Score") +
  theme_minimal()

# Calculate time to reach 50% cumulative events
time_to_50_events <- sapply(unique(lung_disease$ph.ecog), function(ph_score) {
  events <- cumulative_events_df$cum_events[cumulative_events_df$ph.ecog == ph_score]
  if (length(events) > 0) {
    total_events <- sum(lung_disease$status == 1 & lung_disease$ph.ecog == ph_score, na.rm = TRUE)
    if (total_events > 0) {
      time_at_50 <- cumulative_events_df$time[cumulative_events_df$ph.ecog == ph_score][which(events >= 0.5 * total_events)[1]]
      return(time_at_50)
    } else {
      return(NA)  # Return NA if no events exist for that group
    }
  } else {
    return(NA)  # Return NA if no events exist for that group
  }
})

# Create a named vector for easier identification
names(time_to_50_events) <- unique(lung_disease$ph.ecog)
time_to_50_events



# Question 14

surv_object <- Surv(time = lung_disease$time, event = lung_disease$status == 1)  # Event is coded as 1

log_rank_test <- survdiff(surv_object ~ ph.ecog, data = lung_disease)

# Print the results of the log-rank test
print(log_rank_test)

# Extract and round the chi-squared statistic
chisq_statistic <- round(log_rank_test$chisq)
chisq_statistic



# Question 15


# Create survival object
surv_object <- Surv(time = lung_disease$time, event = lung_disease$status == 1)

# Fit Cox regression model
cox_model <- coxph(surv_object ~ age + sex + ph.ecog + ph.karno + pat.karno, data = lung_disease)

# Summary of the Cox model
summary_cox_model <- summary(cox_model)

# Extract the coefficient for ph.ecog
ph_ecog_coefficient <- summary_cox_model$coefficients["ph.ecog", "coef"]

# Round the coefficient to three decimal places
ph_ecog_coefficient_rounded <- round(ph_ecog_coefficient, 3)

# Print the rounded coefficient
ph_ecog_coefficient_rounded

print(summary_cox_model)

print(summary_cox_model$coefficients)

ph_ecog_coefficient <- summary_cox_model$coefficients["ph.ecog", "coef"]
print(ph_ecog_coefficient)  # Print the raw coefficient value before rounding




# Question 16



surv_object <- Surv(time = lung_disease$time, event = lung_disease$status == 1)  # Event is coded as 1

# Fit the Cox regression model
cox_model <- coxph(surv_object ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, data = lung_disease)

# Print the summary of the Cox model
summary_cox_model <- summary(cox_model)
print(summary_cox_model)

# Extract the p-value for ph.ecog
ph_ecog_p_value <- summary_cox_model$coefficients["ph.ecog", "Pr(>|z|)"]

# Round the p-value to three decimal places
ph_ecog_p_value_rounded <- round(ph_ecog_p_value, 3)
ph_ecog_p_value_rounded










# Question 13

# Load necessary libraries
library(survival)
library(survminer)
library(dplyr)

# Remove missing values in ph.ecog
lung_disease <- lung_disease %>% filter(!is.na(ph.ecog))

# Convert ph.ecog to a factor for grouping
lung_disease$ph.ecog <- as.factor(lung_disease$ph.ecog)

# Define survival object
surv_object <- Surv(time = lung_disease$time, event = lung_disease$status == 2)

# Fit survival curves using Kaplan-Meier estimator
km_fit <- survfit(surv_object ~ ph.ecog, data = lung_disease)

# Plot cumulative events
ggsurvplot(km_fit, data = lung_disease, fun = "event",
           xlab = "Time", ylab = "Cumulative Events",
           title = "Cumulative Events vs. Time for Different ph.ecog Scores",
           risk.table = FALSE, legend.title = "ph.ecog")

# Extract median survival times for each ph.ecog group
median_times <- summary(km_fit)$table[, "median"]

# Identify the group with the shortest median survival time
shortest_time_group <- names(which.min(median_times))

# Print results
print(median_times)
cat("The ph.ecog group", shortest_time_group, "reaches 50% of cumulative events the fastest.\n")



# Question 14

# Load necessary libraries
library(survival)
library(dplyr)

# Remove missing values in ph.ecog
lung_disease <- lung_disease %>% filter(!is.na(ph.ecog))

# Convert ph.ecog to a factor for grouping
lung_disease$ph.ecog <- as.factor(lung_disease$ph.ecog)

# Define survival object
surv_object <- Surv(time = lung_disease$time, event = lung_disease$status == 2)

# Perform log-rank test (rho = 0 by default)
log_rank_test <- survdiff(surv_object ~ ph.ecog, data = lung_disease, rho = 0)

# Extract chi-square statistic and round to nearest whole number
chisq_stat <- round(log_rank_test$chisq)

# Print result
cat("The chi-square statistic is:", chisq_stat, "\n")



# Question 15

# Load necessary libraries
library(survival)
library(dplyr)

# Ensure status is correctly coded (1 for event, 2 for censored, so we recode 2 as 0)
lung_disease <- lung_disease %>%
  mutate(status = ifelse(status == 2, 0, 1))

# Fit the Cox proportional hazards model
cox_model <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno, data = lung_disease)

# Display the summary
summary(cox_model)

# Extract and round the coefficient for ph.ecog
round(coef(cox_model)["ph.ecog"], 3)

round(coef(cox_model)[c("ph.ecog1", "ph.ecog2")], 3)



# Question 16

# Load necessary libraries
library(survival)
library(dplyr)

# Read the dataset
lung_disease <- read_csv("C:/Users/hp/Desktop/MS DAB/R Programming/lung_disease.csv")

# Ensure status is correctly coded (1 = event occurred, 2 = censored, so recode 2 as 0)
lung_disease <- lung_disease %>%
  mutate(status = ifelse(status == 2, 0, 1))

# Fit the Cox proportional hazards model with the specified variables
cox_model <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, 
                   data = lung_disease)

# Display the summary of the model
summary(cox_model)

# Extract and round the p-value for ph.ecog
round(summary(cox_model)$coefficients["ph.ecog", "Pr(>|z|)"], 3)




















# Question 15 attempt

# Load necessary libraries
library(survival)
library(readr)

# Read the dataset
lung_disease <- read_csv("C:/Users/hp/Desktop/MS DAB/R Programming/lung_disease.csv")

# Select relevant variables and remove missing values
lung_disease <- na.omit(lung_disease[, c("time", "status", "age", "sex", "ph.ecog", "ph.karno", "pat.karno")])

# Convert status to binary (1 = event occurred, 0 = censored)
lung_disease$status <- ifelse(lung_disease$status == 2, 1, 0)

# Fit the Cox Proportional Hazards model
cox_model <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno, data = lung_disease)

# Print the coefficient of ph.ecog rounded to 3 decimals
round(coef(cox_model)["ph.ecog"], 3)




# Question 16 attempt

# Load necessary libraries
library(survival)
library(readr)

# Read the dataset
lung_disease <- read_csv("C:/Users/hp/Desktop/MS DAB/R Programming/lung_disease.csv")

# Select relevant variables and remove missing values
lung_disease <- na.omit(lung_disease[, c("time", "status", "age", "sex", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss")])

# Convert status to binary (1 = event occurred, 0 = censored)
lung_disease$status <- ifelse(lung_disease$status == 2, 1, 0)

# Fit the Cox Proportional Hazards model
cox_model <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, data = lung_disease)

# Print the p-value of ph.ecog rounded to 3 decimals
round(summary(cox_model)$coefficients["ph.ecog", "Pr(>|z|)"], 3)

