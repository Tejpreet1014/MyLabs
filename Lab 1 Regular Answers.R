#Create a sequence of numbers from 1 to 500 with an interval of 5
sequence <- seq(1, 500, by = 5)

# Calculate the mean and round to 1 decimal place
mean_sequence <- round(mean(sequence), 1)
mean_sequence
# Create a vector that repeats the sequence (1, 2, 3) ten times
vector <- rep(c(1, 2, 3), times = 10)

# Calculate the variance and round to 2 decimal places
variance_vector <- round(var(vector), 2)
variance_vector
# Create a sequence of numbers from 1 to 500 with interval of 5.5
seq_numbers <- seq(1, 500, by = 5.5)

# Calculate the 75th percentile
percentile_75 <- quantile(seq_numbers, 0.75)

# Round the result to nearest 2 decimals
round(percentile_75, 2)

# Step 1: Create the vector by repeating the sequence (1, 2, 3) ten times
repeated_vector <- rep(c(1, 2, 3), times = 10)

# Step 2: Calculate the variance of the vector
variance_value <- var(repeated_vector)

# Step 3: Round the result to two decimal places
variance_rounded <- round(variance_value, 2)

# Step 4: Print the result
print(variance_rounded)


# Step 1: Create the first vector (1, 2, 3) repeated ten times
v1 <- rep(c(1, 2, 3), times = 10)

# Step 2: Create the second vector (4, 5, 6) repeated ten times
v2 <- rep(c(4, 5, 6), times = 10)

# Step 3: Concatenate the two vectors
v_combined <- c(v1, v2)

# Step 4: Calculate the standard deviation of the resulting vector
std_dev <- sd(v_combined)

# Step 5: Round the result to two decimal places
std_dev_rounded <- round(std_dev, 2)

# Step 6: Print the result
print(std_dev_rounded)

# Step 1: Create the sequence from 1 to 500 with an interval of 5.5
sequence <- seq(from = 1, to = 500, by = 5.5)

# Step 2: Calculate the 75th percentile
percentile_75 <- quantile(sequence, 0.75)

# Step 3: Round the result to two decimal places
percentile_75_rounded <- round(percentile_75, 2)

# Step 4: Print the result
print(percentile_75_rounded)