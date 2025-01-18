# Install necessary packages (if not already installed)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("GGally")
install.packages("reshape2")

# Load libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(reshape2)

# Load the data (modify the path if needed)
data <- read.csv("./Data_Files/CSV_files/Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv")

# Convert TIMESTAMP_START to a character string, then to a datetime object
data$TIMESTAMP_START <- as.character(data$TIMESTAMP_START)
data$TIMESTAMP_START <- as.POSIXct(data$TIMESTAMP_START, format="%Y%m%d%H%M", tz="UTC")

# Extract the hour and month from the timestamp
data$Hour <- as.numeric(format(data$TIMESTAMP_START, "%H"))
data$Month <- as.numeric(format(data$TIMESTAMP_START, "%m"))

# Ability to filter data by a specific month, e.g., July (month = 7)
data_filtered <- data %>% filter(Month == 7)

# Define day and night periods (6 AM to 6 PM for day, 6 PM to 6 AM for night) 24hr time
data_filtered$Period <- ifelse(data_filtered$Hour >= 6 & data_filtered$Hour < 21, "Day", "Night")

# Filter out rows where CO2 values are missing
data_filtered <- data_filtered %>% filter(!is.na(CO2))

# Select relevant numeric columns for correlation (replace with your actual column names)
# Ensure these variables exist in your dataset

# Subset data for day and night periods
day_data <- data_filtered %>% filter(Period == "Day")
night_data <- data_filtered %>% filter(Period == "Night")

# Adjust based on your actual column names from the dataset
numeric_cols <- c("CO2", "Temperature", "Humidity", "Wind_Speed")  # Replace with actual column names

# Calculate correlation matrices for day and night periods
cor_day <- cor(day_data[, numeric_cols], use = "complete.obs")
cor_night <- cor(night_data[, numeric_cols], use = "complete.obs")

# Melt correlation matrices to long format for visualization
cor_day_melted <- melt(cor_day)
cor_night_melted <- melt(cor_night)

# Plot correlation matrix for the day period
ggplot(cor_day_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  labs(title = "Correlation Matrix (Day)",
       x = "",
       y = "") +
  theme_minimal()

# Plot correlation matrix for the night period
ggplot(cor_night_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  labs(title = "Correlation Matrix (Night)",
       x = "",
       y = "") +
  theme_minimal()

# Calculate the difference in correlations between day and night
cor_diff <- cor_day - cor_night

# Melt the difference matrix to long format
cor_diff_melted <- melt(cor_diff)

# Plot the difference in correlation between day and night
ggplot(cor_diff_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation Difference") +
  labs(title = "Difference in Correlations (Day - Night)",
       x = "",
       y = "") +
  theme_minimal()
