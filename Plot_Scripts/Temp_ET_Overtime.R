# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data
data <- read.csv("./Data_Files/CSV_files/Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv")

# Convert TIMESTAMP_START to a datetime object
data$TIMESTAMP_START <- as.POSIXct(as.character(data$TIMESTAMP_START), format="%Y%m%d%H%M", tz="UTC")

# Select only relevant columns for plotting
# Replace TA_1_1_1 with the actual column name for temperature if different
selected_data <- data %>%
  select(TIMESTAMP_START, Temperature = TA_1_1_1, ET)

# Filter out rows with missing ET or Temperature values if any
selected_data <- selected_data %>% filter(!is.na(ET), !is.na(Temperature))

# Plot ET and Temperature over Time
ggplot(data = selected_data, aes(x = TIMESTAMP_START)) +
  geom_line(aes(y = ET, color = "ET"), size = 1) +
  geom_line(aes(y = Temperature, color = "Temperature"), size = .7) +
  labs(
    title = "ET and Temperature Over Time",
    x = "Time",
    y = "Values",
    color = "Variables"
  ) +
  theme_minimal()

# ----------------------- Alternative way of displaying it:

# Select relevant columns and pivot to long format
# Replace TA_1_1_1 with the actual temperature column name if it's different
selected_data <- data %>%
  select(TIMESTAMP_START, Temperature = TA_1_1_1, ET) %>%
  pivot_longer(cols = -TIMESTAMP_START, names_to = "name", values_to = "value")

# Plotting
ggplot(data = selected_data, aes(x = TIMESTAMP_START, y = value, color = name)) +
  geom_line(size = 1) +
  scale_y_continuous(name = "concentration") +
  scale_color_manual(NULL, values = c("ET" = "#F8766D", "Temperature" = "#00BFC4")) +
  facet_grid(name ~ ., scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(title = "ET and Temperature Over Time", x = "Time")

