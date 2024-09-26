library(ggplot2)
library(dplyr)

data <- read.csv("./Data_Files/CSV_files/Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv")

# convert TIMESTAMP_START to a character string, then to a datetime object
data$TIMESTAMP_START <- as.character(data$TIMESTAMP_START)
data$TIMESTAMP_START <- as.POSIXct(data$TIMESTAMP_START, format="%Y%m%d%H%M", tz="UTC")

# extract the hour and month from the timestamp
data$Hour <- as.numeric(format(data$TIMESTAMP_START, "%H"))
data$Month <- as.numeric(format(data$TIMESTAMP_START, "%m"))

# Ability to filter data by a specific month, e.g., July (month = 7)
data_filtered <- data %>% filter(Month == 7)

# Define day and night periods (6 AM to 6 PM for day, 6 PM to 6 AM for night) 24hr time
data_filtered$Period <- ifelse(data_filtered$Hour >= 6 & data_filtered$Hour < 18, "Day", "Night")

# filter out rows where CO2 values are missing
data_filtered <- data_filtered %>% filter(!is.na(CO2))

# plot CO2 levels for day and night periods for the filtered month
ggplot(data_filtered, aes(x = TIMESTAMP_START, y = CO2, color = Period)) +
  geom_line() +
  labs(title = "CO2 Levels by Time of Day (Filtered by Month)",
       x = "Time",
       y = "CO2 Levels (ppm)") +
  scale_color_manual(values = c("Day" = "blue", "Night" = "red")) +
  theme_minimal()
