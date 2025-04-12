library(dplyr)

# Read in your CSV (adjust the file name/path as needed)
data <- read.csv("./Data_Files/CSV_files/Imputed_AmeriFluxFormat.csv", stringsAsFactors = FALSE)

# Convert your timestamp column to a proper datetime format.
# Replace 'timestamp' with the actual column name.
data$TIMESTAMP <- as.POSIXct(data$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")

# Create a date column to group by day.
data$date <- as.Date(data$TIMESTAMP)

# Calculate ET in mm for each 30-minute period (mm/hr * 0.5)
data <- data %>% mutate(ET_mm = ET...mm.hour.1.. * 0.5)  # replace 'ET_value' with the correct column name

# Group by date and calculate:
# - Total ET for the day in mm
# - Average instantaneous ET rate (mm/hr) for the day
# - Average ET per hour over the day (total ET / 24)
daily_stats <- data %>%
  group_by(date) %>%
  summarise(
    daily_total_ET = sum(ET_mm, na.rm = TRUE),
    daily_avg_instantaneous = mean(ET...mm.hour.1.., na.rm = TRUE),
    daily_avg_ET_per_hour = daily_total_ET / 24
  )

print(daily_stats)
last(daily_stats)
