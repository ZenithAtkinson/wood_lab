library(tidyverse)
library(lubridate)

# Load UGS data without altering header names
ugs_data <- read.csv("Data_Files/CSV_files/Imputed_AmeriFluxFormat.csv", check.names = FALSE)
ugs_data

# Convert timestamp to POSIXct and extract the date
ugs_data <- ugs_data %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
         date = as.Date(TIMESTAMP))

# Adjust ET values for 30-minute intervals: multiply by 0.5 since it's given as mm/hour
ugs_data <- ugs_data %>%
  mutate(ET_mm = as.numeric(`ET ( mm hour-1 )`) * 0.5)

# Example Plot: 30-minute ET time series
ggplot(ugs_data, aes(x = TIMESTAMP, y = ET_mm)) +
  geom_line(color = "darkgreen") +
  labs(title = "30-Minute ET Time Series", x = "Time", y = "ET (mm per 30 min)") +
  theme_minimal()

# You can also analyze relationships with other variables (e.g., temperature)
ggplot(ugs_data, aes(x = as.POSIXct(TIMESTAMP), y = as.numeric(`TA_1_1_1 ( deg C )`))) +
  geom_line(color = "blue") +
  labs(title = "30-Minute Temperature Time Series", x = "Time", y = "Temperature (°C)") +
  theme_minimal()

#===============================================================================

ugs_data <- read.csv("Data_Files/CSV_files/Cleaned(with units) Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv", check.names = FALSE)
ugs_data

# Convert timestamp to POSIXct and extract the date
ugs_data <- ugs_data %>%
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
         date = as.Date(TIMESTAMP))

# Adjust ET values for 30-minute intervals: multiply by 0.5 since it's given as mm/hour
ugs_data <- ugs_data %>%
  mutate(ET_mm = as.numeric(`ET ( mm hour-1 )`) * 0.5)
ggplot(ugs_data, aes(x = TIMESTAMP, y = ET_mm)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  labs(title = "30-Minute ET Time Series",
       x = "Time", y = "ET (mm per 30 min)") +
  theme_minimal()
View(ugs_data)
