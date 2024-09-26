# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the CSV file (replace with the actual path)
dat_file <- "./Data_Files/CSV_files/Cleaned(with units) Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv"
data <- read.csv(dat_file, header = TRUE)

colnames(data)

# Select relevant columns
# Assuming temperature, ET (evapotranspiration), atmospheric gases (CO2, H2O), solar radiation, and soil water are labeled properly in the CSV
selected_data <- data %>%
  select(TA_1_1_1, TA_1_1_2, ET, CO2, H2O, NETRAD, SW_IN, LW_IN)  # Update column names based on your data structure

# Create multiple plots in one figure
ggplot(data = selected_data, aes(x = TA_1_1_1)) + 
  geom_point(aes(y = ET, color = "ET")) + 
  geom_point(aes(y = CO2, color = "CO2")) +
  geom_point(aes(y = H2O, color = "H2O")) +
  geom_point(aes(y = NETRAD, color = "Net Radiation")) +
  geom_point(aes(y = SW_IN, color = "Solar Radiation (Shortwave)")) +
  geom_point(aes(y = LW_IN, color = "Solar Radiation (Longwave)")) +
  labs(title = "Temperature vs ET, Atmospheric Gases, Solar Radiation, and Soil Water",
       x = "Temperature (°C)",
       y = "Values",
       color = "Variables") +
  theme_minimal()

