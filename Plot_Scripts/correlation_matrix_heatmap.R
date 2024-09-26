# Load necessary libraries
library(ggplot2)
library(corrplot)
library(dplyr)

# Load the CSV file and prevent automatic renaming of columns
dat_file <- "./Data_Files/CSV_files/Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv"
data <- read.csv(dat_file, header = TRUE, na.strings = c("NAN", "NaN", "NA"), check.names = FALSE)

# View the first few rows of the data (optional)
#View(data)

# Remove columns with all NA values
data_cleaned <- data[, colSums(is.na(data)) != nrow(data)]

# Remove non-numeric columns (like character columns)
data_cleaned <- data_cleaned %>%
  select_if(~!is.character(.))  # Remove character columns

# Convert character columns to numeric if there are any
data_cleaned <- data_cleaned %>%
  mutate_if(is.character, as.numeric)

# Remove columns where standard deviation is zero (all values are the same)
data_cleaned <- data_cleaned %>%
  select_if(function(x) sd(x, na.rm = TRUE) != 0)

#View(data_cleaned)

# Use backticks to select columns with units in their names
data_cleaned <- data_cleaned %>% select(`CO2`, `H2O`, `FC`, `NETRAD`)

# Create the correlation matrix, ignoring NA values
correlation_matrix <- cor(data_cleaned, use = "pairwise.complete.obs")

# Plot the correlation matrix using corrplot (prints to R console)
corrplot(correlation_matrix, method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         type = "full", addCoef.col = "black", 
         tl.cex = 0.8,       # variable text size
         number.cex = 0.3)   # plot numbers text size


ameriflux_dict = <- list(
  "TIMESTAMP_START" = "Start of the measurement period. Units: YYYYMMDDHHMM",
  "TIMESTAMP_END" = "End of the measurement period. Units: YYYYMMDDHHMM",
  "CO2" = "Carbon dioxide mixing ratio. Units: µmolCO2 mol-1",
  "H2O" = "Water vapor mixing ratio. Units: mmolH2O mol-1",
  "FC" = "CO2 flux. Units: µmolCO2 m-2 s-1",
  "NETRAD" = "Net radiation. Units: W m-2",
  "LE" = "Latent heat flux. Units: W m-2",
  "H" = "Sensible heat flux. Units: W m-2",
  "TA" = "Air temperature. Units: °C",
  "PA" = "Air pressure. Units: kPa",
  "RH" = "Relative humidity. Units: %",
  "WS" = "Wind speed. Units: m s-1",
  "WD" = "Wind direction. Units: degrees from north",
  "PPFD_IN" = "Incoming photosynthetically active radiation (PAR). Units: µmol m-2 s-1",
  "PPFD_OUT" = "Outgoing photosynthetically active radiation (PAR). Units: µmol m-2 s-1",
  "G" = "Soil heat flux. Units: W m-2",
  "SW_IN" = "Incoming shortwave radiation. Units: W m-2",
  "SW_OUT" = "Outgoing shortwave radiation. Units: W m-2",
  "LW_IN" = "Incoming longwave radiation. Units: W m-2",
  "LW_OUT" = "Outgoing longwave radiation. Units: W m-2"
)