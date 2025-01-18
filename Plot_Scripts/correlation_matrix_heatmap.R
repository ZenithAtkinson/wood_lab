library(ggplot2)
library(corrplot)
library(dplyr)

dat_file <- "./Data_Files/CSV_files/Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv"
data <- read.csv(dat_file, header = TRUE, na.strings = c("NAN", "NaN", "NA"), check.names = FALSE)

#View(data)

# remove columns with all NA values
data_cleaned <- data[, colSums(is.na(data)) != nrow(data)]

# remove non-numeric columns (like character columns)
data_cleaned <- data_cleaned %>%
  select_if(~!is.character(.))  # Remove character columns

#convert characters to numeric
data_cleaned <- data_cleaned %>%
  mutate_if(is.character, as.numeric)

#remove columns where standard deviation is zero (all values are the same)
data_cleaned <- data_cleaned %>%
  select_if(function(x) sd(x, na.rm = TRUE) != 0)

#View(data_cleaned)

# Use backticks to select columns with units in their names
data_cleaned <- data_cleaned %>% select(`CO2`, `H2O`, `FC`, `NETRAD`)

# create the correlation matrix, ignore NA values
correlation_matrix <- cor(data_cleaned, use = "pairwise.complete.obs")

# Plot the correlation matrix using corrplot (prints to R console)
corrplot(correlation_matrix, method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         type = "full", addCoef.col = "black", 
         tl.cex = 0.8,       #variable text size
         number.cex = .6)   #plot numbers text size
