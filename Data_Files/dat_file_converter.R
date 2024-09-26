# Install necessary packages if not already installed (only run once)
# install.packages("DBI")
# install.packages("dplyr")
# install.packages("corrplot")
# install.packages("ggplot2")

# Load necessary libraries
library(corrplot)
library(DBI)
library(dplyr)
library(ggplot2)

# Original data file (modify if necessary)
dat_file <- "./Data_Files/OG_dat_files/Great Salt Lake Phragmites_Flux_AmeriFluxFormat.dat"

# Load the CSV file without adding units to column names
data <- read.csv(dat_file, header = FALSE, na.strings = c("NAN", "NaN", "NA"))

# Assign the headers from the second row (the original headers)
names(data) <- data[2, ]  # Second row becomes the header

# Remove the first, second, and third rows (since the first two rows are metadata and headers)
data <- data[-c(1, 2, 3, 4), ]

# Ensure no empty column names exist by replacing them with valid names
colnames(data)[colnames(data) == ""] <- paste0("Unnamed_", seq_len(sum(colnames(data) == "")))

# View the data (optional)
View(data)

# Convert character columns to numeric for analysis
data <- data %>% mutate_if(is.character, as.numeric)

# Remove columns with all NA values
data_cleaned <- data[, colSums(is.na(data)) != nrow(data)]

# View the cleaned data (optional)
View(data_cleaned)

# Save the cleaned data to a CSV file
write.csv(data_cleaned, "Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv", row.names = FALSE)

# Confirmation message
print("Converted and cleaned csv data saved as 'Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv'.")
