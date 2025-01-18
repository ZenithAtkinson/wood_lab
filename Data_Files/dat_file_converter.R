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
dat_file <- "./Data_Files/OG_dat_files/Great Salt Lake Phragmites_Flux_AmeriFluxFormat.dat"

data <- read.csv(dat_file, header = FALSE, na.strings = c("NAN", "NaN", "NA"))

# assign headers
names(data) <- data[2, ]  #second row to header

# Remove the first, second, and third rows (since the first two rows are metadata and headers)
data <- data[-c(1, 2, 3, 4), ]

# replace mepty col names with valid names
colnames(data)[colnames(data) == ""] <- paste0("Unnamed_", seq_len(sum(colnames(data) == "")))

#View(data)

# chacter to numeric
data <- data %>% mutate_if(is.character, as.numeric)

# remove columns with all NA values
data_cleaned <- data[, colSums(is.na(data)) != nrow(data)]
View(data_cleaned)

write.csv(data_cleaned, "Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv", row.names = FALSE)

# Confirmation message
print("Converted and cleaned csv data saved as 'Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv'.")
