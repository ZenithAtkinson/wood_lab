#(only run this once if not already installed)
#install.packages("DBI")
#install.packages("Rtools")
#install.packages("dplyr")
#install.packages("corrplot")
#install.packages("ggplot2")

library(corrplot)
library(DBI)
library(dplyr)
library(ggplot2)
#library(RSQLite)

#Original data file, modify per file (implement command line arguments?)

dat_file <- "./Data_Files/OG_dat_files/Great Salt Lake Phragmites_Flux_CSFormat.dat"
dat_file <- "/Users/zenit/Downloads/Great Salt Lake Phragmites_Flux_AmeriFluxFormat.dat"
data <- read.csv(dat_file, header = FALSE, na.strings = c("NAN", "NaN", "NA")) #This removes the columns "G", "SG", and 1 other (not sure which one)
View(data)
#preview the first few rows of the data
#print(head(data))
#print(typeof(data))


#TO: Select specific rows
#filtered_data <- data %>%
#  slice(1:10)

#TO: Select specific columns(need to replace col names)
#selected_columns <- data %>%
#  select(column1, column2)

names(data) <- data[2,]
units <- as.character(unlist(data[3,])) # Store units from row 3

# Append units to each corresponding column name
names(data) <- mapply(function(name, unit) paste(name, "(", unit, ")", sep = ""), names(data), units)

data <- data[-(1:3), ] # Remove the first three rows (headers and units)
View(data)
#convert to numeric (for plots)
data <- data %>% mutate_if(is.character, as.numeric)

#remove all NA vals
data_cleaned <- data[, colSums(is.na(data)) != nrow(data)]

View(data_cleaned)
write.csv(data_cleaned, "Cleaned(with units) Great Salt Lake Phragmites_Flux_CSFormat.csv", row.names = FALSE)

print("Converted and cleaned csv data saved as 'Cleaned(with units) Great Salt Lake Phragmites_Flux_CSFormat.csv'.")
