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

dat_file <- "./Data_Files/OG_dat_files/Great Salt Lake Phragmites_Flux_AmeriFluxFormat.dat"
data <- read.csv(dat_file, header = FALSE) #This removes the columns "G", "SG", and 1 other (not sure which one)
View(data)
#preview the first few rows of the data
#print(head(data))
#print(typeof(data))


#Select specific rows
#filtered_data <- data %>%
#  slice(1:10)

#select specific columns(need to replace col names)
#selected_columns <- data %>%
#  select(column1, column2)

#Extract the header (column names) and units (first two rows)
headers <- data[1, ]  #first row for headers
units <- data[2, ]    #second row for units
View(data)

#combine the headers with the unit values like this: Header name(unit)
new_units <- paste(, units, sep = " (")
print(new_units)
new_headers <- paste(new_headers, ")", sep = "")

#assign the new headers to the data and remove the first two rows (headers and units)
data <- data[-(1:2), ]
colnames(data) <- new_headers
View(data)

#convert to numeric (for plots)
data <- data %>% mutate_if(is.character, as.numeric)
View(data)
#remove all NA vals
data_cleaned <- data[, colSums(is.na(data)) != nrow(data)]

View(data_cleaned)
write.csv(data_cleaned, "Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv", row.names = FALSE)

print("Converted and cleaned csv data saved as 'Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv'.")
