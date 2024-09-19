library(ggplot2)
library(corrplot)
library(dplyr)

#Load in the csv file (from dat_file_converter)
dat_file <- "./Data_Files/CSV_files/Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv"
data <- read.csv(dat_file, header = TRUE, na.strings = c("NAN", "NaN"))

data_cleaned <- data[, colSums(is.na(data)) != nrow(data)]

data_cleaned <- data_cleaned %>%
  select_if(~!is.character(.))  #remove character columns

#convert to numeric
data_cleaned <- data_cleaned %>%
  mutate_if(is.character, as.numeric)

#remove colums where (standard deviation = 0)
data_cleaned <- data_cleaned %>%
  select_if(function(x) sd(x, na.rm = TRUE) != 0)

#matrix ignoring NA values
correlation_matrix <- cor(data_cleaned, use = "pairwise.complete.obs")

#using corrplot
corrplot(correlation_matrix, method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         type = "full", addCoef.col = "black", 
         tl.cex = 0.8,       # variable text size
         number.cex = 0.3)   # plot numbers text size
