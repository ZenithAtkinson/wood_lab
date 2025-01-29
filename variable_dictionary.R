# Load necessary libraries
library(dplyr)
library(stringr)

# Load the existing data file and AmeriFlux file (replace with your actual file paths)
existing_data_file = './Data_Files/CSV_files/Cleaned Great Salt Lake Phragmites_Flux_AmeriFluxFormat.csv'
ameriflux_file = './Data_Files/CSV_files/flux_data_variables.csv'

# Read the CSV files
existing_data <- read.csv(existing_data_file, header = TRUE)
ameriflux_data <- read.csv(ameriflux_file, header = TRUE)

# Extract the headers from the existing data
existing_headers <- colnames(existing_data)

# Create a dictionary from the AmeriFlux data where the first column is the variable name and the second is the description
# Clean both the AmeriFlux and existing headers by removing spaces and converting to lowercase
clean_ameriflux_dict <- setNames(
  as.list(ameriflux_data[[3]]),
  str_trim(tolower(ameriflux_data[[2]]))
)

# Match the headers from the existing data with descriptions from AmeriFlux (case-insensitive and ignore spaces)
matching_headers_dict <- sapply(existing_headers, function(header) {
  cleaned_header <- str_trim(tolower(header))  # Clean the header name
  if (cleaned_header %in% names(clean_ameriflux_dict)) {
    clean_ameriflux_dict[[cleaned_header]]
  } else {
    "No description available"
  }
})

# Print the resulting dictionary
matching_headers_dict
View(matching_headers_dict)
