#rdb --command json mydump.rdb > dump.json

# Load necessary libraries
library(jsonlite)

# Step 1: Load JSON data
json_data <- fromJSON("dump.json")

# Step 2: Convert JSON to a data frame
# Assuming the JSON is a key-value structure
data_frame <- data.frame(
  Key = names(json_data),
  Value = unlist(json_data),
  stringsAsFactors = FALSE
)

# Step 3: Write data to a CSV file
write.csv(data_frame, "output.csv", row.names = FALSE)

# Confirmation
cat("CSV file has been created: output.csv")
  
#OR ---------------------------------------------------------------------------------
# Install redux
install.packages("redux")

# Load library
library(redux)

# Connect to Redis
redis <- redux::hiredis()

# Fetch all keys
keys <- redis$KEYS("*")

# Fetch values for keys
values <- lapply(keys, redis$GET)

# Create a data frame
data_frame <- data.frame(
  Key = keys,
  Value = unlist(values),
  stringsAsFactors = FALSE
)

# Write to CSV
write.csv(data_frame, "redis_output.csv", row.names = FALSE)

# Confirmation
cat("CSV file has been created: redis_output.csv")

#DOWNLOADING FILE FROM INTERNET: -------------------------------------------------------------------

install.packages("httr")
library(httr)

# URL for the file with format adjusted (replace "html" with "rdb" or "csv" if supported)
url <- "https://nwis.waterservices.usgs.gov/nwis/iv/?sites=10163000&agencyCd=USGS&startDT=2024-01-22T12:35:33.048-07:00&endDT=2025-01-21T12:35:33.048-07:00&parameterCd=00065&format=rdb"

# Download the data
response <- GET(url)

# Save to file
writeBin(content(response, "raw"), "./Data_Files/usgs_data.rdb")

cat("File downloaded as usgs_data.rdb")
