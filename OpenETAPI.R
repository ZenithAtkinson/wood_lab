# Load required library
library(httr)
library(jsonlite)
s
# API key and endpoint
api_key <- "wood-atkinson-key"
endpoint <- "https://openet.dri.edu/data/"

# Define coordinates and parameters
latitude <- 41.0603
longitude <- -112.1014

params <- list(
  coordinates = paste0("[", longitude, ", ", latitude, "]"),
  start_date = "2023-01-01",
  end_date = "2023-12-31",
  variables = "et",
  api_key = api_key
)

# Send GET request
response <- GET(endpoint, query = params)

# Check for successful response
if (status_code(response) == 200) {
  # Convert JSON response to a data frame
  data <- fromJSON(content(response, as = "text"), flatten = TRUE)
  print(data)
  
  # Optional: Save the ET data to a CSV file
  write.csv(data, "OpenET_evapotranspiration_data.csv", row.names = FALSE)
  
} else {
  # Print error message if request fails
  print(paste("Error:", status_code(response), content(response, as = "text")))
}
