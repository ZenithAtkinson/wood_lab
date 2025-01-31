library(httr)
library(jsonlite)
library(tidyverse)

# Set your API key (replace with your actual key)
Sys.setenv(OPENET_API_KEY = "RfsmpvZYkxj1UZTH45tL1eZUUoElgWjJL7etRMqBEpkHbgaGzTiOPaAMSPqv")

# Define headers EXACTLY as in Python example
headers <- add_headers(
  "Authorization" = Sys.getenv("OPENET_API_KEY"),  # Direct key, no "Bearer"
  "Content-Type" = "application/json"
)

lat_decimal <- 41 + (3/60) + (37/3600)  # ≈ 41.0603
lon_decimal <- -(112 + (6/60) + (5/3600))  # ≈ -112.1014

args <- list(
  date_range = c("2020-01-01", "2023-12-31"),  # Match UGS data timeframe
  interval = "daily",                          # Match UGS temporal resolution
  geometry = c(lon_decimal, lat_decimal),      # UGS coordinates
  model = "Ensemble",
  variable = "ET",
  reference_et = "gridMET",
  units = "mm",
  file_format = "JSON"
)

# Fetch OpenET data
response <- POST(
  "https://openet-api.org/raster/timeseries/point",
  config = headers,
  body = toJSON(args, auto_unbox = TRUE, digits = 7)
)

if (status_code(response) == 200) {
  openet_data <- fromJSON(content(response, "text")) %>%
    mutate(date = as.Date(time)) %>%
    select(date, et_openet = et)
  
  # Add this line to print the data
  print(openet_data)
  
} else {
  cat("Error:", status_code(response), "\n")
  print(content(response, "text"))
}

#-------------------------------------------------------------------------------

