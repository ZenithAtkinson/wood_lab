library(tidyverse)
library(lubridate)

# Read the TOA5 file correctly --------------------------------------------
dat_file <- "./Data_Files/OG_dat_files/Great Salt Lake Phragmites_Flux_AmeriFluxFormat.dat"
df <- read.csv(dat_file, header = FALSE, na.strings = c("NAN", "NaN", "NA"))

colnames(df) <- as.character(unlist(df[2, ]))

df <- df[-c(1, 2, 4), ]

rownames(df) <- NULL

current_colnames <- colnames(df)
units <- as.character(unlist(df[1, ]))

df = df[-1,]

# Append units to column names
colnames(df) <- ifelse(
  units != "",
  paste(current_colnames, "(", units, ")"),  
  current_colnames
)

df <- df %>% 
  select(where(~!all(is.na(.x))))  # Keep only columns with data

# Dynamically identify target columns -------------------------------------
timestamp_col <- grep("TIMESTAMP", colnames(df), value = TRUE)[1]  # Ensure single match
et_col <- grep("ET", colnames(df), value = TRUE)[1]

# Check if the columns exist
if (is.na(timestamp_col) || is.na(et_col)) {
  stop("TIMESTAMP or ET column not found. Check column names!")
}

# Process timestamp and ET ------------------------------------------------
df_hourly <- df %>%
  mutate(
    # Convert TIMESTAMP using the correct parser
    TIMESTAMP = ymd_hms(str_trim(.data[[timestamp_col]])),
    
    # Convert ET to numeric (using dynamic column reference)
    ET = as.numeric(.data[[et_col]]),
    
    # Create hourly time column by rounding down to the nearest hour
    hour = floor_date(TIMESTAMP, unit = "hour")
  ) %>%
  group_by(hour) %>%
  summarise(
    et_ugs = sum(ET, na.rm = TRUE)  # Hourly total in mm
  )

# Convert hourly UGS data to daily totals
ugs_daily <- df_hourly %>%
  mutate(date = as.Date(hour)) %>%
  group_by(date) %>%
  summarise(et_ugs = sum(et_ugs, na.rm = TRUE))

ugs_daily

#=========================================================================================

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
  #print(openet_data)
  
} else {
  cat("Error:", status_code(response), "\n")
  print(content(response, "text"))
}



#-------------------------------------------------------------------------------
openet_data
ugs_daily
