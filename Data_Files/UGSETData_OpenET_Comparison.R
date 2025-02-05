library(tidyverse)
library(lubridate)

# Read the TOA5 file correctly --------------------------------------------
dat_file <- "./Data_Files/OG_dat_files/Great Salt Lake Phragmites_Flux_AmeriFluxFormat (2024 data).dat" 
#Data has values missing in the MAR format (Missing At Random). See Alsaber, Ahmad R. et al.
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

ugs_daily <- df %>%
  mutate(
    TIMESTAMP = ymd_hms(str_trim(.data[[timestamp_col]])),  
    ET = as.numeric(.data[[et_col]])
  ) %>%
  filter(!is.na(TIMESTAMP) & !is.na(ET)) %>%
  mutate(date = as.Date(TIMESTAMP)) %>%
  group_by(date) %>%
  summarise(et_ugs = sum(ET, na.rm = TRUE))  # Sum of ET for each day

ugs_daily <- data.frame(ugs_daily)

# Test to see if the daily value is being correctly calculated. This was for all the values in a given day, and it added up perfectly.
#etcol = df$`ET ( mm hour-1 )`
#nums = as.numeric(etcol[5781:5806])
#sum(nums) #=0.6747583



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

#lat_decimal <- 41 + (3/60) + (37/3600)  # ≈ 41.0603
#lon_decimal <- -(112 + (6/60) + (5/3600))  # ≈ -112.1014 

# Forming a regular hexagon where each point is 30.4m/100ft from the center point, the center point being the UGS site.
hexagon_coords <- c(
  -112.101389, 41.060552,
  -112.101075, 41.060415,
  -112.101075, 41.060141,
  -112.101389, 41.060003,
  -112.101703, 41.060141,
  -112.101703, 41.060415,
  -112.101389, 41.060552  # Close the polygon
)

args <- list(
  date_range = c("2024-01-01", "2025-01-31"),  # Match UGS data timeframe
  interval = "daily",                          # Match UGS temporal resolution
  geometry = hexagon_coords,      # UGS coordinates
  reducer = "mean",
  model = "ensemble",
  variable = "et",
  reference_et = "gridMET",
  units = "mm",
  file_format = "JSON"
)

# Fetch OpenET data
response <- POST(
  "https://openet-api.org/raster/timeseries/polygon",
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
ugs_daily # in mm hour-1
open_cp = openet_data
ugs_cp = ugs_daily

total_data <- merge(open_cp, ugs_cp, by.x = "date", all = TRUE)
total_data

ggplot(total_data, aes(date)) + 
  geom_smooth(aes(y = et_openet, colour = "OpenET")) +
  geom_smooth(aes(y = et_ugs, colour = "UGS")) +
  labs(y = "ET in mm")
# If validated, this could highlight OpenET’s challenges in ecosystems dominated by invasive species like Phragmites.
comparison <- total_data %>%
  filter(!is.na(et_ugs)) %>%  # Focus on overlapping dates
  summarise(
    rmse = sqrt(mean((et_openet - et_ugs)^2)),
    bias = mean(et_openet - et_ugs),
    r_squared = cor(et_openet, et_ugs)^2
  )

print(comparison)
