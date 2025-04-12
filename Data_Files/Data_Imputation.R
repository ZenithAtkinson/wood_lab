library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(naniar)
library(imputeTS)

# Read the TOA5 file correctly --------------------------------------------
dat_file <- "./Data_Files/OG_dat_files/Great Salt Lake Phragmites_Flux_AmeriFluxFormat (2024 data).dat" 
#Data has values missing in the MAR format (Missing At Random). See Alsaber, Ahmad R. et al.
df <- read.csv(dat_file, header = FALSE, na.strings = c("NAN", "NaN", "NA"))

# Set column names from row 2
colnames(df) <- as.character(unlist(df[2, ]))

# Remove unnecessary rows
df <- df[-c(1, 2, 4), ]
rownames(df) <- NULL

# Extract units for column names
current_colnames <- colnames(df)
units <- as.character(unlist(df[1, ]))
df <- df[-1, ]  # Remove units row from the data

# Append units to column names
colnames(df) <- ifelse(
  units != "",
  paste(current_colnames, "(", units, ")"),  
  current_colnames
)

# Convert TIMESTAMP ( TS ) to POSIXct safely
if ("TIMESTAMP ( TS )" %in% colnames(df)) {
  df$`TIMESTAMP ( TS )` <- ymd_hms(df$`TIMESTAMP ( TS )`, quiet = TRUE)
}

# Store original column names before filtering
original_cols <- colnames(df)

# Remove columns that are entirely NA
df <- df %>%
  select(where(~ !all(is.na(.))))  # Keep only columns with some data

# Identify and print removed columns
removed_cols <- setdiff(original_cols, colnames(df))
if (length(removed_cols) > 0) {
  print(paste("Removed columns:", paste(removed_cols, collapse = ", ")))
} else {
  print("No columns were removed.")
}

# Convert numeric-like character columns to numeric while keeping non-numeric columns intact
df <- df %>%
  mutate(across(
    where(~ is.character(.x) & !all(grepl("[^0-9.-]", .x, perl = TRUE))), 
    ~ as.numeric(.x)
  ))

# Remove columns that are now entirely NA after conversion
df <- df %>%
  select(where(~ sum(!is.na(.)) > 0))

# Identify and print columns removed after numeric conversion
removed_cols_after_conversion <- setdiff(original_cols, colnames(df))
if (length(removed_cols_after_conversion) > 0) {
  print(paste("Removed after conversion:", paste(removed_cols_after_conversion, collapse = ", ")))
} else {
  print("No additional columns removed after conversion.")
}

str(df)

# Code for looking at all timestamp values, to see if there are gaps
ugs_data <- df %>%
  mutate(`TIMESTAMP ( TS )` = as.POSIXct(`TIMESTAMP ( TS )`, format = "%Y-%m-%d %H:%M:%S"),
         date = as.Date(`TIMESTAMP ( TS )`))
# Adjust ET values for 30-minute intervals: multiply by 0.5 since its given as mm/hour
ugs_data <- ugs_data %>%
  mutate(ET_mm = as.numeric(`ET ( mm hour-1 )`) * 0.5)
ggplot(ugs_data, aes(x = `TIMESTAMP ( TS )`, y = ET_mm)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  labs(title = "30-Minute ET Time Series",
       x = "Time", y = "ET (mm per 30 min)") +
  theme_minimal()
View(ugs_data)


#===============================================================================
# PERCENT of NA values:
# Function to calculate NA percentages for all columns
na_percentages <- function(df) {
  col_na_percents <- sapply(df, function(col) sum(is.na(col)) / nrow(df) * 100)
  return(as.list(col_na_percents))
}

na_percentages(df)

gg_miss_var(df)
length(df$`TIMESTAMP ( TS )`)

missing_percent <- colMeans(is.na(df)) * 100
missing_percent_sorted <- sort(missing_percent, decreasing = TRUE)
print(missing_percent_sorted)

summary(is.na(df))
#View(df)
#================================================================

# PLOTTING MSISING COLUMNS
#Converting to time/date format
df$`TIMESTAMP ( TS )` <- ymd_hms(df$`TIMESTAMP ( TS )`)

colnames(df)

df$`FCH4 ( nmolCH4 m-2 s-1 )` <- as.numeric(df$`FCH4 ( nmolCH4 m-2 s-1 )`)
df$`CH4 ( nmolCH4 mol-1 )` <- as.numeric(df$`CH4 ( nmolCH4 mol-1 )`)
df$`H2O_SIGMA ( mmolH2O mol-1 )` <- as.numeric(df$`H2O_SIGMA ( mmolH2O mol-1 )`)


# Plot FCH4 and CH4 over time
ggplot(df, aes(x = `TIMESTAMP ( TS )`)) +
  geom_line(aes(y = `H2O_SIGMA ( mmolH2O mol-1 )`, color = "FCH4"), size = 1) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +  # Auto-adjusts spacing
  labs(title = "FCH4 and CH4 Trends Over Time",
       x = "Timestamp",
       y = "Concentration",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotates x-axis labels
        legend.position = "top")

#============================================================================
# For given data, drop anything over 50% missing.

threshold <- 50
na_percent_col <- colMeans(is.na(df)) * 100

df_clean <- df %>% select(which(na_percent_col < threshold))

dropped_columns <- names(df)[which(na_percent_col >= threshold)]
print(paste("Dropped columns:", paste(dropped_columns, collapse = ", ")))

str(df_clean)
#=============================================================================
# Conducting imputation with missForest algorithm (see Alsaber et. al)
library(missForest)
library(doParallel)

# Detect and register parallel cores
num_cores <- detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

df_imputed_ms <- missForest(df_clean %>% select(-`TIMESTAMP ( TS )`), #With background processes: took 4 iterations, total time of 9.5 minutes
                            parallelize = "variables",
                            verbose = TRUE)

stopCluster(cl)

# Extract imputed data
df_clean_imputed_ms <- df_imputed_ms$ximp 

# Add timestamp back in
df_clean_imputed_ms$`TIMESTAMP ( TS )` <- df_clean$`TIMESTAMP ( TS )`

# Check imputation performance
print(df_imputed_ms$OOBerror)  # Out-of-bag error estimates
head(df_clean_imputed_ms)
df_clean_imputed_ms
#write.csv(df_clean_imputed_ms, "/Users/zenit/Downloads/imputed_data.csv")
#===========================================================================
# PLOTTING: Missing timestamp values (should be about 3300)

# Generate complete reference timeline (30-minute intervals)
full_seq <- data.frame(
  TIMESTAMP = seq.POSIXt(
    from = as.POSIXct("2024-07-02 21:30:00"),
    to = as.POSIXct("2025-01-31 12:30:00"),
    by = "30 min"
  )
)

colnames(df)

# Create completeness indicator (1 = exists, 0 = missing)
df_fulltime <- df %>%  # Replace 'your_data' with your actual dataframe
  mutate(TIMESTAMP = as.POSIXct(`TIMESTAMP ( TS )`)) %>%
  right_join(full_seq, by = "TIMESTAMP") %>%
  mutate(data_present = ifelse(!is.na(`RECORD ( RN )`), 1, 0))

ggplot(df_fulltime, aes(x = TIMESTAMP, y = data_present)) +
  geom_point(
    size = 0.3, 
    color = "#2c7bb6", 
    alpha = 0.7,
    shape = "|"  # Vertical line markers for better time-series visibility
  ) +
  scale_y_continuous(
    breaks = c(0, 1), 
    labels = c("Missing", "Present"),
    limits = c(-0.2, 1.2)
  ) +
  labs(
    title = "Data Completeness: 2024-07-02 to 2025-01-31",
    subtitle = "30-minute resolution data",  # Add context
    x = "Date", 
    y = "Data Status",
    caption = "Missing data shown at y=0"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Improve readability
    plot.title.position = "plot",  # Left-align title
    panel.grid.major.x = element_line(color = "grey90")  # Vertical grid lines
  )

#==============================================================================
###############################################################################
# Step 1: Prepare Your Existing Imputed Data
###############################################################################
df_imputed <- df_clean_imputed_ms %>% 
  rename(TIMESTAMP = `TIMESTAMP ( TS )`)

###############################################################################
# Step 2: Generate a Complete Timestamp Sequence
###############################################################################
full_seq <- data.frame(
  TIMESTAMP = seq.POSIXt(
    from = as.POSIXct("2024-07-02 21:30:00"),
    to   = as.POSIXct("2025-01-31 12:30:00"),
    by   = "30 min"
  )
)

###############################################################################
# Step 3: Merge the Complete Sequence with Your Imputed Data
###############################################################################
df_fulltime <- full_seq %>% 
  left_join(df_imputed, by = "TIMESTAMP")

###############################################################################
# Step 4: Impute the Newly Missing Values using missForest (Parallelized)
###############################################################################
library(missForest)
library(doParallel)

# Register a parallel backend:
num_cores <- parallel::detectCores() - 1  # Use all but one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Select only numeric or factor columns to impute (exclude TIMESTAMP)
df_to_impute <- df_fulltime %>%
  select(where(is.numeric) | where(is.factor))

# Run missForest to impute missing values in parallel
imputed_result <- missForest(df_to_impute, parallelize = "variables", verbose = TRUE)
df_imputed_mf <- imputed_result$ximp

# Stop the parallel cluster after imputation
stopCluster(cl)

###############################################################################
# Step 5: Reattach TIMESTAMP and Create an Indicator Column (Optional)
###############################################################################
df_fulltime_imputed <- df_fulltime %>% 
  select(TIMESTAMP) %>% 
  bind_cols(df_imputed_mf)

df_fulltime_imputed <- df_fulltime_imputed %>%
  mutate(data_present = ifelse(!is.na(df_fulltime$`RECORD ( RN )`), 1, 0))

###############################################################################
# Step 6: Visualize Data Completeness
###############################################################################
library(ggplot2)
ggplot(df_fulltime_imputed, aes(x = TIMESTAMP, y = data_present)) +
  geom_point(
    size = 0.3, 
    color = "#2c7bb6", 
    alpha = 0.7,
    shape = "|"  
  ) +
  scale_y_continuous(
    breaks = c(0, 1), 
    labels = c("Missing", "Present"),
    limits = c(-0.2, 1.2)
  ) +
  labs(
    title = "Data Completeness: 2024-07-02 to 2025-01-31",
    subtitle = "30-minute resolution data",  
    x = "Date", 
    y = "Data Status",
    caption = "Missing data shown at y=0"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title.position = "plot",
    panel.grid.major.x = element_line(color = "grey90")
  )

###############################################################################
# Step 7: Save Final Imputed Data
###############################################################################
write.csv(df_fulltime_imputed, "./Data_Files/CSV_files/Imputed_AmeriFluxFormat.csv", row.names = FALSE)
