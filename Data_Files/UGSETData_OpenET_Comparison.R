library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

# --- Step 1: Load and Process UGS Data ---
ugs_data <- read.csv("Data_Files/CSV_files/Imputed_AmeriFluxFormat.csv", check.names = FALSE)

# Convert the timestamp column to POSIXct; adjust column name if needed
ugs_data <- ugs_data %>%
  mutate(TIMESTAMP = as.POSIXct(`TIMESTAMP`, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(date = as.Date(TIMESTAMP))

# Aggregate to daily ET:
ugs_daily <- ugs_data %>%
  group_by(date) %>%
  summarise(et_ugs = sum(as.numeric(`ET ( mm hour-1 )`), na.rm = TRUE) * 0.5)

# --- Step 2: Get OpenET Data ---

# Set your API key (replace with your actual key)
Sys.setenv(OPENET_API_KEY = "RfsmpvZYkxj1UZTH45tL1eZUUoElgWjJL7etRMqBEpkHbgaGzTiOPaAMSPqv")

# Define headers EXACTLY as in Python example
headers <- add_headers(
  "Authorization" = Sys.getenv("OPENET_API_KEY"),
  "Content-Type" = "application/json"
)

# Define the geometry and arguments for OpenET request
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
  interval = "daily",                          
  geometry = hexagon_coords,      
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
} else {
  cat("Error:", status_code(response), "\n")
  print(content(response, "text"))
}

# --- Step 3: Merge UGS and OpenET Data for Comparison ---
total_data <- merge(openet_data, ugs_daily, by = "date", all = TRUE)

# Preview the merged data
head(total_data)

# --- Step 4: Plot the Comparison ---
ggplot(total_data, aes(date)) + 
  geom_smooth(aes(y = et_openet, colour = "OpenET")) +
  geom_smooth(aes(y = et_ugs, colour = "UGS")) +
  labs(y = "ET (mm)", x = "Date",
       title = "Figure 1: Comparison of ET from OpenET and UGS",
       subtitle = "Daily values (UGS ET converted from mm/hour to mm/day)") +
  theme_minimal()

# --- Step 5: Compute Comparison Statistics ---
comparison <- total_data %>%
  filter(!is.na(et_ugs) & !is.na(et_openet)) %>%  # Focus on overlapping dates
  summarise(
    rmse = sqrt(mean((et_openet - et_ugs)^2)),
    bias = mean(et_openet - et_ugs),
    r_squared = cor(et_openet, et_ugs)^2
  )

print(comparison)

#=== Writing OpenET data into a file: ====
openet_data
write.csv()

#=============================================================================
# COMPARISONS: Doing different kidns of plots:
#Scatterplot with Regression Line:

# Fit the linear model
lm_model <- lm(et_openet ~ et_ugs, data = total_data)

# Extract coefficients
coefs <- coef(lm_model)
eq_text <- paste0("y = ", round(coefs[1], 2), " + ", round(coefs[2], 2), "x")

# Extract R-squared
r_squared <- summary(lm_model)$r.squared
r2_text <- paste("R² =", round(r_squared, 2))

# Extract p-value
p_value <- summary(lm_model)$coefficients[2, 4]  # p-value for slope coefficient
p_text <- paste("p =", format.pval(p_value, digits = 3, scientific = TRUE))

# Combine into annotation text
annotation_text <- paste(eq_text, r2_text, p_text, sep = "\n")

# Scatterplot with Regression Line and Statistics
ggplot(total_data, aes(x = et_ugs, y = et_openet)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  annotate("text", 
           x = min(total_data$et_ugs, na.rm = TRUE), 
           y = max(total_data$et_openet, na.rm = TRUE), 
           label = annotation_text,
           hjust = 0, vjust = 1, size = 5, color = "blue") +
  labs(
    x = "UGS Daily ET (mm)", 
    y = "OpenET Daily ET (mm)",
    title = "Figure 2: Regression of OpenET vs. UGS Daily ET"
  ) +
  theme_minimal()

#==============================================================================
#Bland-altman plot with percent differences

#Bland–Altman Plot
# Compute mean and difference for each day
total_data <- total_data %>% 
  mutate(mean_et = (et_ugs + et_openet) / 2,
         diff_et = et_openet - et_ugs)

ggplot(total_data, aes(x = mean_et, y = diff_et)) + 
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = mean(total_data$diff_et), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(total_data$diff_et) + 1.96 * sd(total_data$diff_et), color = "blue", linetype = "dashed") +
  geom_hline(yintercept = mean(total_data$diff_et) - 1.96 * sd(total_data$diff_et), color = "blue", linetype = "dashed") +
  geom_hline(yintercept = mean(total_data$diff_et, na.rm = TRUE), color = "red", linetype = "dashed", size = 1.2) +
  labs(
    x = "Mean ET (mm) [Average of UGS & OpenET]",
    y = "Difference in ET (mm) [OpenET - UGS]",
    title = "Bland–Altman Plot: UGS vs. OpenET Daily ET"
  ) +
  theme_minimal()

mean_diff <- mean(total_data$diff_et, na.rm = TRUE)
print(mean_diff) # Gives -0.25, so on average, OpenET is -.025mm less than UGS data

avg_ugs <- mean(total_data$et_ugs, na.rm = TRUE)
percentage_diff <- abs(mean_diff) / avg_ugs * 100
print(percentage_diff) #GLOBAL percentage difference (7.7)

# Calculate how often OpenET is >5% lower/higher than UGS
total_data %>% 
  mutate(relative_diff = (et_openet - et_ugs) / et_ugs * 100) %>% 
  summarise(
    percent_5pct_lower = mean(relative_diff < -5, na.rm = TRUE) * 100,
    percent_5pct_higher = mean(relative_diff > 5, na.rm = TRUE) * 100
  )

#=================================================================================
#Histogram of daily relative difference, with mean value:
# Filter out values above the 95th percentile
threshold <- quantile(total_data$relative_diff, 0.95, na.rm = TRUE)
filtered_data <- total_data %>% filter(relative_diff <= threshold)

# Calculate the mean for the filtered data
mean_filtered <- mean(filtered_data$relative_diff, na.rm = TRUE)
print(mean_filtered)

# Plot the histogram for the filtered data and add a vertical line at the filtered mean
ggplot(filtered_data, aes(x = relative_diff)) +
  geom_histogram(binwidth = 5, fill = "darkorange", color = "black") +
  geom_vline(xintercept = mean_filtered, color = "blue", linetype = "dashed", size = 1) +
  labs(x = "Relative Difference (%)", y = "Count", 
       title = "Distribution of Daily Relative Differences (OpenET vs UGS)",
       subtitle = paste("Filtered Mean Relative Difference =", round(mean_filtered, 2), "%")) +
  theme_minimal()

#==============================================================================
# Monthly daily average between UGS and OpenET

#Monthly Breakdown of ET Values:
# Create a month variable
total_data <- total_data %>%
  mutate(month = floor_date(date, "month"))

# Aggregate to monthly averages for both datasets
monthly_data <- total_data %>%
  group_by(month) %>%
  summarise(
    avg_et_ugs = mean(et_ugs, na.rm = TRUE),
    avg_et_openet = mean(et_openet, na.rm = TRUE)
  )

ggplot(monthly_data, aes(x = month)) +
  geom_line(aes(y = avg_et_ugs, color = "UGS")) +
  geom_line(aes(y = avg_et_openet, color = "OpenET")) +
  labs(
    x = "Month",
    y = "Average Daily ET (mm)",
    title = "Monthly Average Daily ET: UGS vs. OpenET"
  ) +
  scale_color_manual(values = c("UGS" = "black", "OpenET" = "darkorange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



