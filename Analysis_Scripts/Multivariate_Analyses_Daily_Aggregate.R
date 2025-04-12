library(tidyverse)
library(lubridate)

colnames(ugs_data)

# Assuming ugs_data has been processed as above:
ugs_daily <- ugs_data %>%
  group_by(date) %>%
  summarise(
    daily_total_ET = sum(ET_mm, na.rm = TRUE),                    # Total daily ET in mm
    daily_avg_ET = mean(as.numeric(`ET ( mm hour-1 )`), na.rm = TRUE), # Average instantaneous ET (mm/hour)
    avg_temp = mean(as.numeric(`TA_1_1_1 ( deg C )`), na.rm = TRUE),    # Daily average temperature
    avg_rh = mean(as.numeric(`RH_1_1_1 (%)`), na.rm = TRUE)              # Daily average RH
  )

# Create a dummy variable for Phragmites presence (for example, higher presence in summer)
ugs_daily <- ugs_daily %>%
  mutate(phrag_presence = if_else(month(date) %in% c(7,8,9), 1, 0))

# Example Plot: Daily ET over time
ggplot(ugs_daily, aes(x = date, y = daily_total_ET)) +
  geom_line(color = "darkgreen") +
  labs(title = "Daily Total ET", x = "Date", y = "Total ET (mm)") +
  theme_minimal()

# Multivariate Regression: How do temperature, RH, and Phragmites affect daily ET?
model <- lm(daily_total_ET ~ avg_temp + avg_rh + phrag_presence, data = ugs_daily)
summary(model)

# Seasonal decomposition example using the daily ET time series:
library(forecast)
et_ts <- ts(ugs_daily$daily_total_ET, frequency = 365, start = c(year(min(ugs_daily$date)), yday(min(ugs_daily$date))))
et_stl <- stl(et_ts, s.window = "periodic")
plot(et_stl)
