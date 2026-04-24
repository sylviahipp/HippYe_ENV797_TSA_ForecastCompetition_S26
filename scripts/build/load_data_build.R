# Sylvia Hipp
# March 29, 2026
# clean up load data for time series analysis


# Setup Environment -------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(forecast)

# Import Raw Data ---------------------------------------------------------

load_df_raw <- read_excel(here("data/raw/load.xlsx"))


# Process Load Data -------------------------------------------------------

# Processing steps: 
# pivot longer to hourly data
# group by date and take average load over 24-hours

load_df <- load_df_raw %>%
  mutate(date = as.Date(date)) %>% 
  pivot_longer(cols = c(h1:h24), 
               names_to = "hour", values_to = "load") %>% 
  group_by(date)

  # daily load = average across all 24 hours
  summarize(daily_load = mean(load, na.rm = TRUE)) %>%    # exclude NAs when calculating load
  ungroup()


# Convert to Time Series Object -------------------------------------------

# Step 1: Make sure there are no missing dates
start_date <- first(load_df$date)
end_date <- last(load_df$date)

dates_seq <- seq.Date(start_date, end_date, by = "day")
dates_seq <- tibble("date" = dates_seq)

load_df <- dates_seq %>% 
  left_join(load_df)

summary(load_df)  # no NAs

# Step 2: Convert to time series with multiple seasonality
# weekly seasonal period = 7
# annual seasonal period = 365.25
load_ts <- msts(load_df$daily_load, 
                seasonal.periods = c(7, 365.25), 
                start = c(year(start_date), month(start_date)))


# Export Data -------------------------------------------------------------

# export load data 
write_rds(load_df, here("data/processed/load_df.rds"))

# export load time series object
write_rds(load_ts, here("data/processed/load_ts.rds"))
