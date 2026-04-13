# Sylvia Hipp
# April 13, 2026
# clean up weather data for time series analysis


# Setup Environment -------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(forecast)
library(glue)

# Import Raw Data ---------------------------------------------------------

humidity_raw <- read_excel(here("data/raw/relative_humidity.xlsx"))
temperature_raw <- read_excel(here("data/raw/temperature.xlsx"))


# Clean data --------------------------------------------------------------

# get average temp and humidity values for each hour

humidity_avg <- humidity_raw %>% 
  mutate(datetime = as.POSIXct(glue("{date} {hr}:00"))) %>% 
  pivot_longer(cols = contains("rh"), values_to = "relative_humidity") %>% 
  group_by(datetime) %>% 
  summarize(avg_rh = mean(relative_humidity)) %>% 
  ungroup()

summary(humidity_avg)  # no missing values

temperature_avg <- temperature_raw %>%
  filter(!is.na(date)) %>% 
  mutate(datetime = as.POSIXct(glue("{date} {hr}:00"))) %>% 
  pivot_longer(cols = contains("t_"), values_to = "temperature") %>% 
  group_by(datetime) %>% 
  summarize(avg_temp = mean(temperature)) %>% 
  ungroup()

summary(temperature_avg)  # no missing values 


# Export data  ------------------------------------------------------------

write_rds(humidity_avg, here("data/processed/avg_humidity.RDS"))
write_rds(temperature_avg, here("data/processed/avg_temperature.RDS"))
