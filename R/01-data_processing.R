###############################################################################
# Clean Environment and Load Libraries
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(plyr)

#### Step 1: Data Processing --------------------------------------------------
## Read in data from µSD cards/saved files --------------------------#1
# Get list of CSV files in the specified directory
list_of_files <- list.files(path = "inputs/USE_MODPM_collocated_data/MOD-PM-00854",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

# Read and combine data from CSV files, skipping the first 3 rows and selecting relevant columns
df_raw <- readr::read_csv(list_of_files, skip = 3, col_select = c("timestamp_iso", "pm25_env")) %>%
  drop_na()

# Convert the timestamp to a proper datetime format
df_raw <- df_raw |> 
  mutate(timestamp_iso = ymd_hms(timestamp_iso,
                          quiet = FALSE, tz = "UTC",
                          locale = Sys.getlocale("LC_TIME"),
                          truncated = 0))

# Convert UTC time to East Africa Time (EAT, UTC +3)
df_raw <- df_raw %>%
  mutate(datetime = with_tz(timestamp_iso, tzone = "Africa/Addis_Ababa")) %>%
  mutate(year = year(datetime), month = month(datetime), day = day(datetime))

# Aggregate PM2.5 data to Hourly level ---------------------------------------
Hdf <- df_raw |> select(datetime, pm25_env)

# Function to calculate count and mean PM2.5 value
getmeans <- function(Df) {
  c(n = nrow(Df), pm25_env = mean(Df$pm25_env, na.rm = TRUE))
}

# Add 'hour' column and apply aggregation function
Hdf$hour <- cut(Hdf$datetime, breaks = "hour")
Hdf <- ddply(Hdf, .(hour), getmeans)

# Convert hour to datetime and extract year, month, day, and hour
Hdf <- Hdf |>
  mutate(datetime = ymd_hms(hour)) |> 
  mutate(year = year(datetime), month = month(datetime), day = day(datetime), hour = hour(datetime))

# Filter hourly data for >75% coverage and calculate proportion
hourly_data <- Hdf |> 
  mutate(prop = (n / 720) * 100) |> 
  select(datetime, year, month, day, hour, n, prop, pm25_env) |> 
  filter(prop > 75) 

# Final hourly dataset with station info
hrs_mod_df1 <- hourly_data |> 
  select(datetime, year, month, day, hour, n, prop, pm25_env) |> 
  add_column(sn = "MOD-PM-00854")

# Aggregate PM2.5 data to Daily level ---------------------------------------
Ddf <- df_raw |> select(datetime, pm25_env)

# Apply aggregation function for daily data
Ddf$day <- cut(Ddf$datetime, breaks = "day")
Ddf <- ddply(Ddf, .(day), getmeans)
Ddf$day <- as.POSIXct(Ddf$day)

# Convert day to datetime and extract year, month, day
Ddf <- Ddf |> 
  mutate(datetime = ymd(day)) |> 
  mutate(year = year(datetime), month = month(datetime), day = day(datetime))

# Filter daily data for >75% coverage and calculate proportion
daily_data <- Ddf |> 
  mutate(prop = (n / 17280) * 100) |> 
  select(datetime, year, month, day, n, prop, pm25_env) |> 
  filter(prop > 75)

# Final daily dataset with station info
day_mod_df1 <- daily_data |> 
  select(datetime, month, day, n, prop, pm25_env) |> 
  add_column(sn = "MOD-PM-00854")

## Repeat for additional data sources (MOD-PM-00118) --------------------------
# Repeating similar steps for the second dataset: "MOD-PM-00118"

# Read and process BAM data -----------------------------------------------#3
BAM <- read.csv("inputs/USE_MODPM_collocated_data/AddisAbabaJacros_PM2.5_2024_YTD.csv") |> 
  select(`Date..LT.`, `Raw.Conc.`) |> 
  filter(`Raw.Conc.` > 0)  # Keep values greater than 0

# Rename columns and process datetime
names(BAM) <- c("datetime", "pm25_env")

# Convert datetime to proper format and separate into year, month, day, hour
Hourly_dfBAM <- BAM |> 
  mutate(datetime = ymd_hm(datetime))

# Convert datetime to 4 separate columns (year, month, day, hour)
Hdf <- Hourly_dfBAM |> 
  mutate(datetime = ymd_hms(datetime)) |> 
  mutate(year = year(datetime), month = month(datetime), day = day(datetime), hour = hour(datetime))

## Calculate hourly proportions -------------------------------------------
hourly_data <- Hdf |>
  mutate(n = length(pm25_env),
         prop = (n)*100) |>  # Proportions based on hourly data
  select(datetime, year, month, day, hour, n, prop, pm25_env) 

## Convert to Hourly data ----------------------------------------------------
hrs_mod_df2 <- hourly_data |> 
  select(datetime, year, month, day, hour, n, prop, pm25_env) |> 
  filter(!is.na(datetime)) |>  # Remove NA values
  add_column(sn = "Addis Ababa Jacros") 

## Convert to Daily data ----------------------------------------------------
Ddf <- Hourly_dfBAM |> 
  select(datetime, pm25_env)

# Apply 'getmeans' function to calculate daily mean and counts
Ddf$day <- cut(Ddf$datetime, breaks = "day")
Ddf <- ddply(Ddf, .(day), getmeans)
Ddf$day <- as.POSIXct(Ddf$day)

# Convert day to datetime and separate into year, month, day
Ddf <- Ddf |> 
  mutate(datetime = ymd(day)) |> 
  mutate(year = year(datetime), month = month(datetime), day = day(datetime))

# Calculate daily proportions based on 24 hours
daily_data <- Ddf |>
  mutate(prop = (n / 24) * 100) |> 
  select(datetime, year, month, day, n, prop, pm25_env) |> 
  filter(prop > 75)  # Keep days with more than 75% coverage

## Convert to Daily data -----------------------------------------------------
day_mod_df2 <- daily_data |> 
  select(datetime, month, day, n, prop, pm25_env) |> 
  filter(!is.na(datetime)) |>  # Remove NA values
  add_column(sn = "Addis Ababa Jacros") 

#### Step 2: DATA MERGING ----------------------------------------------------
# COMBINE/ MERGE DATA ========================================================= 
# Combine hourly data into one dataframe
df_combined_hourly <- rbind(hrs_mod_df1, hrs_mod_df2) |> 
  filter(datetime >= "2024-10-31" & datetime <= "2024-12-30")

# Export hourly data to CSV
write.csv(df_combined_hourly, file = paste0("outputs/df_combined_hourly.csv"))

# Combine daily data into one dataframe
df_combined_daily <- rbind(day_mod_df1, day_mod_df2) |> 
  filter(datetime >= "2024-10-31" & datetime <= "2024-12-30")

# Export daily data to CSV
write.csv(df_combined_daily, file = paste0("outputs/df_combined_daily.csv"))
###############################################################################