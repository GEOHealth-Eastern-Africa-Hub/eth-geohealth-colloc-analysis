###############################################################################
# COMBINE DATA FRAME AND CREATE AGGREGATE PM FOR FURTHER ANALYSES =============
#### Step 3: MERGE DATA and Visualization
##
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(tidyr)

# Function to rename columns to avoid duplication
rename_columns <- function(dt, suffix) {
  # Rename all columns except 'datetime'
  colnames(dt) <- ifelse(colnames(dt) == "datetime", "datetime", paste0(colnames(dt), "_", suffix))
  return(dt)
}

# Function to process and merge data
process_and_merge_data <- function(data_list, suffix) {
  data_list <- lapply(data_list, setDT) # Convert to data.table
  data_list <- mapply(rename_columns, data_list, suffix = seq_along(data_list), SIMPLIFY = FALSE) # Rename columns
  data_list <- lapply(data_list, function(dt) dt[!duplicated(dt$datetime)]) # Remove duplicates
  combined_data <- Reduce(function(x, y) merge(x, y, by = "datetime", all = TRUE, allow.cartesian = TRUE), data_list) # Merge
  return(as.data.frame(combined_data)) # Convert back to data.frame
}

# Process hourly data ---------------------------------------------------------
hrs_mod_list <- list(hrs_mod_df1, hrs_mod_df2, hrs_mod_df3)
agg_hourly_df_combined <- process_and_merge_data(hrs_mod_list, suffix = seq_along(hrs_mod_list))

# Filter hourly data by date range
agg_hourly_df_combined <- agg_hourly_df_combined %>%
  filter(datetime >= "2024-10-31" & datetime <= "2024-12-30")

# Specify columns for PM2.5 and calculate hourly averages
pm25_columns <- c("pm25_env_1", "pm25_env_2", "pm25_env_3")
agg_hourly_data <- agg_hourly_df_combined %>%
  mutate(average_pm25 = rowMeans(select(., all_of(pm25_columns)), na.rm = TRUE)) %>%
  select(datetime, pm25_env_1, pm25_env_2, pm25_env_3, average_pm25) %>%
  filter(!is.na(datetime))

# Rename columns for clarity
colnames(agg_hourly_data)[c(2:5)] <- c("pm25_sn_54", "pm25_sn_18", "pm25_bam", "PM25_avg")

# Write hourly data to CSV
write_csv(agg_hourly_data, file = "outputs/agg_hourly_data.csv")

# Process daily data ----------------------------------------------------------
day_mod_list <- list(day_mod_df1, day_mod_df2, day_mod_df3)
agg_daily_df_combined <- process_and_merge_data(day_mod_list, suffix = seq_along(day_mod_list))

# Calculate daily averages
agg_daily_data <- agg_daily_df_combined %>%
  mutate(average_pm25 = rowMeans(select(., all_of(pm25_columns)), na.rm = TRUE)) %>%
  select(datetime, pm25_env_1, pm25_env_2, pm25_env_3, average_pm25)

# Rename columns for clarity
colnames(agg_daily_data)[c(2:5)] <- c("pm25_sn_54", "pm25_sn_18", "pm25_bam", "PM25_avg")

# Write daily data to CSV
write_csv(agg_daily_data, file = "outputs/agg_daily_data.csv")

###############################################################################

library(ggpmisc)

## Create hourly scatter plot `PM2.5`Vs it's average PM for all units
ggp1 <- ggplot(data = agg_hourly_data, aes(x = pm25_sn_54, y = PM25_avg)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "Average PM2.5 (ugm-3)",
       y = "PM2.5 (MOD-PM-00854)") +
  theme_bw()

ggplotly(ggp1)

#Save & Export Plots
saveWidget(ggplotly(ggp1), file = paste0("outputs/plots/hourly_sp1.html"))
ggsave(ggp1,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs//plots/hourly_sp1.png"))

## Create hourly scatter plot `PM2.5`Vs average PM of GEOHealth-BAM
ggp1a <- ggplot(data = agg_hourly_data, aes(x = pm25_sn_54, y = pm25_bam)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "AA Jacros-BAM PM2.5 (ugm-3)",
       y = "PM2.5 (MOD-PM-00854)") +
  theme_bw()

ggplotly(ggp1a)

#Save & Export Plots
saveWidget(ggplotly(ggp1a), file = paste0("outputs/plots/hourly_sp1a.html"))
ggsave(ggp1a,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/hourly_sp1a.png"))

## Create hourly scatter plot `average PM2.5`Vs average PM of GEOHealth-BAM
ggpavg_b <- ggplot(data = agg_hourly_data, aes(x = PM25_avg, y = pm25_bam)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "AA Jacros-BAM PM2.5 (ug/m^3)",
       y = "Average Modulair PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16, face = "bold")   # Increase size of y-axis label
  )

ggplotly(ggpavg_b)

#Save & Export Plots
saveWidget(ggplotly(ggpavg_b), file = paste0("outputs/plots/hourly_avg_bam.html"))
ggsave(ggpavg_b,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/hourly_avg_bam.png"))

## Create daily scatter plot `PM2.5`Vs it's average PM for all units
ggp2 <- ggplot(data = agg_daily_data, aes(x = pm25_sn_54, y = PM25_avg)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "Average PM2.5 (ugm-3)",
       y = "PM2.5 (MOD-PM-00854)") +
  theme_bw()

ggplotly(ggp2)

#Save & Export Plots
saveWidget(ggplotly(ggp2), file = paste0("outputs/plots/daily_sp2.html"))
ggsave(ggp2,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/daily_sp2.png"))

## Create daily scatter plot `PM2.5`Vs average PM of GEOHealth-BAM
ggp2a <- ggplot(data = agg_daily_data, aes(x = pm25_sn_54, y = pm25_bam)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "AA Jacros-BAM PM2.5 (ugm-3)",
       y = "PM2.5 (MOD-PM-00854)") +
  theme_bw()

ggplotly(ggp2a)

#Save & Export Plots
saveWidget(ggplotly(ggp2a), file = paste0("outputs/plots/daily_sp2a.html"))
ggsave(ggp2a,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/daily_sp2a.png"))

## Create daily scatter plot `avg. PM2.5`Vs average PM of GEOHealth-BAM
ggp_avg_b2 <- ggplot(data = agg_daily_data, aes(x = PM25_avg, y = pm25_bam)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "AA Jacros-BAM PM2.5 (ug/m^3)",
       y = "Average Modulair PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16, face = "bold")   # Increase size of y-axis label
  )

ggplotly(ggp_avg_b2)

#Save & Export Plots
saveWidget(ggplotly(ggp_avg_b2), file = paste0("outputs/plots/daily_avg_bam.html"))
ggsave(ggp_avg_b2,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/daily_avg_bam.png"))

# convert data from wide to long format ---------------------------------------
df_longf <- agg_hourly_data |> 
  pivot_longer(cols = c('pm25_sn_54', 'pm25_sn_18', 'pm25_bam'),
               names_to = 'sn', values_to = 'PM')

# Write output csv dataset out ------------------------------------------------

write_csv(df_longf, file = paste0("outputs/hourlypm25_long_format_df.csv"))

ggpm25_all <- ggplot(data = df_longf, aes(x = PM25_avg, y = PM, color = sn)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "Average Modulair PM2.5 (ug/m^3)",
       y = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16, face = "bold")   # Increase size of y-axis label
  )

ggplotly(ggpm25_all)

#Save & Export Plots
saveWidget(ggplotly(ggpm25_all), file = paste0("outputs/plots/pm25_hourly_all.html"))
ggsave(ggpm25_all,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/pm25_hourly_all.png"))

ggpm25_all2 <- ggplot(data = df_longf, aes(x = PM25_avg, y = PM)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "Average Modulair PM2.5 (ug/m^3)",
       y = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16, face = "bold")   # Increase size of y-axis label
  ) +
  facet_wrap(~sn)

ggplotly(ggpm25_all2)

#Save & Export Plots
saveWidget(ggplotly(ggpm25_all2), file = paste0("outputs/plots/pm25_hourly_all2.html"))
ggsave(ggpm25_all2,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/pm25_hourly_all2.png"))

###############################################################################
## To address additional analysis request by Ato Solomon:

# convert data from wide to long format excluding BAM -------------------------
df_longf2 <- agg_hourly_data |> 
  pivot_longer(cols = c('pm25_sn_54', 'pm25_sn_18'),
               names_to = 'sn', values_to = 'PM')

# Write output csv dataset out ------------------------------------------------
write_csv(df_longf2, file = paste0("outputs/hourlypm25_long_format_df2.csv"))

ggpm25_avg <- ggplot(data = df_longf2, aes(x = PM25_avg, y = PM, color = sn)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "Average Modulair PM2.5 (ug/m^3)",
       y = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16, face = "bold")   # Increase size of y-axis label
  )

ggplotly(ggpm25_avg)

#Save & Export Plots
saveWidget(ggplotly(ggpm25_avg), file = paste0("outputs/plots/pm25_hourly_avg.html"))
ggsave(ggpm25_avg,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/pm25_hourly_avg.png"))

ggpm25_avg2 <- ggplot(data = df_longf2, aes(x = PM25_avg, y = PM)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "Average Modulair PM2.5 (ug/m^3)",
       y = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16, face = "bold")   # Increase size of y-axis label
  ) +
  facet_wrap(~sn)

ggplotly(ggpm25_avg2)

#Save & Export Plots
saveWidget(ggplotly(ggpm25_avg2), file = paste0("outputs/plots/pm25_hourly_avg2.html"))
ggsave(ggpm25_avg2,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/pm25_hourly_avg2.png"))

# convert data from wide to long format ---------------------------------------
df_longf <- agg_daily_data |> 
  pivot_longer(cols = c('pm25_sn_54', 'pm25_sn_18', 'pm25_bam'),
               names_to = 'sn', values_to = 'PM')

# Write output csv dataset out ------------------------------------------------
write_csv(df_longf, file = paste0("outputs/dailypm25_long_format_df.csv"))

ggpm25_alld <- ggplot(data = df_longf, aes(x = PM25_avg, y = PM, color = sn)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "Average Modulair PM2.5 (ug/m^3)",
       y = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16, face = "bold")   # Increase size of y-axis label
  )

ggplotly(ggpm25_alld)

#Save & Export Plots
saveWidget(ggplotly(ggpm25_alld), file = paste0("outputs/plots/pm25_daily_all.html"))
ggsave(ggpm25_alld,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/pm25_daily_all.png"))

ggpm25_alld2 <- ggplot(data = df_longf, aes(x = PM25_avg, y = PM)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "Average Modulair PM2.5 (ug/m^3)",
       y = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16, face = "bold")   # Increase size of y-axis label
  ) +
  facet_wrap(~sn)

ggplotly(ggpm25_alld2)

#Save & Export Plots
saveWidget(ggplotly(ggpm25_alld2), file = paste0("outputs/plots/pm25_daily_all2.html"))
ggsave(ggpm25_alld2,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/pm25_daily_all2.png"))

## Addional analysis excluding BAM from the averaging:
# convert data from wide to long format ---------------------------------------
df_longf2 <- agg_daily_data |> 
  pivot_longer(cols = c('pm25_sn_54', 'pm25_sn_18'),
               names_to = 'sn', values_to = 'PM')

# Write output csv dataset out ------------------------------------------------
write_csv(df_longf2, file = paste0("outputs/dailypm25_long_format_df2.csv"))

ggpm25_avgd <- ggplot(data = df_longf2, aes(x = PM25_avg, y = PM, color = sn)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "Average Modulair PM2.5 (ug/m^3)",
       y = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16, face = "bold")   # Increase size of y-axis label
  )

ggplotly(ggpm25_avgd)

#Save & Export Plots
saveWidget(ggplotly(ggpm25_avgd), file = paste0("outputs/plots/pm25_daily_avg.html"))
ggsave(ggpm25_avgd,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/pm25_daily_avg.png"))

ggpm25_avgd2 <- ggplot(data = df_longf2, aes(x = PM25_avg, y = PM)) +
  stat_poly_line() + # assembling a single label with equation and R2
  stat_poly_eq(use_label(c("eq", "R2", "n")), 
               formula = y ~ x) +
  geom_point() +
  labs(x = "Average Modulair PM2.5 (ug/m^3)",
       y = "PM2.5 (ug/m^3)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16, face = "bold"),  # Increase size of x-axis label
        axis.title.y = element_text(size = 16, face = "bold")   # Increase size of y-axis label
  ) +
  facet_wrap(~sn)

ggplotly(ggpm25_avgd2)

#Save & Export Plots
saveWidget(ggplotly(ggpm25_avgd2), file = paste0("outputsplots//pm25_daily_avg2.html"))
ggsave(ggpm25_avgd2,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/pm25_daily_avg2.png"))

###############################################################################