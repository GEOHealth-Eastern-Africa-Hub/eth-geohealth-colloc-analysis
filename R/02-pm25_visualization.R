###############################################################################
#### Step 2: DATA Visualization

library(ggplot2)
library(plotly)
library(htmlwidgets)

## Read in data 
combined_hourly_df <- readxl::read_excel("outputs/combined_hourly_df.xlsx")

#### Hourly Time Series Plots -------------------------------------------------
## Create hourly time series plot (trends) of `PM2.5`
p1 <- combined_hourly_df |> 
  ggplot(aes(x = datetime, y = pm25_env, color = sn)) +
  geom_point(size = 0.8, alpha = 0.7) +
  labs(x = "Date-time (hours)",
       y = "PM2.5 (µg/m³)",  # expression(PM[2.5] ~ (µg/m^3))
       title = "Hourly PM2.5 Trends") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggplotly(p1)

# Save & Export Plots
saveWidget(ggplotly(p1), file = paste0("outputs/plots/ts1.html"))
ggsave(p1,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/ts1.png"))

## Create multi-panel plot (trends) of `PM2.5`
sp1 <- combined_hourly_df |> 
  ggplot(aes(x = datetime, y = pm25_env, color = sn)) +
  geom_point(size = 0.8, alpha = 0.7) +
  labs(x = "Date-time (hours)",
       y = expression(PM[2.5] ~ (µg/m^3)),
       title = "Hourly PM2.5 Trends by Sensor") +
  facet_wrap(~ sn, scales = "free_y") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

ggplotly(sp1)

# Save & Export Plots
saveWidget(ggplotly(sp1), file = paste0("outputs/plots/mp_ts1.html"))
ggsave(sp1,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/mp_ts1.png"))

## ALTERNATIVE: Create multi-panel plot (trends) of `PM2.5` without color by sensor
sp1a <- combined_hourly_df |> 
  ggplot(aes(x = datetime, y = pm25_env)) +
  geom_point(size = 0.8, alpha = 0.7) +
  labs(x = "Date-time (hours)",
       y = expression(PM[2.5] ~ (µg/m^3)),
       title = "Hourly PM2.5 Trends by Sensor (No Color)") +
  facet_wrap(~ sn, scales = "free_y") +
  theme_bw(base_size = 12)

ggplotly(sp1a)

# Save & Export Plots
saveWidget(ggplotly(sp1a), file = paste0("outputs/plots/mp_ts1a.html"))
ggsave(sp1a,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/mp_ts1a.png"))

## Read in data 
combined_daily_df <- readxl::read_excel("outputs/combined_daily_df.xlsx")

#### Daily Time Series Plots --------------------------------------------------
## Create daily time series plot (trends) of `PM2.5`
p2 <- combined_daily_df |> 
  ggplot(aes(x = datetime, y = pm25_env, color = sn)) +
  geom_point(size = 0.8, alpha = 0.7) +
  labs(x = "Date-time (days)",
       y = "PM2.5 (µg/m³)",
       title = "Daily PM2.5 Trends") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

ggplotly(p2)

# Save & Export Plots
saveWidget(ggplotly(p2), file = paste0("outputs/plots/ts2.html"))
ggsave(p2,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/ts2.png"))

## Create multi-panel plot (trends) of `PM2.5`
sp2 <- combined_daily_df |> 
  ggplot(aes(x = datetime, y = pm25_env, color = sn)) +
  geom_point(size = 0.8, alpha = 0.7) +
  labs(x = "Date-time (days)",
       y = expression(PM[2.5] ~ (µg/m^3)),
       title = "Daily PM2.5 Trends by Sensor") +
  facet_wrap(~ sn, scales = "free_y") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

ggplotly(sp2)

# Save & Export Plots
saveWidget(ggplotly(sp2), file = paste0("outputs/plots/mp_ts2.html"))
ggsave(sp2,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/mp_ts2.png"))

## ALTERNATIVE: Create multi-panel plot (trends) of `PM2.5` without color by sensor
sp2a <- combined_daily_df |> 
  ggplot(aes(x = datetime, y = pm25_env)) +
  geom_point(size = 0.8, alpha = 0.7) +
  labs(x = "Date-time (days)",
       y = expression(PM[2.5] ~ (µg/m^3)),
       title = "Daily PM2.5 Trends by Sensor (No Color)") +
  facet_wrap(~ sn, scales = "free_y") +
  theme_bw(base_size = 12)

ggplotly(sp2a)

# Save & Export Plots
saveWidget(ggplotly(sp2a), file = paste0("outputs/plots/mp_ts2a.html"))
ggsave(sp2a,  width = 30, height = 20, units = "cm", dpi = 300, 
       file = paste0("outputs/plots/mp_ts2a.png"))

###############################################################################