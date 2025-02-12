santiago_station <- haven::read_dta(here::here("data",
                                               "raw",
                                               "pollution_station_measure",
                                               "santiago_daily_data.dta"))

bogota_station <- haven::read_dta(here::here("data",
                                             "raw",
                                             "pollution_station_measure",
                                             "Air_Pollution_Bogota_2020_2023.dta"))

bogota_station_hourly <- bogota_station %>%
  # Adjust the hour column so that hour 1 becomes 0, hour 2 becomes 1, etc.
  mutate(hour = hour - 1) %>%
  # Group by date and the adjusted hour to average over all stations
  group_by(date, hour) %>%
  summarize(pm25_avg = mean(pm25, na.rm = TRUE),
            .groups = "drop")

santiago_daily <- santiago %>%
  group_by(Date) %>%
  summarise(pm25_merra2 = mean(pm25_estimate, na.rm = TRUE)) %>% 
  mutate(Date = as.Date(Date))


bogota <- bogota %>% 
  mutate(Date = as.Date(Date))

bogota_daily <- bogota %>% 
  mutate(Date = as.Date(Date)) %>% 
  group_by(Date) %>%
  summarise(pm25_merra2 = mean(pm25_estimate, na.rm = TRUE))

bogota_station_daily <- bogota_station_hourly %>% 
  group_by(date) %>% 
  summarise(pm25_avg = mean(pm25_avg, na.rm = TRUE),
            .groups = "drop")

# Prepare Santiago_station data, ensure date is Date object
santiago_station <- santiago_station %>%
  mutate(date = as.Date(date))

# Merge on date. Keep only dates that exist in santiago_daily (inner join)
santiago_combined <- inner_join(santiago_daily,
                                santiago_station %>% select(date,
                                                            pm25_station = pm25_validated_mean),
                                by = c("Date" = "date"))

# Calculate correlation between pm25_merra2 and pm25_station
santiago_correlation <- cor(santiago_combined$pm25_merra2,
                            santiago_combined$pm25_station,
                            use = "complete.obs")

# Merge data
bogota_combined <- inner_join(
  bogota %>% select(Date, Hour, pm25_estimate) %>% rename(pm25_merra2 = pm25_estimate), 
  bogota_station_hourly %>% rename(date = date, hour = hour, pm25_station = pm25_avg),
  by = c("Date" = "date", "Hour" = "hour"))

bogota_correlation <- cor(bogota_combined$pm25_merra2,
                          bogota_combined$pm25_station,
                          use = "complete.obs")


bogota_combined_daily <- inner_join(
  bogota_daily,
  bogota_station_daily %>% select(date, pm25_station = pm25_avg),
  by = c("Date" = "date")
  )

bogota_correlation_daily <- cor(bogota_combined_daily$pm25_merra2,
                                bogota_combined_daily$pm25_station,
                                use = "complete.obs")
