# need to run this code first
# source('R/01_load_wrangle.R')

# ---------------------------------------------------
# daily meteorological parameters
# ---------------------------------------------------
met_dat_daily <- met_dat %>%
  group_by(year, month, day) %>%
  summarise(dailyrain = sum(rainfall_mm, na.rm = TRUE),
            daily_avg_wtemp = mean(water_temp_c, na.rm = TRUE),
            daily_avg_sal = mean(water_salinity_ppt, na.rm = TRUE),
            daily_avg_atemp = mean(air_temp_c, na.rm = TRUE)) %>%
  ungroup()
met_dat_daily$datetime <- paste(met_dat_daily$year, met_dat_daily$month,
                                met_dat_daily$day, sep="-") %>%
  ymd() %>%
  as.Date()

# ---------------------------------------------------
# monthly meteorological parameters
# ---------------------------------------------------
met_dat_monthly <- met_dat %>%
  group_by(year, month) %>%
  summarise(monthlyrain = sum(rainfall_mm, na.rm = TRUE),
            monthly_avg_wtemp = mean(water_temp_c, na.rm = TRUE),
            monthly_avg_sal = mean(water_salinity_ppt, na.rm = TRUE),
            monthly_avg_atemp = mean(air_temp_c, na.rm = TRUE)) %>%
  ungroup()
met_dat_monthly$day <- 1
met_dat_monthly$datetime <- paste(met_dat_monthly$year, met_dat_monthly$month,
                                  met_dat_monthly$day, sep="-") %>%
  ymd() %>%
  as.Date()

