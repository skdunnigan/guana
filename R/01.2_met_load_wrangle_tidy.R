# need to run this code first
# source('R/01_load_wrangle.R')

# ----01 LOAD read in tolomato met file ----

met_dat <- read_xlsx(here::here("data", "tolomato.xlsx")) %>%
                       janitor::clean_names()

# inspect the data file
head(met_dat)
str(met_dat)

# ----02 TIDY tolomato met data tidying, part 1----

# set 'rainfall_mm' column to numerical value, this column contains all the numerical values from the analyses
cols.num <- c("battery_v", "water_temp_c", "water_conductivity_m_s_cm",
              "water_salinity_ppt", "air_temp_c", "relative_humidity_percent",
              "barometric_pressure_in_hg", "rainfall_mm", "navd88_water_level_m")
met_dat[cols.num] <- sapply(met_dat[cols.num],as.numeric)
sapply(met_dat, class)

# pull out date information
met_dat$month <- month(met_dat$date_time_est)
met_dat$day <- day(met_dat$date_time_est)
met_dat$year <- as.character(year(met_dat$date_time_est))
rm(cols.num)

# ----03 WRANGLE subset to daily meteorological parameters----

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

# ----04 WRANGLE subset to monthly meteorological parameters----

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

