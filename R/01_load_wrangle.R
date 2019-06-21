# code is to bring in data file and do some basic data tidying
source('R/00_loadpackages.R')

# ---------------------------------------------------
# read in guana data file\
# ---------------------------------------------------
dat <- read_xlsx("data/data-0619.xlsx", sheet = 'Sheet1') %>% janitor::clean_names()
#janitor::clean_names() function cleans up the column header names!

# inspect the data file
head(dat)
str(dat)

# ---------------------------------------------------
# data type housekeeping
# ---------------------------------------------------
# set 'result' column to numerical value, this column contains all the numerical values from the analyses
cols.num <- c("result", "mdl", "pql", "mrl", "dilution")
dat[cols.num] <- sapply(dat[cols.num],as.numeric)
sapply(dat, class)
rm(cols.num)

# convert datetimes into POSIXct format
dat$date_sampled<- as.POSIXct(dat$date_sampled, format = "%m/%d/%Y %H:%M", tz = 'America/Regina')
dat$date_received <- as.POSIXct(dat$date_received, format = "%m/%d/%Y %H:%M", tz = 'America/Regina')
dat$date_analyzed <- as.POSIXct(dat$date_analyzed, format = "%m/%d/%Y %H:%M", tz = 'America/Regina')

# dont forget to check the unique component names, uncomment code below if necessary
# should be the same number of both!
unique(dat$component_short)
unique(dat$component_long)

# ---------------------------------------------------
# read in tolomato met file
# ---------------------------------------------------
met_dat <- read_xlsx("data/tolomato.xlsx") %>% janitor::clean_names()

# inspect the data file
head(met_dat)
str(met_dat)

# ---------------------------------------------------
# data type housekeeping
# ---------------------------------------------------
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
