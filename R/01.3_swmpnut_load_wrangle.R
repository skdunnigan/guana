# code is to bring in data file and do some basic data tidying
# source('R/00_loadpackages.R')
library(reshape2)
# ---------------------------------------------------
# read in gtmnerr swmp nut files
# ---------------------------------------------------
swmp_dat <- read_xlsx("data/gtmpinut17-19.xlsx") %>% janitor::clean_names() %>% select(-x52, -x53, -f_record)

# pull out date information
swmp_dat$month <- month(swmp_dat$date_time_stamp)
swmp_dat$day <- day(swmp_dat$date_time_stamp)
swmp_dat$year <- as.character(year(swmp_dat$date_time_stamp))

swmp_dat_n <- swmp_dat %>%
  melt(id.vars = c("station_code", "date_time_stamp", "rep", "month", "year", "day"),
       measure.vars = c("po4f", "tp", "nh4f", "no2f", "no3f", "no23f", "din", "tn", "tkn", "tknf", "ton", "chla_n", "unc_ch_la_n",
                        "phea", "tss", "feccol_cfu", "wtem_n", "salt_n", "do_n", "ph_n", "doc"))

swmp_dat_n2 <- swmp_dat_n %>%
  group_by(station_code, month, day, year, variable) %>%
  summarise(result = mean(value, na.rm = TRUE)) %>%
  ungroup()

rm(swmp_dat, swmp_dat_n)

# housekeeping

swmp_dat_n2$date_sampled <- as.POSIXct(with(swmp_dat_n2, paste(year, month, day, sep="-")), "%Y-%m-%d", tz = "America/Regina")
swmp_dat_n2 <- swmp_dat_n2 %>% select(-month, -day, -year)
swmp_dat_n2 <- swmp_dat_n2[,c(1, 4, 2, 3)]

colnames(swmp_dat_n2)[3] <- "component_short"

swmp_dat_w <- swmp_dat_n2 %>%
  spread(key = component_short, value = result) %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2019-12-31")))

# create year 1 subset
swmp_y1 <- swmp_dat_w %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")))

# create year 2 subset
swmp_y2 <- swmp_dat_w %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")))

