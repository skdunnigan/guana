# agm scripts

# filter between year 1 and year 2
# filter by WBID with only the open water sites
# then will filter by chla, TN, TP, Enterococcus, and DO.

# create functions for geometric means and standard deviations
gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
gmeansd <- function(x) exp(sd(log(x), na.rm = TRUE))

#### LAKE ####
# agm tables for open water sites (used in assessment) in the lake for each year by
# site Y1
agms_l_site_year1 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")) &
           WBID == "Lake" &
           sitetype == "OpenWater") %>%
  group_by(site, component_short) %>%
  summarise(means = gmean(result),
            sd = gmeansd(result))

# WBID Y1
agms_l_WBID_year1 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")) &
           WBID == "Lake" &
           sitetype == "OpenWater") %>%
  group_by(WBID, component_short) %>%
  summarise(means = gmean(result),
            sd = gmeansd(result))

# site Y2
agms_l_site_year2 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")) &
           WBID == "Lake" &
           sitetype == "OpenWater") %>%
  group_by(site, component_short) %>%
  summarise(means = gmean(result),
            sd = gmeansd(result))

# WBID Y2
agms_l_WBID_year2 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")) &
           WBID == "Lake" &
           sitetype == "OpenWater") %>%
  group_by(WBID, component_short) %>%
  summarise(means = gmean(result),
            sd = gmeansd(result))

#### RIVER ####
# agm tables for open water sites (used in assessment) in the river for each year by
# site Y1
agms_r_site_year1 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")) &
           WBID == "River" &
           sitetype == "OpenWater") %>%
  group_by(site, component_short) %>%
  summarise(means = gmean(result),
            sd = gmeansd(result))

# WBID Y1
agms_r_WBID_year1 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")) &
           WBID == "River" &
           sitetype == "OpenWater") %>%
  group_by(WBID, component_short) %>%
  summarise(means = gmean(result),
            sd = gmeansd(result))

# site Y2
agms_r_site_year2 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")) &
           WBID == "River" &
           sitetype == "OpenWater") %>%
  group_by(site, component_short) %>%
  summarise(means = gmean(result),
            sd = gmeansd(result))

# WBID Y2
agms_r_WBID_year2 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")) &
           WBID == "River" &
           sitetype == "OpenWater") %>%
  group_by(WBID, component_short) %>%
  summarise(means = gmean(result),
            sd = gmeansd(result))

# create tables for
