# agm scripts

# filter between year 1 and year 2
# filter by WBID with only the open water sites
# calculate AGM for chla, TN, TP, Enterococcus, and DO.

dat_y1 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")) &
           WBID == "Lake" &
           sitetype == "OpenWater")

dat_y2 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")) &
           WBID == "Lake" &
           sitetype == "OpenWater")

dat_y2 %>%
  filter(component_short == "CHLa_C" & site == "Lake Middle") %>%
  select(result) %>%
  psych::geometric.mean(na.rm = TRUE)
