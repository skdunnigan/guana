# run code prior to this one
source('R/01_load_wrangle.R')
source('R/01_guana_wrangle_tidy.R')

# this is to make data wider, good for qaqc and what Jessica needs for ArcPro

dat_nut_alpha <- dat4 %>%
  select(site, latitude, longitude, station_code, date_sampled, sitetype, WBID, component_short, result)

dat_nut <- dat_nut_alpha %>%
  group_by(station_code, date_sampled, component_short) %>%
  spread(key = component_short, value = result)

rm(dat_nut_alpha)

# replace all NAs with blanks
# this is a new dataframe because it will make everything factors
# this is JUST to export the data into a csv without NAs
dat_nut2 <- sapply(dat_nut, as.character)
dat_nut2[is.na(dat_nut2)] <- " "
dat_nut2<-as.data.frame(dat_nut2)

write_csv(dat_nut2, "output/data_wide.csv")
