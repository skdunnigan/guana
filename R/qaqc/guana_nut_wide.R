# # run code prior to this one
# source('R/01_load_wrangle.R')
# source('R/01_guana_wrangle_tidy.R')

# this is to make data wider, good for qaqc and what Jessica needs for ArcPro

# dat_nut_alpha <-

  dat4 %>%
    dplyr::select(station_code, date_sampled,
                  component_short, result) %>%
    group_by(station_code, date_sampled) %>%
    tidyr::pivot_wider(names_from = component_short,
                       values_from = result)





dat_nut <- dat_nut_alpha %>%
  group_by(station_code, date_sampled, component_short) %>%
  spread(key = component_short, value = result) %>%
  ungroup()

rm(dat_nut_alpha)

# replace all NAs with blanks
# this is a new dataframe because it will make everything factors
# this is JUST to export the data into a csv without NAs
dat_nut2 <- sapply(dat_nut, as.character)
dat_nut2[is.na(dat_nut2)] <- " "
dat_nut2<-as.data.frame(dat_nut2)

write_csv(dat_nut2, "output/data_wide.csv")
rm(dat_nut2)

# pull out date information
dat_nut$month <- month(dat_nut$date_sampled)
dat_nut$day <- day(dat_nut$date_sampled)
dat_nut$year <- as.character(year(dat_nut$date_sampled))
dat_nut$date <- paste(dat_nut$year, dat_nut$month,
                      dat_nut$day, sep="-") %>%
  ymd() %>%
  as.Date()
