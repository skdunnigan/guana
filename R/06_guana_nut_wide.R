# run code prior to this one
source('R/01_load_wrangle.R')
source('R/01_guana_wrangle_tidy.R')

# this is to make data wider

dat_nut_alpha <- dat4 %>%
  select(site, station_code, date_sampled, REGsites, WBID, component_short, result)

dat_nut_beta <- dat_nut_alpha %>%
  group_by(station_code, date_sampled, component_short) %>%
  mutate(count = seq(n()), newcolumn = paste0(component_short, "_", count))

dat_nut_gamma <- dat_nut_beta %>%
  ungroup() %>%
  select(-component_short, -count)

dat_nut <- dat_nut_gamma %>%
  spread(key = newcolumn, value = result)

rm(dat_nut_alpha, dat_nut_beta, dat_nut_gamma)
write_csv(dat_nut, "dat_nut.csv")
