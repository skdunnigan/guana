# swmp agms

# source('R/01_swmpnut_wrangle.R')

# annual geometric means
# -----------------------------------------------------------
# A.  create functions for geometric means and standard deviations
# -----------------------------------------------------------
gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
gmeansd <- function(x) exp(sd(log(x), na.rm = TRUE))

# -----------------------------------------------------------
# B.  calculations
# -----------------------------------------------------------
# Y1
swmp_agmy1 <- swmp_dat_n2 %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30"))) %>%
  group_by(component_short) %>%
  summarise(Year1AGM = gmean(result),
            Year1sd = gmeansd(result))

# Y2
swmp_agmy2 <- swmp_dat_n2 %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30"))) %>%
  group_by(component_short) %>%
  summarise(Year2AGM = gmean(result),
            Year2sd = gmeansd(result))

swmp_agms <- dplyr::full_join(swmp_agmy1, swmp_agmy2, by = 'component_short')
swmp_agms$Year1AGM <- round(swmp_agms$Year1AGM, digits=2)
swmp_agms$Year1sd <- round(swmp_agms$Year1sd, digits=2)
swmp_agms$Year2AGM <- round(swmp_agms$Year2AGM, digits=2)
swmp_agms$Year2sd <- round(swmp_agms$Year2sd, digits=2)

rm(swmp_agmy1, swmp_agmy2)
