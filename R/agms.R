# agm scripts

# filter between year 1 and year 2
# filter by WBID with only the open water sites
# then will filter by chla, TN, TP, Enterococcus, and DO.

# -----------------------------------------------------------
# A.  create functions for geometric means and standard deviations
# -----------------------------------------------------------
gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
gmeansd <- function(x) exp(sd(log(x), na.rm = TRUE))

# -----------------------------------------------------------
# Lake tables
# -----------------------------------------------------------
# agm tables for open water sites (used in assessment) in the lake for each year by
# site Y1
agms_l_site_year1 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")) &
           WBID == "Lake" &
           sitetype == "OpenWater") %>%
  group_by(site, component_short) %>%
  summarise(Year1AGM = gmean(result),
            Year1sd = gmeansd(result),
            Year1Median = median(result))

# WBID Y1
agms_l_WBID_year1 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")) &
           WBID == "Lake" &
           sitetype == "OpenWater") %>%
  group_by(WBID, component_short) %>%
  summarise(Year1AGM = gmean(result),
            Year1sd = gmeansd(result),
            Year1Median = median(result))

# site Y2
agms_l_site_year2 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")) &
           WBID == "Lake" &
           sitetype == "OpenWater") %>%
  group_by(site, component_short) %>%
  summarise(Year2AGM = gmean(result),
            Year2sd = gmeansd(result),
            Year2Median = median(result))

# WBID Y2
agms_l_WBID_year2 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")) &
           WBID == "Lake" &
           sitetype == "OpenWater") %>%
  group_by(WBID, component_short) %>%
  summarise(Year2AGM = gmean(result),
            Year2sd = gmeansd(result),
            Year2Median = median(result))

# -----------------------------------------------------------
# River tables
# -----------------------------------------------------------
# agm tables for open water sites (used in assessment) in the river for each year by
# site Y1
agms_r_site_year1 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")) &
           WBID == "River" &
           sitetype == "OpenWater") %>%
  group_by(site, component_short) %>%
  summarise(Year1AGM = gmean(result),
            Year1sd = gmeansd(result),
            Year1Median = median(result))

# WBID Y1
agms_r_WBID_year1 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")) &
           WBID == "River" &
           sitetype == "OpenWater") %>%
  group_by(WBID, component_short) %>%
  summarise(Year1AGM = gmean(result),
            Year1sd = gmeansd(result),
            Year1Median = median(result))

# site Y2
agms_r_site_year2 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")) &
           WBID == "River" &
           sitetype == "OpenWater") %>%
  group_by(site, component_short) %>%
  summarise(Year2AGM = gmean(result),
            Year2sd = gmeansd(result),
            Year2Median = median(result))

# WBID Y2
agms_r_WBID_year2 <- dat4 %>%
  filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")) &
           WBID == "River" &
           sitetype == "OpenWater") %>%
  group_by(WBID, component_short) %>%
  summarise(Year2AGM = gmean(result),
            Year2sd = gmeansd(result),
            Year2Median = median(result))

# -----------------------------------------------------------
# B.   create tables for report ##############
# -----------------------------------------------------------
# these only need to be the WBIDs
# the parameters for assessment
assparams <- c("CHLa_C", "TN", "TP", "FECCOL", "ENTERO", "DO_p") # create a vector of the ass variables

# 1. first, merge the year1 and year2 for the lake and river into one df
# join lake years into one table
agms_l_WBID <- dplyr::full_join(agms_l_WBID_year1, agms_l_WBID_year2, by = 'component_short')
agms_l_WBID$Year1AGM <- round(agms_l_WBID$Year1AGM, digits=2)
agms_l_WBID$Year1sd <- round(agms_l_WBID$Year1sd, digits=2)
agms_l_WBID$Year1Median <- round(agms_l_WBID$Year1Median, digits=2)
agms_l_WBID$Year2AGM <- round(agms_l_WBID$Year2AGM, digits=2)
agms_l_WBID$Year2sd <- round(agms_l_WBID$Year2sd, digits=2)
agms_l_WBID$Year2Median <- round(agms_l_WBID$Year2Median, digits=2)
# join river years into one table
agms_r_WBID <- dplyr::full_join(agms_r_WBID_year1, agms_r_WBID_year2, by = 'component_short')
agms_r_WBID$Year1AGM <- round(agms_r_WBID$Year1AGM, digits=2)
agms_r_WBID$Year1sd <- round(agms_r_WBID$Year1sd, digits=2)
agms_r_WBID$Year1Median <- round(agms_r_WBID$Year1Median, digits=2)
agms_r_WBID$Year2AGM <- round(agms_r_WBID$Year2AGM, digits=2)
agms_r_WBID$Year2sd <- round(agms_r_WBID$Year2sd, digits=2)
agms_r_WBID$Year2Median <- round(agms_r_WBID$Year2Median, digits=2)

# cleanup
rm(agms_r_WBID_year1, agms_r_WBID_year2, agms_l_WBID_year1, agms_l_WBID_year2)

### lake
#### lets make a full table of all the AGMS for all the parameters,
#### won't go in the report, but may be nice to recall quickly.
#### This section can remain commented out until needed

# #### remove the wbid name columns
# agms_l_WBID_table_full <- agms_l_WBID[,c(2, 3, 4, 6, 7)]
#
# #### rename the component_short column
# colnames(agms_l_WBID_table_full)[1] <- "Parameter Abbrev."
#
# #### create output table
# knitr::kable(agms_l_WBID_table_full)
# rm(agms_l_WBID_table_full)

# 2. now, let's create a table just for assessment "ass"
# parameters that we WILL use in the report
# this code is included in the Rmarkdown summary report document in a chunk

# # remove the wbid name columns
# agms_l_WBID_table_ass <- agms_l_WBID[,c(2, 3, 4, 6, 7)]
#
# # filter by only the parameters for assessment
# agms_l_WBID_table_ass <- agms_l_WBID_table_ass %>%
#   dplyr::filter(component_short %in% assparams)
#
# # rename the component_short column
# colnames(agms_l_WBID_table_ass)[1] <- "Parameter Abbrev."
#
# # create output table
# knitr::kable(agms_l_WBID_table_ass)



### river
#### lets make a full table of all the AGMS for all the parameters,
#### won't go in the report, but may be nice to recall quickly.
#### This section can remain commented out until needed

# #### remove the wbid name columns
# agms_r_WBID_table_full <- agms_r_WBID[,c(2, 3, 4, 6, 7)]
#
# #### rename the component_short column
# colnames(agms_r_WBID_table_full)[1] <- "Parameter Abbrev."
#
# #### create output table
# knitr::kable(agms_r_WBID_table_full)
# rm(agms_r_WBID_table_full)

# 2. now, let's create a table just for assessment "ass"
# parameters that we WILL use in the report
# this code is included in the Rmarkdown summary report document in a chunk

# # remove the wbid name columns
# agms_r_WBID_table_ass <- agms_r_WBID[,c(2, 3, 4, 6, 7)]
#
# # filter by only the parameters for assessment
# agms_r_WBID_table_ass <- agms_r_WBID_table_ass %>%
#   dplyr::filter(component_short %in% assparams)
#
# # rename the component_short column
# colnames(agms_r_WBID_table_ass)[1] <- "Parameter Abbrev."
#
# # create output table
# knitr::kable(agms_r_WBID_table_ass)
#
# # cleanup
# rm(agms_r_WBID, agms_r_WBID_table_ass)
#
# # cleanup
# rm(agms_l_WBID, agms_l_WBID_table_ass)
