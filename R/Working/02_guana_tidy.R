# this code is to do some further data customizations unique to this dataset
# source('R/01_load_wrangle.R')

# -----------------------------------------------------
# add time information
# -----------------------------------------------------
# another aspect of this will be to split the data into years
# year1_dat <- dat4 %>%
#   filter(between(date_sampled, as.POSIXct("2017-07-01"), as.POSIXct("2018-06-30")))
# year2_dat <- dat4 %>%
#   filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30")))
dat$month <- month(dat$date_sampled)
dat$day <- day(dat$date_sampled)
dat$year <- as.character(year(dat$date_sampled))

# ------------------------------------------------------
# rename station codes to a site name
# ------------------------------------------------------
# This is primarily due to the stations taking on different names throughout the sampling process.

# first look at all the different station names
unique(dat$station_code)

# these are what we want to rename, in order of latitude
# SPECIAL NOTE: Lake Middle is also GL3 and Guana River is also GR2

### GTMMKNUT -> Micklers
### GTMGL1NUT -> GL1
### GTMGL2NUT -> GL2
### GTMOLNUT -> Lake Middle
### GTMLMNUT -> Lake Middle
### GTMGL4NUT -> GL4
### GTMDNNUT -> Lake South
### GTMLSNUT -> Lake South
### GTMDSNUT -> River North
### GTMRNNUT -> River North
### GTMGR1NUT -> GR1
### GTMGRNUT -> Guana River
### GTMGR3NUT -> GR3

# first make vectors for each site
Micklers <- c("GTMMKNUT", NA)
GL1 <-c("GTMGL1NUT", NA)
GL2 <-c("GTMGL2NUT", NA)
LakeMiddle <- c("GTMOLNUT", "GTMLMNUT")
GL4 <- c("GTMGL4NUT", NA)
LakeSouth <- c("GTMDNNUT", "GTMLSNUT")
RiverNorth <- c("GTMDSNUT", "GTMRNNUT")
GR1 <- c("GTMGR1NUT", NA)
GuanaRiver <- c("GTMGRNUT", NA)
GR3 <- c("GTMGR3NUT", NA)

# bind the vectors into a data frame
siteID <- bind_cols("Micklers" = Micklers,
                    "GL1" = GL1,
                    "GL2" = GL2,
                    "Lake Middle" = LakeMiddle,
                    "GL4" = GL4,
                    "Lake South" = LakeSouth,
                    "River North" = RiverNorth,
                    "GR1" = GR1,
                    "Guana River" = GuanaRiver,
                    "GR3" = GR3) %>%
  gather(key = "site", value = "station_code")

# remove the vectors, we don't need them anymore
rm(Micklers, LakeMiddle, LakeSouth, RiverNorth, GuanaRiver,
   GL1, GL2, GL4, GR1, GR3)

# merge site names with dataframe
dat2 <- merge(dat, siteID, by="station_code", all.x=TRUE)

# set site as factor with levels, helps with ordering
dat2$site <- as.factor(dat2$site)
dat2$site <- factor(dat2$site, levels = c("Micklers",
                                          "GL1",
                                          "GL2",
                                          "Lake Middle",
                                          "GL4",
                                          "Lake South",
                                          "River North",
                                          "GR1",
                                          "Guana River",
                                          "GR3"))

# clean up the global environment
rm(siteID)
# ------------------------------------------------------
# add information on WBID
# ------------------------------------------------------
# Micklers, Lake Middle, Lake South, GL1, GL2, GL4 -> Lake
# River North, Guana River, GR, GR3 -> River

# first make vectors for each site
Lake <- c("GTMMKNUT", "GTMOLNUT", "GTMLMNUT", "GTMDNNUT",
          "GTMLSNUT", "GTMGL1NUT", "GTMGL2NUT", "GTMGL4NUT")
River <- c("GTMDSNUT", "GTMRNNUT","GTMGRNUT", "GTMGR1NUT",
           "GTMGR3NUT", NA, NA, NA)

# bind the vectors into a data frame
WBID <- bind_cols("Lake" = Lake, "River" = River) %>%
  gather(key = "WBID", value = "station_code")

# remove the vectors, we don't need them
rm(Lake,River)

# merge site names with dataframe
dat2 <- merge(dat2, WBID, by="station_code", all.x=TRUE)

# remove the duplicate samples (also given NA as site name and WBID)
dat3<-filter(dat2, WBID %in% c("Lake", "River"))

# set as factor with levels in order
dat3$WBID <- as.factor(dat3$WBID)
dat3$WBID <- factor(dat3$WBID, levels = c("Lake", "River"))

# clean up the global environment
rm(WBID)

# ------------------------------------------------------
# add information on WBID sites used for regulation
# ------------------------------------------------------
# Lake Middle, GL1, GL2, GL4 -> Lake
# Guana River, GR1, GR3 -> River

# first make vectors for each site
Open_water <- c("Lake Middle", "GL1",
              "GL2", "GL4",
              "Guana River", "GR1",
               "GR3", NA)
WaterControl <- c("Micklers", "Lake South",
              "River North", NA, NA, NA, NA, NA)

# bind the vectors into a data frame
REGsites <- bind_cols("OpenWater" = Open_water, "WaterControl" = WaterControl) %>%
  gather(key = "sitetype", value = "site")

# remove the vectors, we don't need them anymore
rm(Open_water, WaterControl)

# merge site names with dataframe
dat4 <- merge(dat3, REGsites, by = "site", all.x=TRUE)

# clean up the global environment
rm(REGsites)

# clean up old data frames
rm(dat2, dat3)

# remove rejected flagged values <-3>
dat4 <- dat4[-c(559),]
