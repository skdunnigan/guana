# this code is to do some further data customizations unique to this dataset

# ------------------------------------------------------
# rename station codes to a site name
# ------------------------------------------------------
# This is primarily due to the stations taking on different names throughout the sampling process.

# first look at all the different station names
unique(dat$stationcode)

# these are what we want to rename, in order of latitude
# SPECIAL NOTE: Lake Middle is also FDEP Lake 3 and Guana River is also FDEP River 2

### GTMMKNUT -> Micklers
### GTMGL1NUT -> FDEP Lake1
### GTMGL2NUT -> FDEP Lake2
### GTMOLNUT -> Lake Middle
### GTMLMNUT -> Lake Middle
### GTMGL4NUT -> FDEP Lake4
### GTMDNNUT -> Lake South
### GTMLSNUT -> Lake South
### GTMDSNUT -> River North
### GTMRNNUT -> River North
### GTMGR1NUT -> FDEP River1
### GTMGRNUT -> Guana River
### GTMGR3NUT -> FDEP River3

# first make vectors for each site
Micklers <- c("GTMMKNUT", NA)
FDEPLake1 <-c("GTMGL1NUT", NA)
FDEPLake2 <-c("GTMGL2NUT", NA)
LakeMiddle <- c("GTMOLNUT", "GTMLMNUT")
FDEPLake4 <- c("GTMGL4NUT", NA)
LakeSouth <- c("GTMDNNUT", "GTMLSNUT")
RiverNorth <- c("GTMDSNUT", "GTMRNNUT")
FDEPRiver1 <- c("GTMGR1NUT", NA)
GuanaRiver <- c("GTMGRNUT", NA)
FDEPRiver3 <- c("GTMGR3NUT", NA)

# bind the vectors into a data frame
siteID <- bind_cols("Micklers" = Micklers,
                    "FDEP Lake 1" = FDEPLake1,
                    "FDEP Lake 2" = FDEPLake2,
                    "Lake Middle" = LakeMiddle,
                    "FDEP Lake 4" = FDEPLake4,
                    "Lake South" = LakeSouth,
                    "River North" = RiverNorth,
                    "FDEP River 1" = FDEPRiver1,
                    "Guana River" = GuanaRiver,
                    "FDEP River 3" = FDEPRiver3) %>%
  gather(key = "site", value = "stationcode")

# remove the vectors
rm(Micklers, LakeMiddle, LakeSouth, RiverNorth, GuanaRiver,
   FDEPLake1, FDEPLake2, FDEPLake4, FDEPRiver1, FDEPRiver3)

# merge site names with dataframe
dat2 <- merge(dat, siteID, by="stationcode", all.x=TRUE)

# ------------------------------------------------------
# add information on WBID
# ------------------------------------------------------
# Micklers, Lake Middle, Lake South, FDEP Lake 1, FDEP Lake 2, FDEP Lake 4 -> Lake
# River North, Guana River, FDEP River 1, FDEP River 3 -> River

# first make vectors for each site
Lake <- c("GTMMKNUT", "GTMOLNUT", "GTMLMNUT", "GTMDNNUT",
          "GTMLSNUT", "GTMGL1NUT", "GTMGL2NUT", "GTMGL4NUT")
River <- c("GTMDSNUT", "GTMRNNUT","GTMGRNUT", "GTMGR1NUT",
           "GTMGR3NUT", NA, NA, NA)

# bind the vectors into a data frame
WBID <- bind_cols("Lake" = Lake, "River" = River) %>%
  gather(key = "WBID", value = "stationcode")

# remove the vectors
rm(Lake,River)

# merge site names with dataframe
dat2 <- merge(dat2, WBID, by="stationcode", all.x=TRUE)

# remove the duplicate samples (also given NA as site name and WBID)
dat3<-filter(dat2, WBID %in% c("Lake", "River"))

# -----------------------------------------------------
# add time information
# -----------------------------------------------------
# another aspect of this will be to split the data into years
