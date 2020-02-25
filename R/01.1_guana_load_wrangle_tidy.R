# code is to bring in data file and do some basic data tidying
# source('R/00_loadpackages.R')


# ----01 LOAD read in guana nutrient data file----------------------------

dat <- read_xlsx("data/2020-02-25_guana_master_07.17-19.xlsx", sheet = 'Sheet1') %>% janitor::clean_names()
#janitor::clean_names() function cleans up the column header names!

# inspect the data file
head(dat)
str(dat)
dplyr::glimpse(dat)


# ----02a guana data tidying, part 1 -----------------------------------

# set 'result' column to numerical value, this column contains all the numerical values from the analyses
cols.num <- c("result", "mdl", "pql", "mrl", "dilution")
dat[cols.num] <- sapply(dat[cols.num],as.numeric)
sapply(dat, class)
rm(cols.num)

# convert datetimes into POSIXct format
# pull out date information
# remove "duplicate" data

dat <- dat %>%
  dplyr::mutate(date_sampled = as.POSIXct(date_sampled,
                                          format = "%m/%d/%Y %H:%M",
                                          tz = 'America/Regina'),
                date_received = as.POSIXct(date_received,
                                           format = "%m/%d/%Y %H:%M",
                                           tz = 'America/Regina'),
                date_analyzed = as.POSIXct(date_analyzed,
                                           format = "%m/%d/%Y %H:%M",
                                           tz = 'America/Regina'),
                month = month(date_sampled),
                day = day(date_sampled),
                year = as.character(year(date_sampled))
                ) %>%
  dplyr::filter(station_code != "GTMOLNUT_dup")

# dont forget to check the unique component names, uncomment code below if necessary
# should be the same number of both!
unique(dat$component_short)
unique(dat$component_long)

# ----02b rename station codes to a site name--------------------------------

# This is due to the stations taking on different names throughout the sampling period.

# first look at all the different station names
unique(dat$station_code)

# these are what we want to rename, in order of latitude
# SPECIAL NOTE: Lake Middle is also GL3 and Guana River is also GR2
# but these names do not make it into the data file, DEP keeps those separate for WIN

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

#  make vectors for each site
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
  tidyr::pivot_longer(1:10, names_to = "site", values_to = "station_code")

# remove the vectors, we don't need them anymore
rm(Micklers, LakeMiddle, LakeSouth, RiverNorth, GuanaRiver,
   GL1, GL2, GL4, GR1, GR3)

# merge site names with dataframe
dat2 <- merge(dat, siteID, by = "station_code", all.x = TRUE)

# clean up the global environment
rm(siteID)

# ----02c add information on WBID--------------------------------------

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

# clean up the global environment
rm(WBID)

# ----set site and WBID to be factors with levels, to help with ordering----
dat3 <- dat2 %>%
  dplyr::mutate(site = factor(site, levels = c("Micklers",
                                               "GL1",
                                               "GL2",
                                               "Lake Middle",
                                               "GL4",
                                               "Lake South",
                                               "River North",
                                               "GR1",
                                               "Guana River",
                                               "GR3")),
                WBID = factor(WBID, levels = c("Lake", "River")),
  )


# ----02d add information on WBID sites used for regulation----

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
  tidyr::pivot_longer(1:2, names_to = "sitetype", values_to = "site")

# remove the vectors, we don't need them anymore
rm(Open_water, WaterControl)

# merge site names with dataframe
dat4 <- merge(dat3, REGsites, by = "site", all.x=TRUE)

# clean up the global environment
rm(REGsites)

# clean up old data frames
rm(dat2, dat3)

# remove rejected flagged values <-3>
# dat4.5 <- dat4 %>%
#   filter(flag != "<-3> (CHB)")
# you can also remove by a specific row number, but you have to find out which one.
dat4 <- dat4[-c(748),]

unique(dat4$flag)
