# code is to bring in data file and do some basic data tidying
source('R/00_loadpackages.R')

# read in data file
dat <- read_xlsx("data/data.xlsx", sheet = 'Sheet1') %>% janitor::clean_names()
#janitor and clean_names() function cleans up the column header names!

# inspect the data file
head(dat)
str(dat)

# ---------------------------------------------------
# data type housekeeping
# ---------------------------------------------------
# set 'result' column to numerical value, this column contains all the numerical values from the analyses
dat$result <- as.numeric(dat$result)
dat$mdl <- as.numeric(dat$mdl)
dat$mrl <- as.numeric(dat$mrl)

# convert date_received and date_analyzed into POSIXct format
dat$date_received <- as.POSIXct(dat$date_received, format = "%m/%d/%Y %H:%M", tz = 'America/Regina')
dat$date_analyzed <- as.POSIXct(dat$date_analyzed, format = "%m/%d/%Y %H:%M", tz = 'America/Regina')

# dont forget to check the unique component names, uncomment code below if necessary
# should be the same number of both!
# unique(dat$component_short)
# unique(dat$component_long)

