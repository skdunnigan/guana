# code is to bring in data file and do some basic data tidying
source('R/00_loadpackages.R')

# read in data file
dat <- read_xlsx("data/data.xlsx", sheet = 'Sheet1')

# inspect the data file
head(dat)
str(dat)

# ---------------------------------------------------
# clean up the data to make writing easier later
# ---------------------------------------------------
# organize and remove spaces from column names
names(dat) <- tolower(names(dat))
names(dat)<-make.names(names(dat),unique = TRUE) # this will remove spaces in column names and add a .

# ---------------------------------------------------
# data type housekeeping
# ---------------------------------------------------
# set 'Result' column to numerical value, this column contains all the numerical values from the analyses
dat$result <- as.numeric(dat$result)

# convert DateReceived and DateAnalyzed into POSIXct format
dat$datereceived <- as.POSIXct(dat$datereceived, format = "%m/%d/%Y %H:%M", tz = 'America/Regina')
dat$dateanalyzed <- as.POSIXct(dat$dateanalyzed, format = "%m/%d/%Y %H:%M", tz = 'America/Regina')


