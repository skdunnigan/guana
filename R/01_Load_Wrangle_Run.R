# code is to bring in data file and do some basic data tidying
source('R/00_loadpackages.R')

# read in data file
dat <- read_xlsx("data/data.xlsx", sheet = 'Sheet1')

# inspect the data file
head(dat)
str(dat)

# set 'Result' column to numerical value
## this column contains all the numerical values from the analyses
dat$Result <- as.numeric(dat$Result)

