# code is to bring in data file and do some basic data tidying
source('R/00_loadpackages.R')

# ---------------------------------------------------
# read in gtmnerr swmp nut files
# ---------------------------------------------------
swmp_dat1 <- read.csv("data/gtmnut2017.csv") %>% janitor::clean_names() %>%
  filter(station_code == "gtmpinut")
swmp_dat2 <- read.csv("data/gtmnut2018.csv") %>% janitor::clean_names() %>%
  filter(station_code == "gtmpinut")

swmp_dat1 %>%
  group_by(date_time_stamp) %>%
  summarise_all()
