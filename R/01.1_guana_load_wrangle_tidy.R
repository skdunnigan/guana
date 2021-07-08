# code is to bring in data file and do some basic data tidying
# if you have not run the 00_loadpackages.R you can uncomment the line below and run it
# source('R/00_loadpackages.R')

# if you have not run the  '00_vis_custom.R' you can uncomment the line below and run it
# gives you specific axis titles and establishes specific colors for sites.
# source('R/00_vis_custom.R')


# ----01 LOAD read in data files -----------------------------------
# janitor::clean_names() function cleans up the column header names!

# guana nutrient data
dat <- readxl::read_xlsx(here::here('data', 'guana_master_updated_2021.02.15.xlsx'),
                         sheet = 'Sheet1') %>%
  janitor::clean_names()

# data dictionary with site-specific information
dict <- readr::read_csv(here::here('data', 'guana_data_dictionary.csv')) %>%
  janitor::clean_names()

# prodss handheld data
dss <- readxl::read_xlsx(here::here('data', 'Guana_Nut_Run_Master_2019_2020.xlsx'),
                         sheet = 'Data') %>%
  janitor::clean_names()


# inspect the data file
head(dat)
str(dat)
dplyr::glimpse(dat) # this one is my favorite to use

# ---- 02 CLEAN and MERGE: important/relevant columns -----------------------------------
# check the QAQC flags in the data in the `flag` column and prepare to remove any <-3> flags
# unique(dat$flag) # uncomment to check
# then left_join() the dat data frame to the dict data frame to add additional columns
dat2 <- dat %>%
  dplyr::filter(station_code != "GTMOLNUT_dup" & !grepl("<-3>", flag)) %>%  # remove the 'duplicate' station that was only sampled for a short while and remove any rejected <-3> values
  dplyr::select(unid,
                station_code,
                date_sampled,
                component_short,
                component_long,
                result,
                remark,
                flag) %>%
  dplyr::filter(!(component_short %in% c("WIND_D", "SECCHI"))) %>% # remove wind direction and secchi
  dplyr::mutate(component_long = toupper(component_long),
                component_short = toupper(component_short))

# CAUTION: rewrites over dat2 dataframe created in previous lines
# keeps 'dat' as ORIGINAL data that you read in
dat2 <- dplyr::left_join(dat2, dict, by = "station_code")

# and to merge the dss and dict dataframes
dss2 <- dss %>%
  dplyr::mutate(site = toupper(site)) %>%
  dplyr::filter(site != "guana lake") %>%
  dplyr::rename(site_acronym = site)

# CAUTION: rewrites over dat2 dataframe created in previous lines
# keeps 'dat' as ORIGINAL data that you read in
    dict2 <- dict %>%
      dplyr::filter(end == "N")

dss2 <- dplyr::left_join(dss2, dict2, by = "site_acronym")

    rm(dict2)

# ---- 03 CHECK for spelling and duplication errors in data -----------------------------------

## This is where you can find entry errors. If you notice inconsistencies,
## you can correct them in excel, save, and run the previous lines of code again with a new file

# make sure both short and long have the same number of entries (~58)
# should be the same number of entries!
unique(dat2$component_short) # will pull out all the unique component names
unique(dat2$component_long) # will pull out the unique component names

# check for duplicates
# a warning message of 'No duplicate combinations found of...' is a good message!
janitor::get_dupes(dat2)

# review data for 'NA's (e.g. blank cells)
View(dat2 %>%
  dplyr::filter(is.na(result))
)

# ----04 WRANGLE & TIDY -----------------------------------

# set 'result' column to numerical class
# convert datetimes into POSIXct format
# pull out date information
# set 'site' and 'site_friendly' columns as factor with levels
# remove "duplicate" data

dat2 <- dat2 %>%
  dplyr::mutate(date_sampled = as.POSIXct(date_sampled,
                                          format = "%m/%d/%Y %H:%M",
                                          tz = 'America/Regina'),
                result = as.numeric(result),
                month = month(date_sampled),
                day = day(date_sampled),
                year = as.character(year(date_sampled)), # set year as a character
                site = factor(site,
                              levels = c("MICKLERS",
                                         "DEPGL1",
                                         "DEPGL2",
                                         "LAKE MIDDLE",
                                         "DEPGL4",
                                         "LAKE SOUTH",
                                         "RIVER NORTH",
                                         "DEPGR1",
                                         "GUANA RIVER",
                                         "DEPGR3")),
                site_friendly = factor(site_friendly,
                                       levels = c("Micklers",
                                                  "GL1",
                                                  "GL2",
                                                  "Lake Middle",
                                                  "GL4",
                                                  "Lake South",
                                                  "River North",
                                                  "GR1",
                                                  "Guana River",
                                                  "GR3"))
                )

# ---- 05 CHECK your data again -----------------------------------

# check components again, to be sure
unique(dat2$component_short)
unique(dat2$component_long)

# check years!
# noticed in previous data versions that data was written incorrectly for '2014'
unique(dat2$year)

# check for duplicates
# a warning message of 'No duplicate combinations found of...' is a good message!
janitor::get_dupes(dat2)

# review data for 'NA's (e.g. blank cells)
View(dat2 %>%
       dplyr::filter(is.na(result))
)
