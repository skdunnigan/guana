# code is to bring in data file and do some basic data tidying
# if you have not run the 00_loadpackages.R you can uncomment the line below and run it
source('R/00_loadpackages.R')

# if you have not run the  '00_vis_custom.R' you can uncomment the line below and run it
# gives you specific axis titles and establishes specific colors for sites.
source('R/00_vis_custom.R')


# ----01a LOAD read in data files -----------------------------------
# janitor::clean_names() function cleans up the column header names!

# guana nutrient data
dat <- readxl::read_xlsx(here::here('data', 'guana_master_07.17-20.xlsx'),
                         sheet = 'Sheet1') %>%
  janitor::clean_names()

# data dictionary with site-specific information
dict <- readr::read_csv(here::here('data', 'guana_data_dictionary.csv')) %>%
  janitor::clean_names()


# inspect the data file
head(dat)
str(dat)
dplyr::glimpse(dat) # this one is my favorite to use

# ---- 01b CLEAN and MERGE: important/relevant columns -----------------------------------
# then left_join() the dat data frame to the dict data frame to add additional columns
dat2 <- dat %>%
  dplyr::filter(station_code != "GTMOLNUT_dup") %>%  # remove the 'duplicate' station that was only sampled for a short while
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


# ---- 01c CHECK for spelling and duplication errors in data -----------------------------------

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

# ----02a WRANGLE & TIDY -----------------------------------

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

# ---- 02b CHECK your data again -----------------------------------

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

# ----03 timeseries-all sites function -----------------------------------


all_sites <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes

p <- dat2 %>%
  dplyr::filter(component_short == param & end == "N") %>%
  ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = axis_title,
       x = "",
       title = paste(param))

p
}

# use the function to create full timeseries plots of whatever parameter you want, examples below

all_sites("CHLA_C", chla_y_title) # corrected chlorophyll plot
all_sites("TP", phos_y_title) # total phosphorus
all_sites("TN", nitro_y_title) # total nitrogen (this may be incomplete because some values may end up needing to be calculated)
all_sites("ENTERO", entero_y_title) # enterococcus
all_sites("FECCOL", fecal_y_title) # fecal coliform

# ---- 03a EXAMPLES of how to further customize -----------------------------------

# not all parameters have a special y_axis title,
# if you want to label axis that isn't a value from the 00_vis_custom.R,
# you can do so using quotes. As an example:
all_sites("SALT", "Salinity (psu)")

# to change title name if you don't like default:
all_sites("CHLA_C", chla_y_title) +
  labs(title = "New title, you'll want to change me")

# you can also change the scales of the y axis
# (notice the difference between this output and the one from the previous line of code)
all_sites("CHLA_C", chla_y_title) +
  scale_y_continuous(breaks = c(25, 50, 75, 100, 125, 150, 200))

# to do both:
all_sites("CHLA_C", chla_y_title) +
  scale_y_continuous(breaks = c(25, 50, 75, 100, 125, 150, 200))+
  labs(title = "New title, you'll want to change me")

# ----04 timeseries-split by waterbody function -----------------------------------
# you are going to want to have lake and river split into two separate graphs

waterbody_sites <- function(param, lake_threshold, river_threshold, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes
  # lake_threshold - as number
  # river_threshold - as number

lake <- dat2 %>%
  dplyr::filter(component_short == param & end == "N") %>%
  dplyr::filter(wbid == "Lake") %>%
  ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = lake_threshold, linetype = 'longdash', color = 'gray18', size = 1.5) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = axis_title,
       x = "",
       title = "Lake",
       subtitle = paste0("Threshold =", lake_threshold))

river <- dat2 %>%
  dplyr::filter(component_short == param & end == "N") %>%
  dplyr::filter(wbid == "River") %>%
  ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = river_threshold, linetype = 'longdash', color = 'gray18', size = 1.5) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = axis_title,
       x = "",
       title = "River",
       subtitle = paste0("Threshold =", lake_threshold))

p <- cowplot::plot_grid(lake, river,
                   ncol = 1)

p

}

# ---- 04a EXAMPLES-----------------------------------
waterbody_sites("CHLA_C", 11, 6.6, chla_y_title)

waterbody_sites("CHLA_C", lake_threshold = "", 6.6, chla_y_title)

