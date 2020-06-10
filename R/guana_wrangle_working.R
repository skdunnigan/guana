# ----01 LOAD read in guana nutrient data file----------------------------

dat <- readxl::read_xlsx(here::here('data', 'guana_master_07.17-19_2020.xlsx'), sheet = 'Sheet1') %>%
  janitor::clean_names()
#janitor::clean_names() function cleans up the column header names!

# inspect the data file
head(dat)
str(dat)
dplyr::glimpse(dat)

# ----02 LOAD data dictionary file----

dict <- readr::read_csv(here::here('data', 'guana_data_dictionary.csv'))

# inspect the data file
head(dict)
str(dict)
dplyr::glimpse(dict)

# ----03 TIDY data for analysis only, remove lab analyses, join with dict df----

dat_a <- dat %>%
  dplyr::filter(activity_type == "Sample") %>%
  dplyr::select(station_code, date_sampled,
                activity_type, component_short,
                component_long, result,
                flag, remark) %>%
  dplyr::left_join(dict, by = "station_code") %>%
  dplyr::mutate(result = as.numeric(result),
                site = factor(site, levels = c("MICKLERS",
                                                  "DEPGL1",
                                                  "DEPGL2",
                                                  "LAKE MIDDLE",
                                                  "DEPGL4",
                                                  "LAKE SOUTH",
                                                  "RIVER NORTH",
                                                  "DEPGR1",
                                                  "GUANA RIVER",
                                                  "DEPGR3")
                                 ),
                site_friendly = factor(site_friendly, levels = c("Micklers",
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

# ----05 chlorophyll plots----

# lake and river stacked, all sites
lake <- dat_a %>%
  dplyr::filter(component_short == "CHLa_C" & end == "N") %>%
  dplyr::filter(wbid == "Lake") %>%
  ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 11, linetype = 'longdash', color = 'gray18', size = 1.5) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0), breaks = c(25, 50, 75, 100, 125, 150)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = chla_y_title,
       x = "",
       title = "Lake",
       subtitle = "Threshold = 11 ug/L")

river <- dat_a %>%
  dplyr::filter(component_short == "CHLa_C" & end == "N") %>%
  dplyr::filter(wbid == "River") %>%
  ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 6.6, linetype = 'longdash', color = 'gray18', size = 1.5) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = chla_y_title,
       x = "",
       title = "River",
       subtitle = "Threshold = 6.6 ug/L")

cowplot::plot_grid(lake, river,
                   ncol = 1)

# averaging by waterbody
se <- function(x) {
  sd(x, na.rm = TRUE)/sqrt(length(x))
}

avgs <- dat_a %>%
  dplyr::filter(component_short == "CHLa_C" & end == "N") %>%
  dplyr::filter(location != "water_control") %>%
  dplyr::group_by(date, wbid) %>%
  dplyr::summarise(mean = mean(result, na.rm = TRUE),
                   count = n(),
                   se = se(result))

lake <- avgs %>%
  dplyr::filter(wbid == "Lake") %>%
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), fill = "grey85") +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 11, linetype = 'longdash', color = 'gray18', size = 1.5) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0), breaks = c(25, 50, 75, 100, 125, 150)) +
  scale_x_date(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = chla_y_title,
       x = "",
       title = "Lake",
       subtitle = "Threshold = 11 ug/L")


river <- avgs %>%
  dplyr::filter(wbid == "River") %>%
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), fill = "grey85") +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 6.6, linetype = 'longdash', color = 'gray18', size = 1.5) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = chla_y_title,
       x = "",
       title = "River",
       subtitle = "Threshold = 6.6 ug/L")

cowplot::plot_grid(lake, river,
                   ncol = 1)

# ----06 total P plots ----

# lake and river stacked, all sites
lake <- dat_a %>%
  dplyr::filter(component_short == "TP" & end == "N") %>%
  dplyr::filter(wbid == "Lake") %>%
  ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = phos_y_title,
       x = "",
       title = "Lake",
       subtitle = "Threshold = narrative")

river <- dat_a %>%
  dplyr::filter(component_short == "TP" & end == "N") %>%
  dplyr::filter(wbid == "River") %>%
  ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.105, linetype = 'longdash', color = 'gray18', size = 1.5) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = phos_y_title,
       x = "",
       title = "River",
       subtitle = "Threshold = 0.105 mg/L")

cowplot::plot_grid(lake, river,
                   ncol = 1)

# averaging by waterbody
se <- function(x) {
  sd(x, na.rm = TRUE)/sqrt(length(x))
}

avgs <- dat_a %>%
  dplyr::filter(component_short == "TP" & end == "N") %>%
  dplyr::filter(location != "water_control") %>%
  dplyr::group_by(date, wbid) %>%
  dplyr::summarise(mean = mean(result, na.rm = TRUE),
                   count = n(),
                   se = se(result))

lake <- avgs %>%
  dplyr::filter(wbid == "Lake") %>%
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), fill = "grey85") +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = phos_y_title,
       x = "",
       title = "Lake",
       subtitle = "Threshold = narrative")


river <- avgs %>%
  dplyr::filter(wbid == "River") %>%
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), fill = "grey85") +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.105, linetype = 'longdash', color = 'gray18', size = 1.5) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = phos_y_title,
       x = "",
       title = "River",
       subtitle = "Threshold = 0.105 ug/L")

cowplot::plot_grid(lake, river,
                   ncol = 1)

# ----07 Nitrogen----

# lake and river stacked, all sites
lake <- dat_a %>%
  dplyr::filter(component_short == "TN" & end == "N") %>%
  dplyr::filter(wbid == "Lake") %>%
  ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = nitro_y_title,
       x = "",
       title = "Lake",
       subtitle = "Threshold = narrative")

river <- dat_a %>%
  dplyr::filter(component_short == "TN" & end == "N") %>%
  dplyr::filter(wbid == "River") %>%
  ggplot(aes(x = date_sampled, y = result, color = site_friendly)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.65, linetype = 'longdash', color = 'gray18', size = 1.5) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = nitro_y_title,
       x = "",
       title = "River",
       subtitle = "Threshold = 0.65 ug/L")

cowplot::plot_grid(lake, river,
                   ncol = 1)

# averaging by waterbody
se <- function(x) {
  sd(x, na.rm = TRUE)/sqrt(length(x))
}

avgs <- dat_a %>%
  dplyr::filter(component_short == "TN" & end == "N") %>%
  dplyr::filter(location != "water_control") %>%
  dplyr::group_by(date, wbid) %>%
  dplyr::summarise(mean = mean(result, na.rm = TRUE),
                   count = n(),
                   se = se(result))

lake <- avgs %>%
  dplyr::filter(wbid == "Lake") %>%
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), fill = "grey85") +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = nitro_y_title,
       x = "",
       title = "Lake",
       subtitle = "Threshold = narrative")


river <- avgs %>%
  dplyr::filter(wbid == "River") %>%
  ggplot(aes(x = date, y = mean)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), fill = "grey85") +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.65, linetype = 'longdash', color = 'gray18', size = 1.5) +
  scale_colour_manual(name = "Site", values = sitecolours) +
  cowplot::theme_cowplot() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black')) +
  labs(y = nitro_y_title,
       x = "",
       title = "River",
       subtitle = "Threshold = 0.65 ug/L")

cowplot::plot_grid(lake, river,
                   ncol = 1)
