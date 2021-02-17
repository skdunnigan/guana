# code is to bring in data file and do some basic data tidying
# if you have not run the 00_loadpackages.R you can uncomment the line below and run it
# source('R/00_loadpackages.R')

# if you have not run the  '00_vis_custom.R' you can uncomment the line below and run it
# gives you specific axis titles and establishes specific colors for sites.
# source('R/00_vis_custom.R')

# get your data from code below.
# uncomment if you have not run the code
# source('R/01.1_guana_load_wrangle_tidy.R')

# ---- 01 Tidy and Wrangle ----------------------------------
# Only interested in the chlorophyll data from both dataframes

dss_a <- dss2 %>%
  select(date, site_friendly, chlorophyll_rfu) %>%
  dplyr::rename(CHLA_RFU = chlorophyll_rfu)

# a few extra steps to remove unnecessary info and match this data
# to the dss data a bit better
dat_a <- dat2 %>%
  dplyr::select(date_sampled, site_friendly, component_short, result) %>%
  dplyr::filter(component_short %in% c("CHLA_C", "CHLA_UNC")) %>%
  dplyr::mutate(date = lubridate::date(date_sampled)) %>%
  dplyr::select(-date_sampled) %>%
  dplyr::group_by(date, site_friendly, component_short) %>%
  tidyr::pivot_wider(names_from = component_short,
                     values_from = result)

# combine the data into one df called `chla`
# and remove the clutter of extra dfs
chla <- dplyr::left_join(dss_a, dat_a, by = c("date", "site_friendly"))
rm(dss_a, dat_a)

# ---- 02 Regression figure -------------------------------------
# the following figures are made using the `ggpubr::` package

# corrected chlorophyll
chla %>%
  ggpubr::ggscatter(x = "CHLA_RFU", y = "CHLA_C",
                    add = "reg.line",
                    conf.int = TRUE,
                    add.params = list(color = "black",
                                      fill = "grey")) +
  stat_regline_equation(
    aes(label = paste(..rr.label..,
                      eq.label,
                      sep = "~`, `~")),
        label.y.npc = c("top")
  ) +
  theme_cowplot() +
  labs(y = chla_y_title,
       x = expression(paste("Chlorophyll ", italic("a "), "RFU")),
       title = "Corrected Chlorophyll vs RFU")


# uncorrected chlorophyll
chla %>%
  ggpubr::ggscatter(x = "CHLA_RFU", y = "CHLA_UNC",
                    add = "reg.line",
                    conf.int = TRUE,
                    add.params = list(color = "black",
                                      fill = "grey")) +
  stat_regline_equation(
    aes(label = paste(..rr.label..,
                      eq.label,
                      sep = "~`, `~")),
        label.y.npc = c("top")
  ) +
  theme_cowplot() +
  labs(y = chla_y_title,
       x = expression(paste("Chlorophyll ", italic("a "), "RFU")),
       title = "Uncorrected Chlorophyll vs RFU")


