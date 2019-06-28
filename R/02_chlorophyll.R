# run code before
# source('R/00_loadpackages.R')
# source('R/00_vis_custom.R')
# source('R/01_load_wrangle.R')
# source('R/01_guana_wrangle_tidy.R')

# all sites first
dat4 %>%
  filter(component_short == "CHLa_UnC") %>%
  ggplot() +
  geom_line(aes(x = date_sampled, y = result, color = site), size = 1) +
  geom_point(aes(x = date_sampled, y = result, color = site), size = 3) +
  scale_colour_manual(values = sitecolours) +
  theme_classic() +
  theme(legend.title = element_blank(), # everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.ticks = element_line(color='black'),
        plot.caption = element_text(size=6, face='italic'),
        axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks.x = element_line(color='black'),
        plot.title = element_text(size = 16, face='bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95'))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y')+
  labs(x = '', y = chla_y_title,
       title = "All Guana Water Quality Sites")

# --- Lake Middle -------
# threshold plots with ribbons
dat4 %>%
  filter(site == "Lake Middle" & component_short == "CHLa_UnC") %>%
  # filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30"))) %>%
  ggplot() +
    geom_ribbon(aes(x = date_sampled, ymin = 0, ymax = 6.6, fill = 'Good'))+
    geom_ribbon(aes(x = date_sampled, ymin = 6.6, ymax = max(result), fill = 'Poor')) +
    geom_hline(yintercept = 6.6, linetype = 'longdash', color = 'gray18', size = 1.5) +
    geom_line(aes(x = date_sampled, y = result), color = 'black', size = 1) +
    geom_point(aes(x = date_sampled, y = result), color = 'black', size = 3) +
    theme_classic() +
    scale_fill_manual(name = '', values = c('Good' = '#ABD9E9', 'Poor' = '#FEC596')) +
    theme(legend.title = element_blank(),  # everything in theme is strictly aesthetics
          legend.position = "bottom",
          legend.text = element_text(size=12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=13),
          axis.ticks = element_line(color='black'),
          plot.caption = element_text(size=6, face='italic'),
          axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
          axis.text.y = element_text(size=12, color='black'),
          axis.ticks.x = element_line(color='black'),
          plot.title = element_text(size = 16, face='bold'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color='gray95')) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels = '%b') +
    labs(x = '', y = chla_y_title,
        title = "Lake Middle")
# ggsave("output/LM_Chla.png", dpi = 300)

# ----------------------------------------------------------------------
# all lake sites with open water and water control structure sites shown
# ----------------------------------------------------------------------
dat4 %>%
  filter(WBID == "Lake" & component_short == "CHLa_UnC") %>%
  ggplot() +
  geom_line(aes(x = date_sampled, y = result, color = site, linetype = sitetype), size = 1) +
  geom_hline(yintercept = 11, linetype = 'longdash', color = 'gray18', size = 1.5) +
  geom_point(aes(x = date_sampled, y = result, color = site, shape = sitetype), size = 3) +
  scale_colour_manual(values = sitecolours) +
  theme_classic() +
  theme(legend.title = element_blank(),  # everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.ticks = element_line(color='black'),
        plot.caption = element_text(size=6, face='italic'),
        axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks.x = element_line(color='black'),
        plot.title = element_text(size = 16, face='bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95')) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels = '%b-%y')+
  labs(x = '', y = chla_y_title,
       title = "Lake Sites",
       caption = "Sites near water control structures are sampled for hydrologic connectivity and not used for waterbody assessments")

# ----------------------------------------------------------------------
# all river sites with regulation sites and excluded sites shown
# ----------------------------------------------------------------------
dat4 %>%
  filter(WBID == "River" & component_short == "CHLa_UnC") %>%
  ggplot() +
  geom_line(aes(x = date_sampled, y = result, color = site, linetype = sitetype), size = 1) +
  geom_hline(yintercept = 6.6, linetype = 'longdash', color = 'gray18', size = 1.5) +
  geom_point(aes(x = date_sampled, y = result, color = site, shape = sitetype), size = 3) +
  scale_colour_manual(values = sitecolours) +
  theme_classic() +
  theme(legend.title = element_blank(),  # everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.ticks = element_line(color='black'),
        plot.caption = element_text(size=6, face='italic'),
        axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks.x = element_line(color='black'),
        plot.title = element_text(size = 16, face='bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95')) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels = '%b-%y') +
  labs(x = '', y = chla_y_title,
       title = "River Sites")

# ----------------------------------------------------------------------
# looking into values more specifically, color differentiates threshold
# ----------------------------------------------------------------------
dat4 %>%
  filter(WBID == "River" & component_short == "CHLa_UnC") %>%
  ggplot() +
  geom_hline(yintercept = 6.6, linetype = 'longdash', color = 'gray18', size = 1.5) +
  geom_point(aes(x = date_sampled, y = result, color = result > 6.6, shape = sitetype), size = 3) +
  theme_classic() +
  theme(# everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.ticks = element_line(color='black'),
        plot.caption = element_text(size=6, face='italic'),
        axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks.x = element_line(color='black'),
        plot.title = element_text(size = 16, face='bold'),
        plot.subtitle = element_text(size = 11, face = 'italic'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95'))+
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels = '%b-%y')+
  scale_colour_manual(values = c("darkturquoise", "orange")) +
  labs(x = '', y = chla_y_title,
       title = "River Sites",
       subtitle = "Threshold exceedances indicated by color change",
       caption = "Sites near water control structures are sampled for hydrologic connectivity and not used for waterbody assessments")

dat4 %>%
  filter(WBID == "Lake" & component_short == "CHLa_UnC") %>%
  ggplot() +
  geom_hline(yintercept = 11, linetype = 'longdash', color = 'gray18', size = 1.5) +
  geom_point(aes(x = date_sampled, y = result, color = result > 11, shape = sitetype), size = 3) +
  theme_classic() +
  theme(  # everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.ticks = element_line(color='black'),
        plot.caption = element_text(size=6, face='italic'),
        axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks.x = element_line(color='black'),
        plot.title = element_text(size = 16, face='bold'),
        plot.subtitle = element_text(size = 11, face = 'italic'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95'))+
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels = '%b-%y') +
  scale_colour_manual(values = c("darkturquoise", "orange"))+
  labs(x = '', y = chla_y_title,
       title = "Lake Sites",
       subtitle = "Threshold exceedances indicated by color change",
       caption = "Sites near water control structures are sampled for hydrologic connectivity and not used for waterbody assessments")

# ----------------------------------------------------------------------
# just open water sites for regulation
# ----------------------------------------------------------------------

# all open water sites
dat4 %>%
  filter(component_short == "CHLa_UnC" & sitetype == "OpenWater") %>%
  ggplot() +
  geom_line(aes(x = date_sampled, y = result, color = site), size = 1) +
  geom_point(aes(x = date_sampled, y = result, color = site), size = 3) +
  scale_colour_manual(values = sitecolours) +
  theme_classic() +
  theme(legend.title = element_blank(), # everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.ticks = element_line(color='black'),
        plot.caption = element_text(size=6, face='italic'),
        axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks.x = element_line(color='black'),
        plot.title = element_text(size = 16, face='bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95')) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels = '%b-%y') +
  labs(x = '', y = chla_y_title,
       title = "Only the Open Water Guana Water Quality Sites")

# lake
dat4 %>%
  filter(WBID == "Lake" & component_short == "CHLa_UnC" & sitetype == "OpenWater") %>%
  ggplot() +
  geom_line(aes(x = date_sampled, y = result, color = site), size = 1) +
  geom_hline(yintercept = 11, linetype = 'longdash', color = 'gray18', size = 1.5) +
  geom_point(aes(x = date_sampled, y = result, color = site), size = 3) +
  scale_colour_manual(values = sitecolours) +
  theme_classic() +
  theme(legend.title = element_blank(),  # everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.ticks = element_line(color='black'),
        plot.caption = element_text(size=6, face='italic'),
        axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks.x = element_line(color='black'),
        plot.title = element_text(size = 16, face='bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95'))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels = '%b-%y')+
  labs(x = '', y = chla_y_title,
       title = "Open Water Lake Sites")

dat4 %>%
  filter(WBID == "Lake" & component_short == "CHLa_UnC" & sitetype == "OpenWater") %>%
  ggplot() +
  geom_hline(yintercept = 11, linetype='longdash', color = 'gray18', size = 1.5) +
  geom_point(aes(x = date_sampled, y = result, color = result > 11, shape = site), size = 3) +
  theme_classic()+
  theme(# everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.ticks = element_line(color='black'),
        plot.caption = element_text(size=6, face='italic'),
        axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks.x = element_line(color='black'),
        plot.title = element_text(size = 16, face='bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95'))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels = '%b-%y')+
  scale_colour_manual(values = c("darkturquoise", "orange"))+
  labs(x = '', y = chla_y_title,
       title = "Open Water Lake Sites")

# river
dat4 %>%
  filter(WBID == "River" & component_short == "CHLa_UnC" & sitetype == "OpenWater") %>%
  ggplot() +
  geom_line(aes(x = date_sampled, y = result, color = site), size = 1) +
  geom_hline(yintercept = 6.6, linetype = 'longdash', color = 'gray18', size = 1.5) +
  geom_point(aes(x = date_sampled, y = result, color = site), size = 3) +
  scale_colour_manual(values = sitecolours) +
  theme_classic() +
  theme(legend.title = element_blank(),  # everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.ticks = element_line(color='black'),
        plot.caption = element_text(size=6, face='italic'),
        axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks.x = element_line(color='black'),
        plot.title = element_text(size = 16, face='bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95')) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels = '%b-%y') +
  labs(x = '', y = chla_y_title,
       title = "Open Water River Sites")

dat4 %>%
  filter(WBID == "River" & component_short == "CHLa_UnC" & sitetype == "OpenWater") %>%
  ggplot() +
  geom_hline(yintercept = 6.6, linetype = 'longdash', color = 'gray18', size = 1.5) +
  geom_point(aes(x = date_sampled, y = result, color = result > 6.6, shape = site), size = 3) +
  theme_classic() +
  theme(# everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.ticks = element_line(color='black'),
        plot.caption = element_text(size=6, face='italic'),
        axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks.x = element_line(color='black'),
        plot.title = element_text(size = 16, face='bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95')) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels = '%b-%y')+
  scale_colour_manual(values = c("darkturquoise", "orange"))+
  labs(x = '', y = chla_y_title,
       title = "Open Water River Sites")
