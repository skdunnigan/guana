# run code before
# source('R/00_loadpackages.R')
# source('R/00_vis_custom.R')
# source('R/01_load_wrangle.R')
# source('R/01_guana_wrangle_tidy.R')
source('R/06_guana_nut_wide.R') # this will give the dat_nut data frame

dat_N_wide <- dat_nut

# calculate a new TN, since it is not reported for each month = TKN + NO23
dat_N_wide$TN_calc <- dat_N_wide$TKN + dat_N_wide$NO23F

# calculate DIN, which is NH4 + NO23
dat_N_wide$DIN <- dat_N_wide$NH4_N + dat_N_wide$NO23F

# calculate DON, which is DTKN - NH4
dat_N_wide$DON <- dat_N_wide$DTKN - dat_N_wide$NH4_N

# remove all but the nitrogen values
# melt back together again for plotting
dat_N <- dat_N_wide %>%
  select(site, date_sampled, month,day, year, WBID, sitetype,
         -NO23F, -TN, -TKN, -DTKN, -NH4_N,
         TN_calc, DIN, DON) %>%
  melt(id.vars = c("site", "date_sampled", "month", "day", "year", "WBID", "sitetype"),
       measure.vars = c("TN_calc", "DIN", "DON"))

# remove the wide data frame, no longer needed
rm(dat_N_wide)

# make a scatterplot figure of the different nitrogen types
dat_N %>%
  filter(site == "Guana River") %>%
  filter(between(date_sampled, as.POSIXct("2018-01-01"), as.POSIXct("2019-06-30"))) %>%
  ggplot() +
  geom_point(aes(x = date_sampled, y = value, color = variable), size = 4)+
  geom_line(aes(x = date_sampled, y = value, color = variable))+
  theme_classic()+
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
        title = element_text(size = 13, face='bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95'))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y') +
  labs(x = '', y = 'Nitrogen (mg/L)',
       title = 'Nitrogen at Guana River')

dat_N %>%
  filter(site == "Lake Middle", variable != "TN_calc") %>%
  filter(between(date_sampled, as.POSIXct("2018-01-01"), as.POSIXct("2019-06-30"))) %>%
  ggplot() +
  geom_bar(aes(x = date_sampled, y = value, fill = variable),
           stat = "identity")+
  theme_classic()+
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
        title = element_text(size = 13, face='bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='gray95'))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y')

# line lakes
dat_N %>%
  filter(WBID == "Lake" & variable == "TN_calc" & sitetype == "OpenWater") %>%
  ggplot() +
  geom_line(aes(x = date_sampled, y = value, color = site), size = 1) +
  geom_point(aes(x = date_sampled, y = value, color = site), size = 3) +
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
  labs(x = '', y = nitro_y_title,
       title = "Open Water Lake Sites")
