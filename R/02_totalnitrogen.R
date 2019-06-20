# Nitrogen information
dat_N <- dat4 %>%
  select(site, date_sampled, component_short,
         result, month, day, year, WBID, sitetype) %>%
  dplyr::filter(component_short %in% c("TN", "NO23F", "TKN", "DTKN", "NH4_N"))

dat_N_wide <- dat_N %>%
  group_by(site, date_sampled) %>%
  spread(key = component_short, value = result) %>%
  ungroup() %>%
  as.data.frame()
str(dat_N_wide)
View(dat_N_wide)

# calculate a new TN, since it is not reported for each month = TKN + NO23
dat_N_wide$TN_calc <- dat_N_wide$TKN + dat_N_wide$NO23F

# calculate DIN, which is NH4 + NO23
dat_N_wide$DIN <- dat_N_wide$NH4_N + dat_N_wide$NO23F

# calculate DON, which is DTKN - NH4
dat_N_wide$DON <- dat_N_wide$DTKN - dat_N_wide$NH4_N

# melt it all back together again
dat_N2 <- dat_N_wide %>%
  select(-NO23F, -TN, -TKN, -DTKN, -NH4_N) %>%
  melt(id.vars = c("site", "date_sampled", "month", "day", "year", "WBID", "sitetype"),
       measure.vars = c("TN_calc", "DIN", "DON"))

# make a scatterplot figure of the different nitrogen types
dat_N2 %>%
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

dat_N2 %>%
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


# total nitrogen graphs

dat4 %>%
  filter(site == "Guana River" & component_short == "TN") %>%
  # filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30"))) %>%
  ggplot()+
  geom_ribbon(aes(x = date_sampled, ymin = 0, ymax = 0.65, fill = 'Good'))+
  geom_ribbon(aes(x = date_sampled, ymin = 0.65, ymax = max(result), fill = 'Poor'))+
  geom_hline(yintercept = 0.65, linetype='longdash', color = 'gray18', size = 1.5)+
  geom_line(aes(x = date_sampled, y = result), color = 'black', size = 1) +
  geom_point(aes(x = date_sampled, y = result), color = 'black', size = 3) +
  theme_classic()+
  scale_fill_manual(name = '', values = c('Good' = '#ABD9E9', 'Poor' = '#FEC596'))+
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
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b')+
  labs(x = '', y = 'Total Nitrogen (mg/L)',
       title = "Guana River")

# all lake sites with open water and water control structure sites shown
dat4 %>%
  filter(WBID == "Lake" & component_short == "TN") %>%
  # filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30"))) %>%
  ggplot()+
  geom_line(aes(x = date_sampled, y = result, color = site, linetype = sitetype), size = 1) +
  # geom_hline(yintercept = 0.65, linetype='longdash', color = 'gray18', size = 1.5)+
  geom_point(aes(x = date_sampled, y = result, color = site, shape = sitetype), size = 3) +
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
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y')+
  labs(x = '', y = 'Total Nitrogen (mg/L)',
       title = "Lake Sites",
       caption = "Sites near water control structures are sampled for hydrologic connectivity and not used for waterbody assessments")

# all river sites with regulation sites and excluded sites shown
dat4 %>%
  filter(WBID == "River" & component_short == "TN") %>%
  # filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30"))) %>%
  ggplot()+
  geom_line(aes(x = date_sampled, y = result, color = site, linetype = sitetype), size = 1) +
  geom_hline(yintercept = 0.65, linetype='longdash', color = 'gray18', size = 1.5)+
  geom_point(aes(x = date_sampled, y = result, color = site, shape = sitetype), size = 3) +
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
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y')+
  labs(x = '', y = 'Total Nitrogen (mg/L)',
       title = "River Sites")
