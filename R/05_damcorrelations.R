# run code before
source('R/00_loadpackages.R')
source('R/00_vis_custom.R')
source('R/01_load_wrangle.R')
source('R/01_guana_wrangle_tidy.R')
source('R/06_guana_nut_wide.R')

# pull out date information
dat_nut$month <- month(dat_nut$date_sampled)
dat_nut$day <- day(dat_nut$date_sampled)
dat_nut$year <- as.character(year(dat_nut$date_sampled))
dat_nut$date <- paste(dat_nut$year, dat_nut$month,
                          dat_nut$day, sep="-") %>%
  ymd() %>%
  as.Date()

# subset to lake south site with listed parameters
dat_LS <- dat_nut %>%
  filter(site == "Lake South") %>%
  select(date, CHLa_UnC, NO23F, SALT, WTEM)

# subset to lake south site with listed parameters
dat_RN <- dat_nut %>%
  filter(site == "River North") %>%
  select(date, CHLa_UnC, NO23F, SALT, WTEM)

# bind the columns from one into the next
dat_dam <- left_join(dat_RN, dat_LS, by = c("date"), suffix = c("_RN", "_LS"))

# chlorophyll
ggplotly(dat_dam %>%
  ggplot() +
  geom_point(aes(x = CHLa_UnC_RN, y = CHLa_UnC_LS, text = date), size = 3) +
  geom_abline(slope = 1, intercept=0) +
  theme_classic() +
  theme(legend.title = element_blank(),  # everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_text(size=13),
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
  labs(x = 'River North',
       y = 'Lake South',
       title = "Chlorophyll ug/L"), tooltip = "text")

# nitrate nitrite
ggplotly(dat_dam %>%
  ggplot() +
  geom_point(aes(x = NO23F_RN, y = NO23F_LS, text = date), size = 3) +
  geom_abline(slope = 1, intercept=0) +
  theme_classic() +
  theme(legend.title = element_blank(),  # everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_text(size=13),
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
  labs(x = 'River North',
       y = 'Lake South',
       title = "Nitrite + Nitrate mg/L"), tooltip = "text")

# salinity interactive to look at date
ggplotly(dat_dam %>%
  ggplot() +
  geom_point(aes(x = SALT_RN, y = SALT_LS, text = date), size = 3) +
  geom_abline(slope = 1, intercept=0) +
  theme_classic() +
  theme(legend.title = element_blank(),  # everything in theme is strictly aesthetics
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title.x = element_text(size=13),
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
  labs(x = 'River North',
       y = 'Lake South',
       title = "Salinity ppt"), tooltip = "text")

# temperature interactive to look at date
ggplotly(dat_dam %>%
           ggplot() +
           geom_point(aes(x = WTEM_RN, y = WTEM_LS, text = date), size = 3) +
           geom_abline(slope = 1, intercept=0) +
           theme_classic() +
           theme(legend.title = element_blank(),  # everything in theme is strictly aesthetics
                 legend.position = "bottom",
                 legend.text = element_text(size=12),
                 axis.title.x = element_text(size=13),
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
           labs(x = 'River North',
                y = 'Lake South',
                title = "Water Temperature C"), tooltip = "text")
