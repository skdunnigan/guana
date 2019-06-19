# total phosphorus graphs

dat4 %>%
  filter(site == "Guana River" & component_short == "TP") %>%
  # filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30"))) %>%
  ggplot()+
  geom_ribbon(aes(x = date_sampled, ymin = 0, ymax = 0.105, fill = 'Good'))+
  geom_ribbon(aes(x = date_sampled, ymin = 0.105, ymax = max(result), fill = 'Poor'))+
  geom_hline(yintercept = 0.105, linetype='longdash', color = 'gray18', size = 1.5)+
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
  labs(x = '', y = 'Total Phosphorus (mg/L)',
       title = "Guana River")

dat4 %>%
  filter(WBID == "River" & component_short == "TP") %>%
  # filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30"))) %>%
  ggplot()+
  geom_line(aes(x = date_sampled, y = result, color = site, linetype = sitetype), size = 1) +
  geom_hline(yintercept = 0.105, linetype='longdash', color = 'gray18', size = 1.5)+
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
  labs(x = '', y = 'Total Phosphorus (mg/L)',
       title = "River Sites")

dat4 %>%
  filter(WBID == "River" & component_short == "TP") %>%
  # filter(between(date_sampled, as.POSIXct("2018-07-01"), as.POSIXct("2019-06-30"))) %>%
  ggplot()+
  # geom_line(aes(x = date_sampled, y = result, linetype = REGsites), size = 1) +
  geom_hline(yintercept = 0.105, linetype='longdash', color = 'gray18', size = 1.5)+
  geom_point(aes(x = date_sampled, y = result, color = result > 0.105, shape = sitetype), size = 4) +
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
    title = element_text(size = 13, face='bold'),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color='gray95'))+
  scale_x_datetime(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y')+
  scale_colour_manual(values = c("darkturquoise", "orange"))+
  labs(x = '', y = 'Total Phosphorus (mg/L)',
       title = "River Sites",
       caption = "Sites near water control structures are sampled for hydrologic connectivity and not used for waterbody assessments")
