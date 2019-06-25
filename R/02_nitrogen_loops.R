# run code before
# source('R/00_loadpackages.R')
# source('R/00_vis_custom.R')
# source('R/01_load_wrangle.R')
# source('R/01_guana_wrangle_tidy.R')
# source('R/02_totalnitrogen.R')

# by waterbody, full timeframe
dat_lake <- dat_N2 %>%
  filter(WBID == "Lake")%>%
  filter(between(date_sampled, as.POSIXct("2018-01-01"), as.POSIXct("2019-06-30")))
dat_river <- dat_N2 %>%
  filter(WBID == "River")%>%
  filter(between(date_sampled, as.POSIXct("2018-01-01"), as.POSIXct("2019-06-30")))
# list of values to loop over
uniq_sites_l = unique(dat_lake$site)
uniq_sites_r = unique(dat_river$site)

# -----------------------------------------------------------------
# Lake Sites
# -----------------------------------------------------------------

# Loop by site
for (i in uniq_sites_l) {

  N_plot = ggplot(data = subset(dat_lake, site == i)) +
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
    labs(x = '', y = 'Nitrogen (mg/L)')+
    ggtitle(i)
  ggsave(N_plot, file = paste0("output/plot_", i,"_N.png"), dpi = 120)
}

# -----------------------------------------------------------------
# River Sites, includes TN threshold
# -----------------------------------------------------------------

# Loop by site
for (i in uniq_sites_r) {

  N_plot = ggplot(data = subset(dat_river, site == i)) +
    geom_point(aes(x = date_sampled, y = value, color = variable), size = 4)+
    geom_line(aes(x = date_sampled, y = value, color = variable))+
    geom_hline(aes(yintercept = 0.65), linetype='longdash', color = 'gray18', size = 1.5)+
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
         caption = 'Horizontal line represents the threshold value for Total Nitrogen within a Class II waterbody in the state of Florida.')+
    ggtitle(i)
  ggsave(N_plot, file = paste0("output/plot_", i,"_N.png"), dpi = 120)
}
