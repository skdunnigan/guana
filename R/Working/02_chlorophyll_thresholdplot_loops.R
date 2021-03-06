# chlorophyll loops

# by waterbody, full timeframe
dat_lake <- dat4 %>%
  filter(WBID == "Lake" & component_short == "CHLa_UnC")
dat_river <- dat4 %>%
  filter(WBID == "River" & component_short == "CHLa_UnC")
# list of values to loop over
uniq_sites_l = unique(dat_lake$site)
uniq_sites_r = unique(dat_river$site)

# -----------------------------------------------------------------
# Lake Sites
# -----------------------------------------------------------------

# Loop by site
for (i in uniq_sites_l) {

  chl_plot = ggplot(data = subset(dat_lake, site == i)) +
    geom_ribbon(aes(x = date_sampled, ymin = 0, ymax = 11, fill = 'Good'))+
    geom_ribbon(aes(x = date_sampled, ymin = 11, ymax = max(result), fill = 'Poor'))+
    geom_hline(yintercept = 6.6, linetype='longdash', color = 'gray18', size = 1.5)+
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
    labs(x = '', y = chla_y_title)+
  ggtitle(i)
  ggsave(chl_plot, file = paste0("output/plot_", i,"_CHLa.png"), dpi = 120)
}

# -----------------------------------------------------------------
# River Sites
# -----------------------------------------------------------------

# Loop by site
for (i in uniq_sites_r) {

  chl_plot = ggplot(data = subset(dat_river, site == i)) +
    geom_ribbon(aes(x = date_sampled, ymin = 0, ymax = 6.6, fill = 'Good'))+
    geom_ribbon(aes(x = date_sampled, ymin = 6.6, ymax = max(result), fill = 'Poor'))+
    geom_hline(yintercept = 6.6, linetype='longdash', color = 'gray18', size = 1.5)+
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
    labs(x = '', y = chla_y_title)+
    ggtitle(i)
  ggsave(chl_plot, file = paste0("output/plot_", i,"_CHLa.png"), dpi = 120)
}
