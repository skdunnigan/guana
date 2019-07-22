# total phosphorus loops

# by waterbody, full timeframe
dat_river <- dat4 %>%
  filter(WBID == "River" & component_short == "TP")
# list of values to loop over
uniq_sites_r = unique(dat_river$site)

# -----------------------------------------------------------------
# River Sites
# -----------------------------------------------------------------

# Loop by site
for (i in uniq_sites_r) {

  phos_plot = ggplot(data = subset(dat_river, site == i)) +
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
    labs(x = '', y = phos_y_title)+
    ggtitle(i)
  ggsave(phos_plot, file = paste0("output/plot_", i,"_TP.png"), dpi = 120)
}
