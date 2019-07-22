# run code before
source('R/00_loadpackages.R')
source('R/00_vis_custom.R')
source('R/01_load_wrangle.R')
source('R/01_guana_wrangle_tidy.R')
source('R/06_guana_nut_wide.R') # this will give the dat_nut data frame used in this script

# subset to lake south site with listed parameters
dat_lake <- dat_nut %>%
  filter(WBID == "Lake")

dat_lake$TN_calc <- dat_lake$TKN + dat_lake$NO23F
dat_lake$NP <- dat_lake$TN_calc/dat_lake$TP

ggplotly(dat_lake %>%
           ggplot() +
           geom_point(aes(x = date_sampled, y = NP, color = NP > 7, text = site), size = 3) +
           geom_abline(slope = 1, intercept = 0) +
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
           labs(x = 'Date',
                y = 'N:P',
                title = "N:P"), tooltip = c("text", "date_sampled"))
