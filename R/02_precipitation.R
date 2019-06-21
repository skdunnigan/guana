# need to run this code first
# source('R/01_load_wrangle.R')
# source('R/01_met_wrangle.R')

# ---------------------------------------------------
# precipitation figures
# ---------------------------------------------------
# daily average precipitation graph

met_dat_daily %>%
  ggplot() +
  geom_bar(aes(x = datetime, y = dailyrain), stat = "identity", fill = 'black')+
  theme_classic()+
  theme(# everything in theme is strictly aesthetics
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=13),
    axis.ticks = element_line(color='black'),
    axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
    axis.text.y = element_text(size=12, color='black'),
    axis.ticks.x = element_line(color='black'),
    title = element_text(size = 13, face='bold'),
    plot.subtitle = element_text(size = 11, face = 'italic'),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color='gray95'))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y')+
  labs(x = 'date', y = 'rainfall (mm)',
       title = 'Average Daily Rainfall',
       subtitle = 'FloridaDEP-Stevens Tolomato River Station #872-0494')

# monthly average precipitation graph
met_dat_monthly %>%
  ggplot() +
  geom_bar(aes(x = datetime, y = monthlyrain), stat = "identity", fill = 'gray') +
  theme_classic() +
  theme(# everything in theme is strictly aesthetics
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=13),
    axis.ticks = element_line(color='black'),
    axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
    axis.text.y = element_text(size=12, color='black'),
    axis.ticks.x = element_line(color='black'),
    title = element_text(size = 13, face='bold'),
    plot.subtitle = element_text(size = 11, face = 'italic'),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(date_breaks = '1 month', date_minor_breaks = '2 weeks', date_labels='%b-%y')+
  labs(x = 'date', y = 'rainfall (mm)',
       title = 'Average Monthly Rainfall',
       subtitle = 'FloridaDEP-Stevens Tolomato River Station #872-0494')
