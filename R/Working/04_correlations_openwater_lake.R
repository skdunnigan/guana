# correlations of only open water sites

# run code before
# source('R/00_loadpackages.R')
# source('R/01_load_wrangle.R')
# source('R/01_guana_wrangle_tidy.R')
# source('R/06_guana_nut_wide.R')

# all help came from:
# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

# ------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------
# subset dat_nut from 06_guana_nut_wide.R to only include specific parameters

dat_nut_corr <- dat_nut %>%
  filter(sitetype == "OpenWater" & WBID == "Lake") %>%
  select(ATEMP, DO_p, DO, SALT, SpCond, pH, SECCHI, WDEPTH, WTEM,
         CHLa_UnC, ENTERO, FECCOL, DTKN, NO23F, NH4_N, TKN, TN, TP,
         -site, -latitude, -longitude, -station_code, -date_sampled, -sitetype, -WBID)
dat_nut_corr <- data.matrix(dat_nut_corr) # make it a data frame
# compute correlation matrix
corr <- cor(dat_nut_corr, method = "spearman", use = "complete.obs")
head(dat_nut_corr) # be sure to check it

# compute a matrix of correlation p-values
p.mat <- cor_pmat(dat_nut_corr)
head(p.mat[,1:4]) # be sure to check it

# ------------------------------------------------------------------
# correlation matrix visualisation
# ------------------------------------------------------------------
ggcorrplot(corr,
           outline.color = "transparent",
           method = "circle",
           colors = c("#6D9EC1", "white", "#E46726")) +
  theme_classic() +
  theme(# everything in theme is strictly aesthetics
    legend.text = element_text(size=12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color='black'),
    plot.caption = element_text(size=6, face='italic'),
    axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
    axis.text.y = element_text(size=12, color='black'),
    axis.ticks.x = element_line(color='black'),
    plot.title = element_text(size = 16, face='bold'),
    plot.subtitle = element_text(size = 11, face = 'italic')) +
  labs(title = 'Correlation Plot 1, open water lake sites',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'basic')
ggsave("output/correlations/openwater_1_lake.png", width = 7.5, height = 7.5, dpi = 300)

# Leave blank on no significant coefficient
ggcorrplot(corr, p.mat = p.mat,
           insig = "blank",
           method = "circle",
           outline.color = "transparent",
           colors = c("#6D9EC1", "transparent", "#E46726")) +
  theme_classic() +
  theme(# everything in theme is strictly aesthetics
    legend.text = element_text(size=12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color='black'),
    plot.caption = element_text(size=6, face='italic'),
    axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
    axis.text.y = element_text(size=12, color='black'),
    axis.ticks.x = element_line(color='black'),
    plot.title = element_text(size = 16, face='bold'),
    plot.subtitle = element_text(size = 11, face = 'italic')) +
  labs(title = 'Correlation Plot 2, open water lake sites',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'insignificant correlations removed and theme changed')
ggsave("output/correlations/openwater_2_lake.png", width = 7.5, height = 7.5, dpi = 300)

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corr, p.mat = p.mat,
           type = "lower",
           insig = "blank",
           # method = "circle",
           outline.color = "transparent",
           lab = TRUE,
           colors = c("#6D9EC1", "transparent", "#E46726")) +
  theme_classic() +
  theme(# everything in theme is strictly aesthetics
    legend.text = element_text(size=12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color='black'),
    plot.caption = element_text(size=6, face='italic'),
    axis.text.x = element_text(angle = 90, vjust=0.3, size=12, color='black'),
    axis.text.y = element_text(size=12, color='black'),
    axis.ticks.x = element_line(color='black'),
    plot.title = element_text(size = 16, face='bold'),
    plot.subtitle = element_text(size = 11, face = 'italic')) +
  labs(title = 'Correlation Plot 3, open water lake sites',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'insignificant correlations removed and correlation coefficients')
ggsave("output/correlations/openwater_3_lake.png", width = 8, height = 8, dpi = 300)
dev.off()
