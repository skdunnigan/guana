# correlations

# run code before
source('R/00_loadpackages.R')
source('R/01_load_wrangle.R')
source('R/01_guana_wrangle_tidy.R')
source('R/06_guana_nut_wide.R')

# all help came from:
# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

# ------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------
# subset dat_nut from 06_guana_nut_wide.R to only include specific parameters
dat_nut_corr <- dat_nut %>%
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
# visualise the correlation matrix
# method = "square" (default)
ggcorrplot(corr,
           outline.color = "transparent",
           colors = c("#6D9EC1", "white", "#E46726")) +
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
  labs(title = 'Correlation Plot 1',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'basic')
ggsave("output/correlations/plot1.png", width = 6, height = 6, dpi = 300)

# method = "circle"
ggcorrplot(corr, method = "circle",
           colors = c("#6D9EC1", "white", "#E46726")) +
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
  labs(title = 'Correlation Plot 2.1',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'circles instead of squares')
ggsave("output/correlations/plot2.1.png", width = 7.5, height = 7.5, dpi = 300)


# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.col = "transparent",
           colors = c("#6D9EC1", "white", "#E46726")) +
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
  labs(title = 'Correlation Plot 3',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'hierarchical clustering')
ggsave("output/correlations/plot3.png", width = 6, height = 6, dpi = 300)

# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr, type = "lower",
           outline.col = "transparent",
           colors = c("#6D9EC1", "white", "#E46726")) +
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
  labs(title = 'Correlation Plot 4.1',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'lower triangle')
ggsave("output/correlations/plot4.1.png", width = 6, height = 6, dpi = 300)

# Get the lower triangle with circles
ggcorrplot(corr, type = "lower",
           method = "circle",
           colors = c("#6D9EC1", "white", "#E46726")) +
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
  labs(title = 'Correlation Plot 4.2',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'lower triangle with circles')
ggsave("output/correlations/plot4.2.png", width = 7, height = 7, dpi = 300)

# Get the lower triangle with circles
ggcorrplot(corr, type = "lower",
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
  labs(title = 'Correlation Plot 4.3',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'lower triangle with circles and theme changed')
ggsave("output/correlations/plot4.3.png", width = 7, height = 7, dpi = 300)

# Get the upper triangle
ggcorrplot(corr, type = "upper",
           outline.col = "transparent",
           colors = c("#6D9EC1", "white", "#E46726")) +
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
  labs(title = 'Correlation Plot 5',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'upper triangle')
ggsave("output/correlations/plot5.png", width = 6, height = 6, dpi = 300)

# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(corr, type = "lower",
           outline.col = "transparent",
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
  labs(title = 'Correlation Plot 6',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'theme changed')
ggsave("output/correlations/plot6.png", width = 6, height = 6, dpi = 300)

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corr, type = "lower",
           lab = TRUE, outline = "transparent",
           colors = c("#6D9EC1", "white", "#E46726")) +
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
  labs(title = 'Correlation Plot 7',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'with correlation coefficients labeled')
ggsave("output/correlations/plot7.png", width = 10, height = 10, dpi = 300)


# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(corr,
           type = "lower", p.mat = p.mat, outline = "transparent",
           colors = c("#6D9EC1", "white", "#E46726")) +
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
  labs(title = 'Correlation Plot 8',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'add correlation significance level, barring the no significant coefficient')
ggsave("output/correlations/plot8.png", width = 6, height = 6, dpi = 300)

# Leave blank on no significant coefficient
ggcorrplot(corr, p.mat = p.mat,
           type = "lower", insig = "blank", outline = "transparent",
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
  labs(title = 'Correlation Plot 9.1',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'insignificant correlations removed and theme changed')
ggsave("output/correlations/plot9.1.png", width = 6, height = 6, dpi = 300)

# Leave blank on no significant coefficient
ggcorrplot(corr, p.mat = p.mat, method = "circle",
           type = "lower", insig = "blank", outline = "transparent",
           colors = c("#6D9EC1", "transparent", "#E46726")) +
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
  labs(title = 'Correlation Plot 9.2',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'insignificant correlations removed and circles')
ggsave("output/correlations/plot9.2.png", width = 7, height = 7, dpi = 300)

# method = "circle"
# Leave blank on no significant coefficient
ggcorrplot(corr, method = "circle",
           insig = "blank", outline.color = "transparent",
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
  labs(title = 'Correlation Plot 2.2',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'circles instead of squares with insignificant values removed and theme changed')
ggsave("output/correlations/plot2.2.png", width = 7.5, height = 7.5, dpi = 300)

# method = "circle"
# Leave blank on no significant coefficient
# Add correlation coefficients
ggcorrplot(corr, p.mat = p.mat,
           insig = "blank",
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
  labs(title = 'Correlation Plot 2.3',
       caption = 'Spearman Rank correlations, complete obs',
       subtitle = 'insignificant values removed and theme changed')
ggsave("output/correlations/plot10.1.png", width = 10, height = 10, dpi = 300)

dev.off()
