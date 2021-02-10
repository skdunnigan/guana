# with facet

x <- "CHLa_C"
ylab <- chla_y_title

dat4 %>%
  select(site, WBID, component_short, result) %>%
  filter(component_short == x) %>%
  ggplot() +
  geom_point(aes(x = site, y = result, color = site), alpha = 0.5, size = 2) +
  geom_boxplot(aes(x = site, y = result, fill = site), width = 0.5) +
  facet_wrap(~WBID, drop = TRUE, scales = "free_x", strip.position = "bottom") +
  scale_x_discrete(labels = c("Micklers" = "Micklers",
                              "GL1" = "GL1",
                              "GL2" = "GL2",
                              "Lake Middle" = "Lake\nMiddle",
                              "GL4" = "GL4",
                              "Lake South" = "Lake\nSouth",
                              "River North" = "River\nNorth",
                              "GR1" = "GR1",
                              "Guana River" = "Guana\nRiver",
                              "GR3" = "GR3")) +
  scale_fill_discrete(name = "Site") +
  theme_classic() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = "white", linetype = NULL, color = "white"),
        strip.text = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14),
        legend.position = "none") +
  labs(x = "",
       y = ylab)
ggsave("output/CHLA_boxplot_facet.png")

# without facet

x <- "CHLa_C"
ylab <- chla_y_title

dat4 %>%
  select(site, WBID, component_short, result) %>%
  filter(component_short == x) %>%
  ggplot() +
  geom_point(aes(x = site, y = result, color = site), alpha = 0.5, size = 2) +
  geom_boxplot(aes(x = site, y = result, fill = site), width = 0.5, outlier.size = 3) +
  # facet_wrap(~WBID, drop = TRUE, scales = "free_x", strip.position = "bottom") +
  scale_x_discrete(labels = c("Micklers" = "Micklers",
                              "GL1" = "GL1",
                              "GL2" = "GL2",
                              "Lake Middle" = "Lake\nMiddle",
                              "GL4" = "GL4",
                              "Lake South" = "Lake\nSouth",
                              "River North" = "River\nNorth",
                              "GR1" = "GR1",
                              "Guana River" = "Guana\nRiver",
                              "GR3" = "GR3")) +
  scale_color_discrete(name = "Site") +
  theme_classic() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14),
        legend.position = "none") +
  labs(x = "",
       y = ylab)
ggsave("output/CHLAc_boxplot_nofacet.png")
