# visual customizations for plots

# create label for chlorophyll plots
chla_y_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L"))
# create label for phosphorus plots
phos_y_title <- expression(paste("Total Phosphorus mg/L"))
# create label for nitrogen plots
nitro_y_title <- expression(paste("Total Nitrogen mg/L"))
# create label for fecal plots
fecal_y_title <- expression(paste("Fecal coliform CFU/100mL"))
# create label for enterococcus plots
entero_y_title <- expression(paste("Enterococcus MPN/100mL"))
# create label for temperature title
temp_y_title <- expression(paste("Temperature ("~degree*C*")"))


# create color palette for sites
sitecolours <- c(
  Micklers = "#F8766D",
  `GL1` = "#D89000",
  `GL2` = "#A3A500",
  `Lake Middle` = "#39B600",
  `GL4` = "#00BF7D",
  `Lake South` = "#00BFC4",
  `River North` = "#00B0F6",
  `GR1` = "#9590FF",
  `Guana River` = "#E76BF3",
  `GR3` = "#FF62BC"
)
