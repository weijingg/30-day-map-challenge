# Day 16: Chloropleth

## Load libraries ----
library(tidyverse)
library(sf)
library(ggplot2)
library(biscale)
library(cowplot)
library(tigris)
library(glue)
library(ggtext)

## Load data ----
# USA
usa_wateruse_penn <- st_read("Data/usa_wateruse_penn/usa_wateruse_penn.shp")
usa_states <- states(cb = TRUE)

# Rename columns and project data
usa_wateruse_penn <- usa_wateruse_penn %>%
  rename(Water = "PS.Wtotl") %>%
  rename(Household = "AVE_HH_SZ") %>%
  st_transform('EPSG:2272')

# Classify data into bivariate categories using 'biscale'
bivariate <- bi_class(usa_wateruse_penn, x = Water, y = Household, style = "quantile", dim = 3)

# Basemap
usa_states_pennproj <- usa_states %>%
  st_transform('EPSG:2272')

## Map ----
# Define title and annotations using colored text
title16 <- glue("<span style='color:#6EB4B5;'>**Water usage**</span> and <span style='color:#A25F99;'>**household size**</span> in Pennsylvania")
annotation16_1 <- glue("High water usage<br>small households")
annotation16_2 <- glue("High water usage<br>large households")
annotation16_3 <- glue("Low water usage, smaller households")

# Define palette
palette <- "DkBlue2"

# Create map with colored subtitle, annotations, and leader lines
map <- ggplot() +
  theme_void(base_size = 14) +
  
  # Plot basemap
  geom_sf(data=usa_states_pennproj, fill = "grey92", col = "white", lwd = 0.9)+
  
  # Plot bivariate map
  geom_sf(data = bivariate, aes(fill = bi_class), color = NA, linewidth = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = palette, dim = 4, flip_axes = FALSE, rotate_pal = FALSE)+
  
  # Plot outline
  geom_sf(data=st_union(bivariate),fill=NA,col="white",lwd=0.8)+
  
  # Set map limits
  coord_sf(xlim = c(1189585-120000, 2814853+100000), 
           ylim = c(140908.2-320000, 1075997+200000), expand = FALSE)+
  
  # Labels
  labs(title = title16,
       subtitle = "16 Chloropleth",
       caption = "Wei Jing Ang\n30 Day Map Challenge\n\nSource: Dieter, C.A., Linsey, K.S., Caldwell, R.R., Harris, M.A., 
Ivahnenko, T.I., Lovelace, J.K., Maupin, M.A., Barber, N.L., 2018, 
Estimated Use of Water in the United States County-Level Data for 2015,
https://doi.org/10.5066/F7TB15V5. Esri Data and Maps, USA Counties.

Water usage refers to the total withdrawals from public supply per day in 2015") +
  
  # Define map theme
  theme(plot.title = element_markdown(size=16, face="bold",margin=margin(10,0,-5,50)),
        plot.subtitle = element_text(size=14, margin = margin(20,80,-75,0),hjust=1),
        plot.caption = element_text(size=6,hjust=0, margin = margin(-70,0,5,40)),
        panel.grid = element_blank(),
        panel.background = element_rect(fill="#FCF8F2",color=NA))+
  
  # Annotations
  geom_richtext(aes(x = 1220585, y = 310000,
                    label = annotation16_1),
                size = 11/.pt,
                colour = "white",
                hjust = 0,
                fill = NA,
                label.color = NA)+
  geom_richtext(aes(x = 2319085, y = 280000,
                    label = annotation16_2),
                size = 11/.pt,
                colour = "white",
                hjust = 0,
                fill = NA,
                label.color = NA)+
  geom_richtext(aes(x = 1870585, y = 900000,
                    label = annotation16_3),
                size = 11/.pt,
                colour = "white",
                hjust = 0,
                fill = NA,
                label.color = NA)

# Legend
legend <- bi_legend(pal = palette,   
                    flip_axes = FALSE,
                    rotate_pal = FALSE,
                    dim = 4,
                    xlab = "Water usage",
                    ylab = "Household size",
                    size = 7)+
  # Remove white legend background
  theme(plot.background = element_rect(fill = NA, color = NA))

# Combine map and legend using cowplot
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.7, 0.02, 0.21, 0.21)

# Display final map
finalPlot

## Save figure ----
ggsave("Output/16-WeiJing-Choropleth.png", plot = last_plot(), height = 5.5, width = 7,units = "in")

## Code used for data wrangling ----
# usa_wateruse <- read_csv("Data/Water_Use/usco2015v2.0.csv",skip=1)
# counties <- st_read("Data/USA_Counties_1016676858958179337.geojson")
# usa_wateruse_penn <- left_join(counties %>%
#                                  filter(STATE_FIPS == "42") %>%
#                                  dplyr::select("FIPS","STATE_FIPS","AVE_HH_SZ","geometry"),
#                                usa_wateruse %>%
#                                  dplyr::select("FIPS","COUNTY","STATE","PS-Wtotl"),
#                                by = "FIPS") 
# st_write(usa_wateruse_penn, "usa_wateruse_penn.shp")
