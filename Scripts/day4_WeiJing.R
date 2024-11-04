# Day 4: Hexagons

## Load libraries ----
library(tidyverse)
library(sf)
library(ggplot2)

## Load data ----
# USA
usa_wateruse_state <- read_csv("Data/usa_wateruse_state.csv")
hexbin <- read_sf("Data/us_states_hexgrid.geojson")

# Join to hexagons (total withdrawals in million gallons per day (Mgal/d))
usa_wateruse_hex <- left_join(hexbin %>%
                                dplyr::select("iso3166_2","geometry"),
                              usa_wateruse_state,
                              by = c("iso3166_2"="STATE")) %>%
  st_transform('EPSG:3857') # Spherical Mercator for equal hexagon sizes


# Categorize data (convert to billion gallons)
usa_wateruse_hex$bin <- cut(usa_wateruse_hex$Use,
                            breaks = c(0,2000, 5000, 10000, 20000,Inf),
                            labels = c("0-2",">2-5", ">5-10", ">10-20",">20-28"),
                            include.lowest = TRUE
)

## Map ----
# Define palette
palette_water <- c("#90e0ef","#00b4d8","#0188BC","#015BA0","#03045e")

ggplot(data=usa_wateruse_hex)+
  
  # Hexagons and state labels
  geom_sf(aes(fill=bin), color = "transparent", alpha = 0.9) +
  geom_sf_text(aes(label = iso3166_2), color = "white", size = 3, alpha = 0.6) +
  
  # Labels
  labs(title = "Water withdrawals by state in 2015",
       subtitle = "04 Hexagons",
       caption = "Wei Jing Ang\n30 Day Map Challenge\n\nSource: Dieter, C.A., Linsey, K.S., Caldwell, R.R., Harris, M.A.,
Ivahnenko, T.I., Lovelace, J.K., Maupin, M.A., Barber, N.L., 2018,
Estimated Use of Water in the United States County-Level Data for 2015,
https://doi.org/10.5066/F7TB15V5. ANDREWXHILL, us_states_hexgrid.") +
  
  # Colours
  scale_fill_manual(values = palette_water,
                    name = "Billion gallons per day",
                    guide = guide_legend(keyheight = unit(3, units = "mm"),
                                         keywidth = unit(10, units = "mm"),
                                         label.position = "bottom", title.position = "top", nrow = 1))+
  
  # Map theme
  theme_void() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.565, 0.865),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        plot.background = element_rect(fill = "#FCF8F2", color = NA),
        panel.background = element_rect(fill = "#FCF8F2", color = NA),
        legend.background = element_rect(fill = "#FCF8F2", color = NA),
        plot.caption = element_text(size=5,hjust=1, 
                                    margin = margin(b = 0.5, t = -0.7, r = 0.5, unit = "cm")),
        plot.title = element_text(size = 16,face="bold",
                                  margin = margin(b = -0.7, t = 0.6, l = 3.5, unit = "cm")),
        plot.subtitle = element_text(size = 13,
                                     margin = margin(b = -1.6, t = 1.4, l = 3.5, unit = "cm")))

## Save figure ----
ggsave("Output/04-WeiJing-Hexagons.png", plot = last_plot(), height = 5, width = 7,units = "in")

## Code used for data wrangling ----
# usa_wateruse <- read_csv("Data/Water_Use/usco2015v2.0.csv",skip=1)
# Sum water usage per state
# usa_wateruse_state <- usa_wateruse %>%
#   dplyr::select("STATE","TO-Wtotl") %>%
#   rename(Water = "TO-Wtotl") %>%
#   group_by(STATE) %>%
#   summarise(Use = sum(Water, na.rm = T))
# write.csv(usa_wateruse_state,"usa_wateruse_state.csv", row.names = FALSE)
