# Day 2: Lines

## Load libraries ----
library(sf)
library(ggplot2)
library(viridis)

## Load data ----
# Amazon
amazon_river <- st_read("Data/Amazon_hydrorivers/Amazon_hydrorivers.shp")
amazon_basin <- st_read("Data/Amazon_hydrobasin/Amazon_hydrobasin.shp")

# Project data to SAD 1969 Lambert South America
amazon_river <- amazon_river %>%
  st_transform('ESRI:102015')
amazon_basin <- amazon_basin %>%
  st_transform('ESRI:102015')

## Map ----
ggplot()+
  
  # Basin
  geom_sf(data=amazon_basin,fill="black",col=NA)+ 
  
  # Rivers by stream order
  geom_sf(data=amazon_river, aes(col=ORD_STRA,linewidth=ORD_STRA)) + 
  scale_color_viridis(option="A",begin=0.3)+
  scale_linewidth(range = c(0.5, 1.5)) + 
  
  # Labels
  labs(title = "The Amazon River Basin",
       subtitle = "02 Lines",
       caption = "Wei Jing Ang\n30 Day Map Challenge\n\nSource: HydroSHEDS")+
  
  # Theme
  theme_void()+
  theme(legend.position = "none",
        plot.title = element_text(size=16,face="bold",margin=margin(20,0,-20,20)),
        plot.subtitle = element_text(size=14,margin=margin(30,0,-30,20)),
        plot.background = element_rect(fill="#FCF8F2", color = NA),
        plot.caption = element_text(size=7,margin=margin(-25,15,15,0)))

## Save figure ----
ggsave("Output/02-WeiJing-Lines.png", plot = last_plot(), height = 6, width = 6,units = "in")

## Code used for data wrangling ----
# hydrorivers <- st_read("Data/HydroRIVERS_v10_sa_shp/HydroRIVERS_v10_sa_shp/HydroRIVERS_v10_sa.shp")
# amazon_river <- hydrorivers %>%
#   filter(MAIN_RIV == "60443230") %>%
#   filter(ORD_STRA>4)
# st_write(amazon_river, "Amazon_hydrorivers/Amazon_hydrorivers.shp")
# 
# hydrobasins <- st_read("Data/hybas_sa_lev01-06_v1c/hybas_sa_lev03_v1c.shp")
# amazon_basin <- hydrobasins %>% filter(MAIN_BAS=="6030007000")
# st_write(amazon_basin, "Amazon_hydrobasin.shp")
