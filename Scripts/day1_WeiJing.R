# Day 1: Points

## Load libraries ----
library(tidyverse)
library(sf)
library(ggplot2)
library(ggtext)
library(glue)

## Load data ----
# Mekong river basin
basin <- read_sf("Data/Mekong_basin_delta/Mekong_basin_delta.shp")
countries <- read_sf("Data/World_Countries_(Generalized)/World_Countries__Generalized_.shp")

# Dams
dams <- read_sf("Data/Dams_1055/Dams_1055.shp")

# River and lake
river1 <- read_sf("Data/River_1/FLOW1_STRA_3_China.shp") # upper course
river2 <- read_sf("Data/River_2/FLOW1_STRA_5.shp") # middle course
river3 <- read_sf("Data/River_3/FLOW3_CLASS2_TS.shp") # lower course
lake <- read_sf("Data/Lake/HydroLAKES_TS_finer.shp") # Tonle Sap Lake

# Define annotation 
annotation1 <- glue("<span style='color:#C66125;'>**Dams planned and under<br>construction**</span> are expected to<br>expand the network to **over a<br>thousand** in the next decade.")

## Map ----
ggplot() +
  
  # Map limits
  xlim(93, 110) +
  ylim(9.2, 33.2) +
  
  # Basemap (countries)
  geom_sf(data=countries, fill = "grey92", col = "#FCF8F2", lwd = 0.9)+
  
  # Mekong basin
  geom_sf(data = basin, fill = "grey30", color = "transparent") + # lower basin
  geom_sf(data = rbind(river1, river2, river3), color = "#3981A4", lwd=0.9) + # river
  geom_sf(data = lake, fill = "#3981A4", color = "transparent") + # Tonle Sap Lake
  
  # Operating dams
  geom_sf(data = dams %>% 
            filter(Status == "Operational"),
          pch = 21, aes(fill = "grey"), color = "transparent", size = 2) +
  
  # Planned/ constructing dams
  geom_sf(data = dams %>% 
            filter(Status == "Under construction" | Status == "Planned"),
          pch = 21, aes(fill = "#C66125"), color = "transparent", size = 2) +
  
  # Define colors
  scale_fill_manual(values = c(alpha("#C66125",0.8),alpha("grey",0.5)))+
  
  # Labels
  geom_text(aes(x = 109.5, y = 31.2, label = "01 Points"), size = 13/.pt, hjust=1) +
  
  geom_text(aes(x = 93, y = 10.5, label = "Wei Jing Ang\n30 Day Map Challenge"), hjust = 0, size = 5/.pt) +
  
  labs(title = "Dams in the Mekong",
       caption = "Source: Ang, W.J., Park, E., Pokhrel, Y., Tran, D.D., 
Loc, H.H., 2023, Replication Data for: Dams in the Mekong:
A comprehensive database, spatiotemporal distribution, and 
hydropower potentials, https://doi.org/10.21979/N9/ACZIJN.")+
  
  # Annotations
  geom_richtext(aes(x = 102.5, y = 29,
                    label = annotation1),
                size = 9/.pt, 
                colour = "grey30",
                hjust = 0,
                fill = NA,
                label.color = NA)+
  
  # Define map theme
  theme_void() +
  theme(plot.title = element_text(face="bold",size=15,margin=margin(0,20,-40,0),hjust=1),
        plot.caption = element_text(size=5, hjust = 0,margin=margin(-30,0,0,14)),
        # make sure to include color=NA to not have a thin grey border!
        panel.background = element_rect(fill = "#FCF8F2", color = NA),
        legend.position = "none")

## Save figure ----
ggsave("Output/01-WeiJing-Points.png", plot = last_plot(), height = 7, width = 4.5,units = "in")
