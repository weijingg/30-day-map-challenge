# Day 7: Vintage

## Load libraries ----
library(tidyverse)
library(sf)
library(ggplot2)
library(ggtext)
library(glue)
library(magick)
library(ggpubr)

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
# Define annotation 
annotation7 <- glue("Dams planned and under<br>construction are expected to<br>expand the network to over a<br>thousand in the next decade.")

# Load background image (from pinterest)
vintage <- image_read("Data/Vintage_bg.jpg")

ggplot() +
  
  background_image(vintage) +
  
  # Map limits
  xlim(93, 110) +
  ylim(9.2, 33.2) +
  
  # Basemap (countries)
  geom_sf(data=countries, fill = "transparent", col = alpha("#3a2317",0.4), lwd = 0.3)+
  
  # Mekong basin
  geom_sf(data = basin, fill = "#9C846B", color = "transparent") + # lower basin
  geom_sf(data = rbind(river1, river2, river3), color = "#C4C6AE", lwd=0.9) + # river
  geom_sf(data = lake, fill = "#C4C6AE", color = "transparent") + # Tonle Sap Lake
  
  # Operating dams
  geom_sf(data = dams %>% 
            filter(Status == "Operational"),
          pch = 4, aes(size = 1), color = alpha("#3a2317",0.4)) +
  
  # Planned/ constructing dams
  geom_sf(data = dams %>% 
            filter(Status == "Under construction" | Status == "Planned"),
          pch = 4, color = "#3a2317", aes(size = 2)) +
  
  # Define sizes
  scale_size_continuous(range=c(1,2),
                        name = "",
                        breaks=c(1,2),
                        labels = c("Operational","Planned and\nunder construction"))+
  
  # Labels
  geom_text(aes(x = 109.5, y = 31.2, label = "07 Vintage"), size = 13/.pt, hjust=1) +
  
  geom_text(aes(x = 93, y = 11, label = "Wei Jing Ang\n30 Day Map Challenge"), hjust = 0, size = 5/.pt) +
  
  labs(title = "Dams in the Mekong",
       caption = "Source: Ang, W.J., Park, E., Pokhrel, Y., Tran, D.D., 
Loc, H.H., 2023, Replication Data for: Dams in the Mekong:
A comprehensive database, spatiotemporal distribution, and 
hydropower potentials, https://doi.org/10.21979/N9/ACZIJN.
Vintage background was obtained from pinterest.")+
  
  # Annotations
  geom_richtext(aes(x = 102.5, y = 29,
                    label = annotation7),
                size = 9/.pt, 
                colour = "grey30",
                hjust = 0,
                fill = NA,
                label.color = NA)+
  
  # Define map theme
  theme_void() +
  theme(plot.title = element_text(face="bold",size=15,margin=margin(0,20,-38,0),hjust=1),
        plot.caption = element_text(size=5, hjust = 0,margin=margin(-40,0,0,14)),
        legend.position = "inside",
        legend.position.inside = c(0.78, 0.7),
        legend.text = element_text(size=8),
        legend.background = element_rect(fill = NA, color = NA))

## Save figure ----
ggsave("Output/07-WeiJing-Vintage.png", plot = last_plot(), height = 7, width = 4.5,units = "in")
