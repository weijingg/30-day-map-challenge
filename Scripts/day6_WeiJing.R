# Day 6: Raster

## Load libraries ----
library(sf)
library(ggplot2)
library(ggtext)
library(glue)
library(raster)
library(classInt)
library(terra)

## Load data ----
# Amazon river
curuai <- st_read("Data/CR/CR.shp") # shapefile for single lake

# Bathymetry
bathy <- rast("Data/Bathymetry/bathymetry.tif") # SpatRaster (rater() gives RasterLayer)
bathy_values <- values(bathy)[!is.na(values(bathy))] # remove na for classification
breaks <- classInt::classIntervals(bathy_values, 
                                   style = "quantile", n = 4)
bathy_class <- terra::classify(bathy, breaks$brks) # split by quantiles
bathy_df <- as.data.frame(bathy_class,xy=TRUE) # convert to df for mapping

# River polygon
hydropoly <- read_sf("Data/Hydropoly/hydropoly_recropped.shp")
hydropoly_add <- read_sf("Data/Hydropoly_additional/Hydropoly_additional.shp")

## Map ----
# Define annotation 
annotation6 <- glue("<span style='color:#355B7C;'>**The Amazon**</span> floods for up to **7 months**<br>each year, forming <span style='color:#DD6F78;'>**large complex lakes<br>of varying depths**</span> along its banks.")

# Define palette
amazon_palette = c("#F1A790","#DD6F78","#BF6476","#8E6781")

ggplot()+
  
  # Map limits
  xlim(-60, -54) +
  ylim(-3.8, -1.5) +
  
  # Lake depths
  geom_tile(data = bathy_df, aes(x = x, y = y, fill = bathymetry), color = NA) + 
  
  # River
  geom_sf(data=hydropoly, fill = "#355B7C", color = NA) + 
  geom_sf(data=hydropoly_add, fill = "#355B7C", color = NA) + 
  
  # Define colors
  scale_fill_manual(values=amazon_palette,
                    name = "Depth (m)",
                    labels = c("2","","","10"),
                    guide=guide_legend(direction="horizontal",title.vjust = 0.8))+
  
  # Labels
  labs(title = "The Amazon River's floodplain lakes",
       subtitle = "06 Raster",
       caption="Wei Jing Ang\n30 Day Map Challenge\n\nSource: Ang, W.J., Edward, P., Enner, A., 2021, 
Related data for: Mapping floodplain bathymetry in the 
middle-lower Amazon River using inundation frequency 
and field control, https://doi.org/10.21979/N9/CEPE6Y.")+
  
  # Annotations
  geom_richtext(aes(x = -60, y = -2.2,
                    label = annotation6),
                size = 11/.pt,
                colour = "grey30",
                hjust = 0,
                fill = NA,
                label.color = NA)+
  
  # Define map theme
  theme_void()+
  theme(plot.title = element_text(face="bold",size=14,margin=margin(20,0,-30,30)),
        plot.subtitle = element_text(size=12,margin=margin(40,0,-40,30)),
        plot.caption = element_text(size=5, hjust = 1,margin=margin(-40,10,10,0)),
        plot.background = element_rect(fill = "#FCF8F2", color = NA),
        panel.background = element_rect(fill="#FCF8F2",color=NA),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        legend.position="inside",
        legend.position.inside = c(0.5,0.07),
        legend.title.position = "left",
        legend.text.position = "bottom",
        legend.key.height = unit(0.5, "cm"),
        legend.key.width=unit(0.5, "cm"))

## Save figure ----
ggsave("Output/06-WeiJing-Raster.png", plot = last_plot(), height = 3.5, width = 8,units = "in")
