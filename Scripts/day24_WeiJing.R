# Day 24: Circles

## Load libraries ----
library(sf)
library(ggplot2)
library(raster)
library(classInt)
library(terra)

## Load data ----
# Amazon river
curuai <- st_read("Data/CR/CR.shp") # shapefile for single lake

# Bathymetry
bathy <- rast("Data/Bathymetry/bathymetry.tif") # SpatRaster (rater() gives RasterLayer)

# Crop bathymetry to a single lake (Curuai)
bathy_cropped <- crop(bathy, curuai)
bathy_masked <- mask(bathy_cropped, curuai)

# Resample to coarser grid
new_res <- 0.01
new_ras <- rast(ext(bathy_masked), resolution = new_res, crs = crs(bathy_masked))
curuai_rs <- resample(bathy_masked, new_ras, method = "bilinear")

# Classify raster into quantiles
curuai_values <- values(curuai_rs)[!is.na(values(curuai_rs))] # remove na for classification
curuai_breaks <- classInt::classIntervals(curuai_values, 
                                          style = "quantile", n = 4)
curuai_class <- terra::classify(curuai_rs, curuai_breaks$brks) # split by quantiles

# Convert to data frame for mapping as raster
curuai_df <- as.data.frame(curuai_class,xy=TRUE)

# Turn data frame to sf object to map as circles
curuai_sf = st_as_sf(curuai_df, coords = c("x", "y"), 
                     crs = 4326, agr = "constant")

## Map ----
ggplot()+
  
  # Curuai lake depth
  geom_sf(data = curuai_sf, aes(color=bathymetry), size=1.2) + 
  
  # Define colors
  scale_color_manual(values=amazon_palette,
                     name = "Depth (m)",
                     labels = c("2","","","7"),
                     guide=guide_legend(direction="horizontal",title.vjust = 0.8))+
  
  # Labels
  labs(title = "Curuai Lake in the Amazon",
       subtitle = "24 Only circular shapes",
       caption="Wei Jing Ang\n30 Day Map Challenge\n\nSource: Ang, W.J., Edward, P.; Enner, A., 2021, Related data for: 
Mapping floodplain bathymetry in the middle-lower Amazon River using 
inundation frequency and field control, https://doi.org/10.21979/N9/CEPE6Y.")+
  
  # Define map theme
  theme_void()+
  theme(plot.title = element_text(face="bold",size=14,margin=margin(20,20,-20,0),hjust=1),
        plot.subtitle = element_text(size=12,margin=margin(30,20,-30,0),hjust=1),
        plot.caption = element_text(size=6,hjust=0,margin=margin(-25,0,10,20)),
        plot.background = element_rect(fill="#FCF8F2",color=NA),
        panel.background = element_rect(fill="#FCF8F2",color=NA),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11),
        legend.position="inside",
        legend.position.inside = c(0.84,0.7),
        legend.title.position = "left",
        legend.text.position = "bottom",
        legend.key.height = unit(0.5, "cm"),
        legend.key.width=unit(0.5, "cm"))

## Save figure ----
ggsave("Output/24-WeiJing-Circles.png", plot = last_plot(), height = 3.8, width = 7,units = "in")
