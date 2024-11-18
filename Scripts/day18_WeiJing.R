# Day 18: 3D

## Load libraries ----
library(sf)
library(ggplot2)
library(raster)
library(rayshader)
library(magick)
library(terra)

## Load data ----
# Amazon river
curuai <- st_read("Data/CR/CR.shp") # shapefile for single lake

# Bathymetry
bathy <- rast("Data/Bathymetry/bathymetry.tif") # SpatRaster (rater() gives RasterLayer)

# Crop bathymetry to a single lake (Curuai)
bathy_cropped <- crop(bathy, curuai)
bathy_masked <- mask(bathy_cropped, curuai)

# Convert to matrix to use rayshader package
matrix = raster_to_matrix(bathy_masked)
matrix = 0-matrix*800 # exaggerate depth and convert to negative elevation

## Map ----
# Add shadow and water to 3D plot
matrix %>%
  sphere_shade(sunangle = 40, zscale = 200, texture = "imhof2") %>%
  add_shadow(ray_shade(matrix, zscale = 200), 0.5) %>% #,
  plot_3d(matrix, zscale = 500, fov = 0, theta = 10, zoom = 0.4, phi = 45, windowsize = c(1000, 400),water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5,background="#FCF8F2") 
Sys.sleep(0.2)
render_snapshot("Data/Curuai_3D.png",clear=TRUE) # save snapshot

# Load snapshot and convert to raster
img <- image_read("Data/Curuai_3D.png")
img_gg <- as.raster(img)

# Label using ggplot
ggplot() +
  
  # Snapshot (now a raster)
  annotation_raster(img_gg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  
  # Labels
  labs(title = "Curuai Lake in the Amazon",
       subtitle = "18 3D",
       caption="Wei Jing Ang\n30 Day Map Challenge\n\nSource: Ang, W.J., Edward, P.; Enner, A., 2021, Related data for: 
Mapping floodplain bathymetry in the middle-lower Amazon River using 
inundation frequency and field control, https://doi.org/10.21979/N9/CEPE6Y.")+
  
  # Define map theme
  theme_void()+
  theme(aspect.ratio = 2/5,
        plot.title = element_text(face="bold",size=14,margin=margin(20,20,-25,0),hjust=1),
        plot.subtitle = element_text(size=12,margin=margin(40,20,-40,0),hjust=1),
        plot.caption = element_text(size=6,hjust=0,margin=margin(-25,0,10,20)),
        plot.background = element_rect(fill="#FCF8F2",color=NA))

## Save figure ----
ggsave("Output/18-WeiJing-3D.png", plot = last_plot(), height = 3.5, width = 7,units = "in")
