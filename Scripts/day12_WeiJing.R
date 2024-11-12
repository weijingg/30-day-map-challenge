# Day 12: Time and space

## Load libraries ----
library(tidyverse)
library(sf)
library(ggplot2)
library(ggtext)
library(glue)

## Load data ----
# Mekong river basin
basin <- read_sf("Data/Mekong_basin_delta/Mekong_basin_delta.shp")
countries_mekong <- read_sf("Data/Mekong_basin_countries/Mekong_basin_countries.shp")

# Dams
dams1980 <- read_sf("Data/1980/1980.shp") # 1980
dams1990 <- read_sf("Data/1990/1990_combi.shp") # 1990
dams2000 <- read_sf("Data/2000/2000_combi.shp") # 2000
dams2010 <- read_sf("Data/2010/2010_combi.shp") # 2010
dams2020 <- read_sf("Data/2020/2020_combi.shp") # 2020

# Define function to sum hydropower per country per decade
bycountry <- function(hpyear,year){
  hpyear %>% 
    st_drop_geometry() %>%
    dplyr::select(Capacity_M, Country) %>%
    group_by(Country) %>%
    summarise(Capacity = sum(Capacity_M, na.rm = T)) %>%
    mutate(decade = year)
}

# Run function to sum hydropower
# Note: Myanmar only appears in 2010, so add in a row for Myanmar until 2000
country1980 <- bycountry(dams1980,1980) %>%
  rbind(.,data.frame(Country = "Myanmar",Capacity = 0, decade = 1980)) 
country1990 <- bycountry(dams1990,1990) %>%
  rbind(.,data.frame(Country = "Myanmar",Capacity = 0, decade = 1990))
country2000 <- bycountry(dams2000,2000) %>%
  rbind(.,data.frame(Country = "Myanmar",Capacity = 0, decade = 2000))
country2010 <- bycountry(dams2010,2010)
country2020 <- bycountry(dams2020,2020)
countryall <- rbind(country1980,country1990,country2000,country2010,country2020)

# Add geometry to countryall
countries_mekong <- countries_mekong %>% rename(Country = COUNTRY) # rename for joining
countryall_withgeom <- left_join(countryall,
                                 countries_mekong %>% 
                                   dplyr::select(geometry,Country),
                                 by = "Country") %>% # left join geometries to country data
  st_as_sf(crs = st_crs(basin)) # turn into sf for mapping

## Map ----
# Decadal labels for the map
annotations <- data.frame(
  decade = c(1980, 1990, 2000, 2010, 2020),
  label = c("1980s", "1990s", "2000s", "2010s", "Post-2020s")
)

# Define title and subtitle 
title12 <- glue("Growing hydropower in <span style='color:#C66125;'>Laos</span> in the Mekong Basin")

# Maps of hydropower capacity over the decades
ggplot()+
  
  # Mekong delta
  geom_sf(data = basin, fill = "#D6D6FF", color = "transparent") + 
  
  # Capacity
  geom_sf(data = countryall_withgeom, aes(fill = as.factor(ntile(Capacity, 5))), # quintiles
          color = "transparent") +
  facet_wrap(~decade, nrow = 1) + # 1 map for each decade
  scale_fill_manual(values = c("#D6D6FF","#B2B4F0","#6B7ED4","#4561BF","#003995"),
                    name = "Gigawatts",
                    labels = c("0","","","","33"),
                    guide=guide_legend(direction="horizontal",title.vjust = 0.8))+
  
  # Laos
  geom_sf(data = countries_mekong %>%
            filter(NAME == "Laos"), fill = "transparent", color = "#C66125", linewidth = 1) +
  
  # Labels
  geom_text(data = annotations, aes(x = 100, y = 8, label = label), size = 9/.pt) + # decadal labels
  labs(title = title12,
       subtitle = "12 Time and space",
       caption = "Wei Jing Ang\n30 Day Map Challenge\n\nSource: Ang, W.J., Park, E., Pokhrel, Y., Tran, D.D., Loc, H.H., 2023, Replication 
Data for: Dams in the Mekong: A comprehensive database, spatiotemporal 
distribution, and hydropower potentials, https://doi.org/10.21979/N9/ACZIJN.") +
  
  # Define map theme
  theme_void() + 
  theme(
    strip.text.x = element_blank(), # remove facet wrap labels
    legend.position = "inside",
    legend.position.inside = c(0.18,-0.18),
    legend.title.position = "left",
    legend.text.position = "bottom",
    legend.key.height = unit(0.4, "cm"), # reduce legend key size
    legend.key.width = unit(0.4, "cm"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
    plot.background = element_rect(fill = "#FCF8F2", color = NA),
    plot.caption = element_text(hjust = 1, size=5, margin = margin(9,6,6,0)),
    plot.subtitle = element_text(size=12,margin=margin(10,0,10,7)),
    plot.title = element_markdown(size=12,face="bold", margin=margin(10,0,0,7))
  )

## Save figure ----
ggsave("Output/12-WeiJing-SpaceTime.png", plot = last_plot(), height = 4, width = 7,units = "in")
