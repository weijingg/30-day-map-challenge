# Day 26: Projections

## Load libraries ----
library(sf)
library(ggplot2)

## Load data ----
# The coordinates for the spilhaus projection was extracted from github
# https://github.com/rtlemos/spilhaus/tree/main
spilhaus_coord = read.csv("Data/spilhaus_coord.csv")

# Map ----
# Annotation data frame
annotation26 <- data.frame(
  x = c(4200000,1000000,-7500000),
  y = c(-6000000,1500000,4000000),
  label = c("Pacific\nOcean", "Indian\nOcean","Atlantic\nOcean")
)

# Map
ggplot(data=spilhaus_coord, aes(x=x, y=y)) +
  geom_raster(fill="#008080") +
  coord_equal() +
  
  # Labels
  geom_text(data=annotation26, aes(x=x, y=y, label=label),
            color="white", 
            size=4,
            fontface="bold")+
  labs(title = "Spilhaus Projection",
       subtitle = "26 Map projections",
       caption = "Wei Jing Ang\n30 Day Map Challenge\n\nSource: rtlemos, spilhaus") +
  
  # Theme
  theme_void()+
  theme(plot.background = element_rect(fill = "#FCF8F2", color = NA),
        plot.title = element_text(size=16,face="bold",
                                  margin = margin(b = -2.6, t = 1.5, l = 7.2, unit = "cm")),
        plot.subtitle = element_text(size=14,
                                     margin = margin(b = -2.6, t = 3, l = 7.2, unit = "cm")),
        plot.caption = element_text(size=8,hjust=0,
                                    margin = margin(b = 1.2, t = -2, l = 0.8, unit = "cm")))

## Save figure ----
ggsave("Output/26-WeiJing-Projections.png", plot = last_plot(), height = 6, width = 6,units = "in")
