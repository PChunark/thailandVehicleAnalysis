# https://www.youtube.com/watch?v=AgWgPSZ7Gp0 ####
library(tidyverse)



# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html ####

library("rnaturalearth")
library("rnaturalearthdata")
library("sf")

theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

## Data and basic plot (ggplot and geom_sf) ####
ggplot(data = world) +
  geom_sf()+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

## Map color (geom_sf) ####
ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggplot(data = world %>% filter(name == "Thailand")) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

## Projection and extent (coord_sf) ####
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))


ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)
