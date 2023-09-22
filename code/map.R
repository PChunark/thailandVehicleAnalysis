# https://www.youtube.com/watch?v=AgWgPSZ7Gp0 ####
library(tidyverse)

# They used the vaccination in EU
# EUVAX

mapdata <- map_data("world")
view(mapdata)

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

# https://www.youtube.com/watch?v=2k8O-Y_uiRU ####
## https://mapping-in-r.netlify.app/ #####

library(tidyverse)     # for data cleaning and plotting
library(gardenR)       # for Lisa's garden data
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())

theme_set(theme_minimal())

# Starbucks locations
Starbucks <- read_csv("https://www.macalester.edu/~ajohns24/Data/Starbucks.csv")

# ไม่ได้ใช้ google map 
# ใช้ stamen map โดยดู  bbox จาก openstreet.com
tha_map <-
  get_stamenmap(
    bbox = c(left = 97, bottom = 5.5, right = 110, top = 20.5),
    maptype = "terrain",
    zoom = 6
  )

ggmap(tha_map)+
  geom_path(data = ,
            aes(x = ,
                y = ,))

# อาจารย์ TU EP1 http://www.gis2me.com/gcom/?cat=271&paged=2 ####
## http://www.gis2me.com/gcom/?p=3239 ####

library(leaflet)
library(tidyverse)
library(ggmap)
library(leaflet.extras)
library(htmltools)
library(maps)
library(mapproj)
library(mapdata)

w <- map_data("world")
tcji <- map_data("world",
                 region = c("Thailand", "China", "Japan", "India"))

tcji %>% 
  ggplot(aes(x = long, y = lat, group = group))+
  geom_polygon(color = "black")

tcji %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = region))+
  geom_polygon(color = "black")

# draw coordinate world map
tcji %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = region))+
  geom_polygon(color = "black")+
  coord_map("polyconic")

# อาจารย์ TU EP2 