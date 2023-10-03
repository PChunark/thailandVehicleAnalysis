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

# อาจารย์ TU EP2 https://www.youtube.com/watch?v=MsMeDf6CALY&list=PLCHOThJWrXeWAmnWISeKulpLYAP_NuHJB&index=2 ####

## Call global map using leaflet library ####
leaflet() %>% addTiles()
names(providers) # list map providers
# test map type
leaflet() %>% addProviderTiles("Esri")
leaflet() %>% addProviderTiles("Wikimedia")
leaflet() %>% addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
leaflet() %>% addProviderTiles("TomTom.Basic")
leaflet() %>% addProviderTiles("HERE.basicMap")
leaflet() %>% addProviderTiles("OpenTopoMap")

# Location ตึก ท100
leaflet() %>% addProviderTiles("Esri.WorldStreetMap") %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16) %>% 
  addMarkers(lng = 100.50551995044027, lat = 13.811329944624301)

leaflet() %>% addProviderTiles("Esri.WorldStreetMap") %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16) %>% 
  addCircleMarkers(lng = 100.50551995044027, 
                   lat = 13.811329944624301,
                   radius = 4,
                   color = "red")

# อาจารย์ TU EP3 https://www.youtube.com/watch?v=gh8VMFUv91o ####

## Set map option ####
leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", group = "Streets") %>%
  addProviderTiles("Esri", group = "Imagery") %>% 
  addProviderTiles("OpenTopoMap", group = "Topomap") %>% 
  addLayersControl(baseGroups = c("Streets", "Topomap","Imagery"),
                   options = layersControlOptions(collapsed = F, autoZIndex = T)) %>%
  addMarkers(lng = 100.50551995044027, lat = 13.811329944624301) %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16)

## Fix zoom option ####
leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", 
                   group = "Streets", 
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>%
  addProviderTiles("Esri", 
                   group = "Imagery",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addProviderTiles("OpenTopoMap", 
                   group = "Topomap",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addLayersControl(baseGroups = c("Streets", "Topomap","Imagery"),
                   options = layersControlOptions(collapsed = F, autoZIndex = T)) %>%
  addMarkers(lng = 100.50551995044027, lat = 13.811329944624301) %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16)

## Hide/show layer control option ####
# options = layersControlOptions(collapsed = T
leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", 
                   group = "Streets", 
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>%
  addProviderTiles("Esri", 
                   group = "Imagery",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addProviderTiles("OpenTopoMap", 
                   group = "Topomap",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addLayersControl(baseGroups = c("Streets", "Topomap","Imagery"),
                   options = layersControlOptions(collapsed = T, autoZIndex = T)) %>%
  addMarkers(lng = 100.50551995044027, lat = 13.811329944624301) %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16)

## Search ---addSearchOSM() #####
library(leaflet.extras)

leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", 
                   group = "Streets", 
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>%
  addProviderTiles("Esri", 
                   group = "Imagery",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addProviderTiles("OpenTopoMap", 
                   group = "Topomap",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addSearchOSM() %>% 
  addLayersControl(baseGroups = c("Streets", "Topomap","Imagery"),
                   options = layersControlOptions(collapsed = T, autoZIndex = T)) %>%
  addMarkers(lng = 100.50551995044027, lat = 13.811329944624301) %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16)

# อาจารย์ TU EP4 https://www.youtube.com/watch?v=6XX-niZs8DI&list=PLCHOThJWrXeWAmnWISeKulpLYAP_NuHJB&index=4 ####
library(leaflet)
library(leaflet.extras)

leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", 
                   group = "Streets", 
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>%
  addProviderTiles("Esri", 
                   group = "Imagery",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addProviderTiles("OpenTopoMap", 
                   group = "Topomap",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addSearchOSM() %>% 
  addLayersControl(baseGroups = c("Streets", "Topomap","Imagery"),
                   options = layersControlOptions(collapsed = T, autoZIndex = T)) %>%
  addMarkers(lng = 100.50551995044027, lat = 13.811329944624301) %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16)
## Reading Shapefile -- library(raster) ####
library(raster)
library(widgetframe)
library(htmltools)
library(sf)
library(tmap)


tha_map <-
  st_read(dsn = "rawdata/tha_adm_rtsd_itos_20210121_shp/tha_admbnda_adm1_rtsd_20220121.shp") %>% 
  as_tibble() %>% 
  separate(ADM1_PCODE, into = c("pcode_text", "pcode_num"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(pcode_num = as.numeric(pcode_num)) %>% 
  # select(geometry, pcode_num)
  st_as_sf()
  
basinmap<-
  shapefile("rawdata/basin2/basin2.shp")

basinmap %>% 
leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", 
                   group = "Streets", 
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>%
  addProviderTiles("Esri", 
                   group = "Imagery",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addProviderTiles("OpenTopoMap", 
                   group = "Topomap",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addPolygons(label = ~htmlEscape(BASIN_ID),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = T)) %>% 
  addSearchOSM() %>% 
  addLayersControl(baseGroups = c("Streets", "Topomap","Imagery"),
                   options = layersControlOptions(collapsed = T, autoZIndex = T)) %>%
  addMarkers(lng = 100.50551995044027, lat = 13.811329944624301) %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16)

## Add layer control in option ####
basinmap %>% 
  leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", 
                   group = "Streets", 
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>%
  addProviderTiles("Esri", 
                   group = "Imagery",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addProviderTiles("OpenTopoMap", 
                   group = "Topomap",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addPolygons(label = ~htmlEscape(BASIN_ID),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              group = "Basin",
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = T)) %>% 
  addSearchOSM() %>% 
  addLayersControl(baseGroups = c("Streets", "Topomap","Imagery"),
                   overlayGroups = c("Basin"),
                   options = layersControlOptions(collapsed = T, autoZIndex = T)) %>%
  addMarkers(lng = 100.50551995044027, lat = 13.811329944624301) %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16)

## Symbology ####
bins <- seq(10,100,10)
pal <- colorBin("Set3", domain = tha_map$pcode_num, bins = bins)


tha_map %>%
  # separate(ADM1_PCODE, into = c("pcode_text", "pcode_num"), sep = "(?<=[A-Za-z])(?=[0-9])")
  leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", 
                   group = "Streets", 
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>%
  addProviderTiles("Esri", 
                   group = "Imagery",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addProviderTiles("OpenTopoMap", 
                   group = "Topomap",
                   options = tileOptions(minZoom = 4, maxZoom = 15)) %>% 
  addPolygons(label = ~htmlEscape(ADM1_EN),
              fillColor = ~pal(pcode_num),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              group = "Province",
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = T)) %>% 
  addSearchOSM() %>% 
  addLayersControl(baseGroups = c("Streets", "Topomap","Imagery"),
                   overlayGroups = c("Province"),
                   options = layersControlOptions(collapsed = T, autoZIndex = T)) %>%
  addMarkers(lng = 100.50551995044027, lat = 13.811329944624301) %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16)

# อาจารย์ TU EP5 https://www.youtube.com/watch?v=bl338EyBTgU&list=PLCHOThJWrXeWAmnWISeKulpLYAP_NuHJB&index=5####
# การนำเข้าจุด point จาก excel

library(leaflet)
library(leaflet.extras)
library(raster) # Reading shape file
library(widgetframe)
library(htmltools)

## Display leaflet map original based map ####

leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", group = "Streets") %>%
  addProviderTiles("Esri", group = "Imagery") %>% 
  addProviderTiles("OpenTopoMap", group = "Topomap") %>% 
  addLayersControl(baseGroups = c("Streets", "Topomap","Imagery"),
                   options = layersControlOptions(collapsed = F, autoZIndex = T)) %>%
  addMarkers(lng = 100.50551995044027, lat = 13.811329944624301) %>% 
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 16)

## Reading excel ####
library(readxl)
covidTh <-
  read_excel("rawdata/TH_COVID_REPORT/TH_COVID_import.xls",
           sheet = "raw",
           range = "A1:L3011")
covidTh
str(covidTh) # See the structure of a data frame
summary(covidTh)
# View(covidTh)
fix(covidTh) # for edit data frame directly in R

## For filter -- use library(tidyverse) ####

library(tidyverse)

covidMap <-
  covidTh %>% 
  filter(NationEn == "Thai") 
covidMap  

covidMap <-
  covidMap %>% 
  group_by(ProvinceEn) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
covidMap$ProvinceEn <- tolower(covidMap$ProvinceEn)
summary(covidMap)

thgps <-
  read_excel("rawdata/TH_COVID_REPORT/TH_COVID_import.xls",
             sheet = "gps")
thgps

#Change font to lower
thgps$PROVINCE_E <- tolower(thgps$PROVINCE_E)

# These techniques can be used for binding the dataframe
data <-
  merge(covidMap, thgps,
        by.x = "ProvinceEn",
        by.y = "PROVINCE_E")

data2<-  full_join(covidMap,thgps, by = c("ProvinceEn" = "PROVINCE_E"))
  
names(data)
reports <-
  sort(table(data$ProvinceEn), decreasing = T)

row.names(reports[1:10])

covidMap2 <-
  leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik", group = "Streets") %>%
  addProviderTiles("Esri", group = "Imagery") %>% 
  addProviderTiles("OpenTopoMap", group = "Topomap") %>% 
  addLayersControl(baseGroups = c("Streets", "Topomap","Imagery"),
                   options = layersControlOptions(collapsed = F, autoZIndex = T)) %>%
  setView(lng = 100.50551995044027, lat = 13.811329944624301,
          zoom = 10)
covidMap2

## Add circle marker ####
covidMap2 %>% addCircleMarkers(data = data, popup = data$count, radius = 3)

## Change color by province ####
library(RColorBrewer)
