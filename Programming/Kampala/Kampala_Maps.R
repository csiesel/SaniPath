library(sp)
library(shiny)
library(leaflet)
library(readxl)
library(ggplot2)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(rgeos)
library(maptools)
library(maps)
library(spatstat)
library(geosphere)
library(leaflet.extras)
library(raster)

spTransform()
bwaiseII <- spTransform(readOGR("C:/Users/csiesel/Downloads/kampala_maps/BwaiseII_Kawempe.shp"), CRS("+proj=longlat +datum=WGS84"))
kabowa <- spTransform(readOGR("C:/Users/csiesel/Downloads/kampala_maps/Kabowa_Rubaga.shp"), CRS("+proj=longlat +datum=WGS84"))
kamwokyaI <- spTransform(readOGR("C:/Users/csiesel/Downloads/kampala_maps/KamwokyaI_CentralDivision.shp"), CRS("+proj=longlat +datum=WGS84"))
mbuyaII <- spTransform(readOGR("C:/Users/csiesel/Downloads/kampala_maps/MbuyaII_Nakawa.shp"), CRS("+proj=longlat +datum=WGS84"))
bukasa <- spTransform(readOGR("C:/Users/csiesel/Downloads/kampala_maps/Bukasa_Makindye.shp"), CRS("+proj=longlat +datum=WGS84"))
parishes <- SpatialPolygons(readOGR("C:/Users/csiesel/Downloads/kampala_maps/Kampala_Parishes.shp"), proj4string=CRS("+proj=utm +zone=36 +datum=WGS84 +ellps=clrk80 +units=m +no_defs"))


parishshape <- shapefile("C:/Users/csiesel/Downloads/kampala_maps/Kampala_Parishes.shp")
parishshape2 <- spTransform(parishshape, CRS("+proj=longlat +datum=WGS84"))
proj4string(parishshape)=CRS("+proj=utm +zone=36 +datum=WGS84"))

parishes2 <- readOGR("C:/Users/csiesel/Downloads/kampala_maps/Kampala_Parishes.shp")
parishes2 <- Spatial(parishes2, proj4string=CRS("+proj=utm +zone=36 +datum=WGS84"))
proj4string(parishes2)

kabowa2 <- spTransform(kabowa, CRS("+proj=longlat +datum=WGS84 +ellps=sphere"))

map1 <- leaflet() %>%
  addPolygons(data=bwaiseII, label="Bwaise II") %>%
  addPolygons(data=kabowa, label="Kabowa") %>%
  addPolygons(data=kamwokyaI, label="Kamwokya I") %>%
  addPolygons(data=mbuyaII, label="Mbuya II") %>%
  addPolygons(data=bukasa, label="Bukasa") %>%
  addPolygons(data=parishshape2, color="orange", fill="") %>%
  addProviderTiles("Esri.WorldStreetMap")
map1
