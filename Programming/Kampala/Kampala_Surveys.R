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

#surveys2 <- getKMLcoordinates("Household_Survey-xlsform_kla2018_final_2018_12_03_15_19_49.kml")

#surveys <- readOGR("Household_Survey-xlsform_kla2018_final_2018_12_03_15_19_49.kml")

surveys <- read_excel("Household_Survey-xlsform_kla2018_final_2018_12_03_14_37_06.xlsx")

surveys$lat <- as.numeric(surveys$`_h_coordinates_latitude`)
surveys$lng <- as.numeric(surveys$`_h_coordinates_longitude`)
palChol <- colorFactor("Set3", domain=surveys$h_enum)



mymap <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data=surveys, lat=~surveys$lat, lng=~surveys$lng, color=~palChol(surveys$h_enum),
                   fillColor=~palChol(surveys$h_enum), fillOpacity=1, radius=5,
                   popup=paste0(surveys$h_enum))
mymap
