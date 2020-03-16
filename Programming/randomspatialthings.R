library(readxl)
library(ggmap)

gps <-read_excel(paste0(getwd(),"/spatialtest.xlsx"))

type <- c("1","3","4")

gps <- filter(gps, gps$Select_the_type_of_GPS_coordin %in% type)
plot(gps$`_Take_the_GPS_coordinates_for_the_point_latitude`)


gps$lat <- as.numeric(gps$`_Take_the_GPS_coordinates_for_the_point_latitude`)
gps$lng <- as.numeric(gps$`_Take_the_GPS_coordinates_for_the_point_longitude`)
leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(data=gps, lng=~gps$lng, lat=~gps$lat, popup=paste0("Type", gps$Select_the_type_of_GPS_coordin))
gps$lat

points <- readOGR("C:/Users/csiesel/Downloads/map(1).kml", layer="points")
lines <- readOGR("C:/Users/csiesel/Downloads/map(2).kml", layer="lines")
multilinestrings <- readOGR("C:/Users/csiesel/Downloads/map(2).kml", layer="multilinestrings")
multipolygons <- readOGR("C:/Users/csiesel/Downloads/map(1).kml", layer="multipolygons")
ogrListLayers("C:/Users/csiesel/Downloads/map(1).kml")

leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(data=points$name)

waterways <- lines[is.na(lines@data$waterway)!=TRUE,]
residential <- c("apartments", "commercial;residential", "hotel", "guest house", "guest_house", "dormitory", "house", "lodge", "residential")
hhs <- multipolygons[multipolygons@data$building %in% residential,]


leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolylines(data=waterways, popup=paste0("Type: ", waterways$waterway)) %>%
  #addPolylines(data=multilinestrings) %>%
  addScaleBar()%>%
  addPolygons(lng=heat$data[,1], lat=heat$data[,2])
  
hhs$centroid <- centroid(hhs)
heat <- geom_density2d(data=hhs, aes(x=hhs$centroid[,1],y=hhs$centroid[,2]),geom="polygon")
heatmap(hhs$centroid)
