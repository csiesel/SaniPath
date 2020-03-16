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


water <- readOGR("C:/Users/csiesel/Downloads/SpatialPlanningProposal.kml", layer="WaterPoints")
cholera <- readOGR("C:/Users/csiesel/Downloads/SpatialPlanningProposal.kml", layer="Cholera")
latrines <- readOGR("C:/Users/csiesel/Downloads/SpatialPlanningProposal.kml", layer="Latrines")
sewage <- readOGR("C:/Users/csiesel/Downloads/SpatialPlanningProposal.kml", layer="Sewage Lines")
kanyama <- readOGR("C:/Users/csiesel/Downloads/SpatialPlanningProposal.kml", layer="Kanyama")
  
mymap <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data=water, color="blue") %>%
  addCircleMarkers(data=cholera, color="darkred", fillColor="darkred") %>%
  addCircleMarkers(data=latrines, color="brown") %>%
  addPolylines(data=sewage, color="green") %>%
  addPolylines(data=kanyama, color="orange")
  
  
print(mymap)


water_coord <- water@coords
window<- as(as(kanyama, "SpatialPolygons"), "owin")
d <- dirichlet(as.ppp(water_coord, window))
dsp <- as(d, "SpatialPolygons")
dsp_df <- SpatialPolygonsDataFrame(dsp, data = data.frame(id = 1:length(dsp@polygons)))
#proj4string(dsp_df) <- CRS("+init=epsg:20934")
#dsp_df <- spTransform(dsp_df, CRS("+init=epsg:20934 +proj=utm +zone=34 +south +a=6378249.145 +b=6356514.966398753 +towgs84=-143,-90,-294,0,0,0,0 +units=m +no_defs"))
gArea(dsp_df, byid=TRUE)
areaPolygon(dsp_df, byid=TRUE)
dsp_df$area <- round((areaPolygon(dsp_df, byid = TRUE) / 1000000), 1)
proj4string(dsp_df) <- CRS("+init=epsg:20934")
dsp_xy <- spTransform(dsp_df, CRS("+proj=longlat +datum=WGS84"))

#Adding in the Dirichlet polygons to map
mymap2 <- leaflet() %>%
  addCircleMarkers(data=water, color="blue", group="Water Points") %>%
  addCircleMarkers(data=cholera, color="darkred", fillColor="darkred", group="Cholera Cases") %>%
  addCircleMarkers(data=latrines, color="darkgrey", group="Latrines") %>%
  addPolylines(data=sewage, color="green", group="Open Drains") %>%
  addPolylines(data=kanyama, color="orange") %>%
  addPolygons(data = dsp_df, 
              color = "lightblue", 
              fill = "lightblue",
              popup = paste0("Area: ", 
                             as.character(dsp_df$area), 
                             " square km")) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addLayersControl(baseGroups=c("OpenStreetMap"), overlayGroups=c("Water Points", "Cholera Cases", "Latrines", "Open Drains")) %>%
  addLegend(colors=c("blue", "darkred", "darkgrey", "green", "orange"), labels=c("Water Points", "Cholera Cases", "Latrines", "Open Drains", "Kanyama"))
print(mymap2)




# cholera plot
proj4string(cholera) <- CRS("+init=epsg:20934 +proj=utm +zone=34 +south +a=6378249.145 +b=6356514.966398753 +towgs84=-143,-90,-294,0,0,0,0 +units=m +no_defs")
dsp_df$choleracount <- colSums(gContains(dsp_df, cholera, byid=TRUE))
pal <- colorBin("Reds", domain = dsp_df$choleracount, bins=c(0,1,2,3,7))
#Trying to calculate clusters
cholera@coords <- cholera@coords[,1:2]
cholDist <- distm(cholera@coords)
clustering <- hclust(as.dist(cholDist),method="average")
plot(clustering)
plot(rect.hclust(clustering, k=6))
cholera$clust <- cutree(clustering,h=1000)
palChol <- colorBin("Set3", bins=11, domain=cholera$clust)

mymap3 <- leaflet() %>%
  addCircleMarkers(data=water, color="blue", group="Water Points") %>%
  addCircleMarkers(data=cholera, color="darkred", fillColor=~palChol(cholera$clust), group="Cholera Cases") %>%
  addCircleMarkers(data=latrines, color="darkgrey", group="Latrines") %>%
  addPolylines(data=sewage, color="green", group="Open Drains") %>%
  addPolylines(data=kanyama, color="orange") %>%
  addPolygons(data = dsp_df,
              color = "lightblue",
              fillColor = ~pal(dsp_df$choleracount),
              popup = paste0("Area: ",
                             as.character(dsp_df$area),
                             " square km,",
                             "\nNum Cases: ", as.character(dsp_df$choleracount))) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addLayersControl(baseGroups=c("OpenStreetMap"), overlayGroups=c("Water Points", "Cholera Cases", "Latrines", "Open Drains")) %>%
  addLegend(colors=c("blue", "darkred", "darkgrey", "green", "orange"),
            labels=c("Water Points", "Cholera Cases", "Latrines", "Open Drains", "Kanyama"))
print(mymap3)


#Open Drain buffers
proj4string(sewage) <- CRS("+init=epsg:20934 +proj=utm +zone=34 +south +a=6378249.145 +b=6356514.966398753 +towgs84=-143,-90,-294,0,0,0,0 +units=m +no_defs")
buff15 <- gBuffer(sewage, byid=FALSE, width=.00015)
buff30 <- gBuffer(sewage, byid=FALSE, width=.00030)
buff45 <- gBuffer(sewage, byid=FALSE, width=.00045)
buff60 <- gBuffer(sewage, byid=FALSE, width=.00060)

mymap4 <- leaflet() %>%
  addCircleMarkers(data=water, color="blue", group="Water Points") %>%
  addCircleMarkers(data=cholera, color="darkred", fillColor="darkred", group="Cholera Cases") %>%
  addCircleMarkers(data=latrines, color="darkgrey", group="Latrines") %>%
  addPolylines(data=sewage, color="green", group="Open Drains") %>%
  addPolylines(data=kanyama, color="orange") %>%
  addPolygons(data = dsp_df, 
              color = "lightblue", 
              fillColor = "",
              popup = paste0("Area: ", 
                             as.character(dsp_df$area), 
                             " square km,",
                             "\nNum Cases: ", as.character(dsp_df$choleracount))) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addLayersControl(baseGroups=c("OpenStreetMap"), overlayGroups=c("Water Points", "Cholera Cases", "Latrines", "Open Drains")) %>%
  addLegend(colors=c("blue", "darkred", "darkgrey", "green", "orange"),
            labels=c("Water Points", "Cholera Cases", "Latrines", "Open Drains", "Kanyama")) %>%
  addPolygons(data=buff15, fillColor="red", color="") %>%
  addPolygons(data=buff30, fillColor="orange", color="") %>%
  addPolygons(data=buff45, fillColor="yellow", color="") %>%  
  addPolygons(data=buff60, fillColor="green", color="")
  
print(mymap4)
