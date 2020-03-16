#This is the code to generate the ggmap for Kolkata ward and pumping station map;
library(ggmap)
library(ggrepel)
library(ggplot2)
library(leaflet)
library(rgdal)

#You have to have a key for geocoding API on google to run this part.
register_google(key="AIzaSyCXpHGZYVvDyk9nRMvr9yoSxAToWcIHGMo")

kolkata.map <- get_googlemap(center = c(lon = 88.36, lat = 22.533),
                             zoom = 12, scale = 2,
                             maptype ='roadmap',
                             color = 'color')
p <- ggmap(kolkata.map)

#read ward map; Please modify your direction if you want to reproduce the map;
path <- "H:/Programming/kolkata_maps/"
wards <- readOGR(path, "wards_kolkata_all")

wards <- spTransform(wards, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
f.wards <- fortify(wards)
f.wards$id <- as.character(f.wards$id)
info.wards <- data.frame(cbind(as.character(0:143),as.character(wards$wardbound),as.numeric(as.character(wards$TOT_P)),as.numeric(as.character(wards$TOT_AREA))))
names(info.wards) <- c("id","ward","pop","area")
info.wards$id <- as.character(info.wards$id)
info.wards$ward <- as.character(info.wards$ward)
info.wards$pop <- as.numeric(as.character(info.wards$pop))
info.wards$area <- as.numeric(as.character(info.wards$area))
info.wards$pop_density <- info.wards$pop/info.wards$area

f.wards <- merge(f.wards, info.wards, all.x=T, by="id")

centroid <- aggregate(cbind(long,lat) ~ ward, data=f.wards, FUN=mean)

load("~/stat/SurveillanceSimulation/ES sampling sites update design/Simulation/ES simul Kolkata/kolkata_maps/kolkata_PS_loc.rda")
PS.loc$ps.col <- "Pumping Station"
PS.loc$ps.pch <-16
add.one <- length(PS.loc[,1])+1
PS.loc[add.one,] <- NA
PS.loc$PS.name[add.one] <- "NICED"
PS.loc$results.geometry.location.lat[add.one] <- 22.565806
PS.loc$results.geometry.location.lng[add.one] <- 88.399856
PS.loc$ps.col[add.one] <- "NICED"
PS.loc$ps.pch[add.one] <- 15

p + geom_polygon(data = f.wards, aes(x = long, y = lat, group = group, fill = pop_density), alpha=0.8, color="black") +
  scale_fill_continuous("Population Density\n(people/km2)",high = "#303030", low = "#DCDCDC") + 
  #geom_text(data = centroid, mapping = aes(x=long, y=lat, label=ward)) +
  geom_point(aes(x = results.geometry.location.lng, y = results.geometry.location.lat, colour = ps.col), size = 3, data = PS.loc) +
  scale_color_discrete("Locations") +
  geom_text(aes(x = 88.399856+0.015, y = 22.565806, label = "NICED"),color = "orangered",size = 6) +
  xlab("Longitude") + ylab("Latitude")



