#########################################################
#   obtain city geography and building bounding boxes   #
#########################################################

#install the osmdata, sf, tidyverse and ggmap package
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")

#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

### load data ###
cities <- read.csv("Population_top20.csv")
#fix problem names
cities$Geographic.name <- as.character(cities$Geographic.name)
cities[2,3] <- "Montréal"
cities[11,3] <- "Québec"

#############################
### build shape file list ###
#############################

#get geometry for each city
cities.shp <- list()
for(i in 1:nrow(cities)){
  #build search query 
  get.city <- getbb(cities[i,3], display_name_contains = "Canada") %>%
    opq() %>%
    add_osm_feature(key="boundary",value="administrative") %>%
    add_osm_feature(key="name",value = cities[i,3])
  #extract polygon
  city.poly <- osmdata_sf(get.city)$osm_multipolygons
  #merge geometry with attributes
  city.shp <-sp::merge(city.poly[1,1],cities[i,])
  #write to list
  cities.shp[[i]] <- city.shp
  #loop status
  print(cities[i,3])
}

### trouble cities ###

#island of montreal - amalgamation artifacts
get.city <- getbb("Montréal") %>%
  opq() %>%
  add_osm_feature(key="boundary",value="administrative") #%>%
montreal <- osmdata_sf(get.city)
montreal.shp <- montreal$osm_multipolygons %>%
            filter(name=="Montréal (06)")
city.shp <-sp::merge(montreal.shp[1,1],cities[2,])
cities.shp[[2]] <- city.shp

#Quebec city - province and city share name
get.city <- getbb("Quebec City") %>%
  opq() %>%
  add_osm_feature(key="boundary",value="administrative") #%>%
quebec_city <- osmdata_sf(get.city)
quebec_city.shp <- quebec_city$osm_multipolygons %>%
  filter(osm_id==2319206)
city.shp <-sp::merge(quebec_city.shp[1,1],cities[11,])
cities.shp[[11]] <- city.shp

############################
### build bounding boxes ###
############################

#table of cords for bounding box
cities.bb <- as.data.frame(matrix(ncol=5))
names(cities.bb) <- c("city","xmin","ymin","xmax","ymax")

#iterate bounding box function
for(i in 1:nrow(cities)){
  cities.bb[i,1] <- cities[i,3]
  cities.bb[i,2:5] <-st_bbox(cities.shp[[i]])
}

#write
write.csv(cities.bb,"cities_boundingboxes.csv")




