#########################################################
#   obtain city geography and building bounding boxes   #
#########################################################

### ATTENTION THIS IS ENCODED IN UTF-8
# Use File > Reopen with encoding > UTF-8 if you don't have a mac


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
library(lwgeom)

### load data ###
cities <- read.csv("Data/Population_top20.csv")
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
  add_osm_feature(key="boundary",value="administrative") 
montreal <- osmdata_sf(get.city)
montreal.shp <- montreal$osm_multipolygons %>%
            dplyr::filter(name=="Montréal (06)")
city.shp <-sp::merge(montreal.shp[1,1],cities[2,])
# You might have a problem here due to the encoding
cities.shp[[2]] <- city.shp

#Quebec city - province and city share name
get.city <- getbb("Quebec City") %>%
  opq() %>%
  add_osm_feature(key="boundary",value="administrative") 
quebec_city <- osmdata_sf(get.city)
quebec_city.shp <- quebec_city$osm_multipolygons %>%
  dplyr::filter(osm_id==2319206)
city.shp <-sp::merge(quebec_city.shp[1,1],cities[11,])
cities.shp[[11]] <- city.shp

### list to dataframe ###
#for plotting
cities.shp.df <- plyr::ldply(cities.shp, data.frame) %>% st_as_sf(sf_column_name="geometry")

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
write.csv(cities.bb,"Data/cities_boundingboxes.csv", row.names = FALSE)
cities.bb <- read.csv("Data/cities_boundingboxes.csv"
					  , encoding = "UTF-8")

###############################
### Extract city properties ###
###############################
#park areas
for(i in 1:nrow(cities)){
	park <-  st_bbox(cities.shp[[i]]) %>%
		opq() %>%
		add_osm_feature("leisure","park") %>%
		osmdata_sf()
	#find projection
	utm_zone <- ceiling((cities.bb$xmin[i] + 180)/6) 
	proj_string <- paste0("+proj=utm +zone=", utm_zone)
	#extract parks and project
	park.poly <- park$osm_polygons %>%
		st_transform(crs=proj_string)
	park.mpoly <- park$osm_multipolygons %>%
		st_transform(crs=proj_string)
	#select parks that are within city
	clip.city <- st_transform(cities.shp[[i]],crs=proj_string)
	city.park.poly <- park.poly[lengths(st_intersects(park.poly,clip.city))!=0,]
	city.park.mpoly <- park.mpoly[lengths(st_intersects(park.mpoly,clip.city))!=0,]
	#sum areas
	park.area <- sum(st_area(city.park.poly)) + sum(st_area(city.park.mpoly))
	#add area to cities
	cities$park.area[i] <- park.area
	#loop status
	print(cities[i,"Geographic.name"])
}

cities$park.area.percentage <- cities$park.area * 0.000001 / cities$Land.area.in.square.kilometres..2016 *100
write.csv(cities, "Data/cities_pop_park.csv", rownames = FALSE)
