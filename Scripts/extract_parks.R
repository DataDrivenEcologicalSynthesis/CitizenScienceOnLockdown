##############################
#   Obtain greenspace data   #
##############################

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

#load boundaries
CMA_boundaries <- st_read("../Data/CMA_boundary_red.shp")
#extract boundary for one city (can be iterated in loop)
city.shp <- CMA_boundaries[CMA_boundaries$CMANAME=="Toronto",]
#transform boundary for lat long bounding box
c.transform<-st_transform(city.shp, CRS("+proj=longlat"))
city.bb <- st_bbox(c.transform)

#retrieve park data
get.park <- city.bb %>%
              opq() %>%
                add_osm_feature("leisure","park")
park <- osmdata_sf(get.park)
#match projection of the boundary layer
park.prj <- st_transform(park$osm_polygons,crs=st_crs(city.shp)$input)

#select parks that only fall within the boundary layer
city.park <- park.prj[lengths(st_intersects(park.prj,city.shp))!=0,]
city.park.sum <- sum(st_area(city.park))
