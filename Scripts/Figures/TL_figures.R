#######################################
#   Figure generation - Timothy Law   #
#######################################

#libraries
library(tidyverse)
library(sf)
library(viridis)

#---------------------import data--------------------------#
#inat stuff
inat <- read.table("Data//04_Inat_from_clipping_NoDuplicates-csv.tsv", 
				   quote= "\"", fill=T, sep='\t',comment.char = "", 
				   na.strings=c("NA", ""), header=T)
#inat as shp
inat$long <- as.character(inat$geometry) %>% readr::parse_number()
inat$lat <- as.character(inat$Column.50) %>% readr::parse_number()
inat.shp <- st_as_sf(inat,coords=c("long","lat"),crs=4326)

#city properties
cities <- read.csv("Data/02_cities_pop_park.csv")
city.boundaries <- sf::st_read("Data/02_cities_boundaries.shp", crs=4326)
city.box <- read.csv("Data/02_cities_boundingboxes.csv")
parks.mpoly <- sf::st_read("Data/06_park_mpoly_boundaries.shp")
parks.poly <- sf::st_read("Data/06_park_poly_boundaries.shp")

#--------------generate spc richness----------------#

#generate city level observation counts
CityTotal=inat%>%
	group_by(Prvnc__, Ggrphc_n, year)%>%
	count()
CityTotal$Province=as.factor(CityTotal$Prvnc__)
CityTotal$city <- factor(CityTotal$Ggrphc_n, levels = c("Vancouver","Surrey", "Calgary","Edmonton", 
														"Saskatoon","Winnipeg","London","Hamilton", 
														"Brampton", "Mississauga","Toronto", "Markham","Vaughan", 
														"Ottawa", "Gatineau" , "Laval" , "Montreal","Longueuil",  
														"Quebec",  "Halifax"))

#generate species richness
for(c in 1:nrow(CityTotal)){
	CityTotal$spc_richness[c] <- inat %>% 
									filter(Ggrphc_n==CityTotal$city[c] & year==CityTotal$year[c]) %>% 
									distinct(scientific_name) %>% 
									nrow()
}

#--------------data visualization figures----------------#
jpeg("Figures/Exploratory/TL_Year_vs_Richness_perCity.jpg", width = 900, height = 500)
ggplot(data = CityTotal) +
	geom_jitter(aes(x = year, y = spc_richness)) +
	facet_wrap(~as.factor(Ggrphc_n))
dev.off()



#Toronto
toronto <- get_map(location=c(left=city.box$xmin[1],bottom=city.box$ymin[1],
							  right=city.box$xmax[1],top=city.box$ymax[1]), 
				   maptype = "roadmap", color="bw")

toronto_data <- sf::st_intersection(inat.shp, city.boundaries[1,])
toronto_boundary <- st_transform(city.boundaries[1,],crs="+proj=utm +zone=17")
toronto_parks.p <- sf::st_intersection(parks.poly, toronto_boundary)
toronto_parks.m <- sf::st_intersection(parks.mpoly, toronto_boundary)

png("Figures/Exploratory/TL_toronto_map.png", width = 2000, height = 1700, units ="px", res=300)
ggmap(toronto) +
	geom_sf(data=city.boundaries$geometry[1],inherit.aes = FALSE, 
			color="red", fill="transparent", size=1) +
	geom_sf(data=toronto_parks.p, inherit.aes = FALSE,
			color="transparent", fill="forestgreen") +
	geom_sf(data=toronto_parks.m, inherit.aes = FALSE,
			color="transparent", fill="forestgreen") +
	geom_sf(data=toronto_data, inherit.aes = FALSE, size=0.5, aes(color=factor(year))) +
	scale_color_viridis(discrete=TRUE,option="plasma")
dev.off()

#--------------Hypothesis 2----------------#
#map
