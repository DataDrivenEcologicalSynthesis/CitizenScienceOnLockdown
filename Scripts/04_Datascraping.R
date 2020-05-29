#########################################################
#   downloading all data from inat + clipping		   #
#########################################################

# INIT ----
	# Libraries ----
	rm(list = ls())
	library(rinat)
	library(sf)

# end INIT

##########################################################
### downloading data from inat using the cities bboxes ###
#########################################################

# loading the bboxes
cities_bb <- read.csv("Data/02_cities_boundingboxes.csv", encoding = "UTF-8")

# creating a list to save everything
record <- list()
years <- c(2016, 2017, 2018, 2019, 2020)
t = 1
# for each city, loading the data inside the bbox
for (i in 1:length(cities_bb$city)) {
	xmin <- cities_bb$xmin[i]
	xmax <- cities_bb$xmax[i]
	ymin <- cities_bb$ymin[i]
	ymax <- cities_bb$ymax[i]
	for (j in years) {
		print(paste(cities_bb$city[i], j))
		inat_april_temp <- get_inat_obs(year = j
										, month = 4
										, geo = T
										, bounds = c(ymin, xmin, ymax, xmax) # updated the coordinates to select just southern Canada
										, taxon_name ="Aves"
										, maxresults = 999999)
		inat_march_temp <- get_inat_obs(year = j
										, month = 3
										, geo = T
										, bounds = c(ymin, xmin, ymax, xmax) # updated the coordinates to select just southern Canada
										, taxon_name ="Aves"
										, maxresults = 999999)
		inat_temp <- rbind(inat_march_temp, inat_april_temp)
		inat_temp$city <- (cities_bb$city[i])
		inat_temp$year <- j
		record[t] <- list(inat_temp)
		t=t+1
	}
}
# problems due to no observation with several cities and years (mosty 2016)
# some random errors show up due to encoding sometimes
# in that case I manually iterate j until I can move to the next i

# formating it in a big dataframe
record_merged <- inat_temp[FALSE,]
for (i in 1:length(record)) {
	record_merged <- rbind(record_merged, record[[i]])
}
record_spatial <- st_as_sf(record_merged, coords = c("longitude", "latitude"), crs= 4326)
write.csv(record_merged, "Data/04_Inat_from_bboxes.csv")

# loading the true outline of the cities
shape <- sf::st_read("Data/02_cities_boundaries.shp", crs=4326)

# intersecting the two
final_data <- sf::st_intersection(shape, record_spatial)

# saving the new version
write.csv(final_data, "Data/04_Inat_from_clipping.csv")
