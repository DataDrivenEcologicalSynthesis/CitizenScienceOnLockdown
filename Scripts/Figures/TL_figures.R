#######################################
#   Figure generation - Timothy Law   #
#######################################

#libraries
library(tidyverse)
library(sf)
library(viridis)
library(ggplot2)
library(ggmap)
library(osmdata)
library(nlme)
library(reshape2)
library(broom)

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
cities_info <- read.csv("Data/02_cities_pop_park.csv")

cities_info_clean <- cities_info[, c("Geographic.name"
									 , "Population..2016"
									 , "Total.private.dwellings..2016"
									 , "Land.area.in.square.kilometres..2016"
									 , "Population.density.per.square.kilometre..2016"
									 , "park.area"
									 , "park.area.percentage"
)]
rm(cities_info)

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

#saskatoon
saskatoon <- get_map(location=c(left=city.box$xmin[19],bottom=city.box$ymin[19],
							  right=city.box$xmax[19],top=city.box$ymax[19]), 
				   maptype = "roadmap", color="bw")
saskatoon_data <- sf::st_intersection(inat.shp, city.boundaries[19,])
saskatoon_boundary <- st_transform(city.boundaries[19,],crs=4326)

park <-  c(city.box$xmin[19],city.box$ymin[19],
		   city.box$xmax[19],city.box$ymax[19]) %>%
	opq() %>%
	add_osm_feature("leisure","park") %>%
	osmdata_sf(quiet=F)	
saskatoon_parks.p <- park$osm_polygons %>% st_transform(crs=4328)
saskatoon_parks.m <- park$osm_multipolygons %>% st_transform(crs=4328)

png("Figures/Exploratory/TL_saskatoon_map.png", width = 2000, height = 1700, units ="px", res=300)
ggmap(saskatoon) +
	geom_sf(data=city.boundaries$geometry[19],inherit.aes = FALSE, 
			color="red", fill="transparent", size=1) +
	geom_sf(data=saskatoon_parks.p, inherit.aes = FALSE,
			color="transparent", fill="forestgreen") +
	geom_sf(data=saskatoon_parks.m, inherit.aes = FALSE,
			color="transparent", fill="forestgreen") +
	geom_sf(data=saskatoon_data, inherit.aes = FALSE, size=0.5, aes(color=factor(year))) +
	scale_color_viridis(discrete=TRUE,option="plasma")
dev.off()

#hamilton
hamilton <- get_map(location=c(left=city.box$xmin[10],bottom=city.box$ymin[10],
								right=city.box$xmax[10],top=city.box$ymax[10]), 
					 maptype = "roadmap", color="bw")
hamilton_data <- sf::st_intersection(inat.shp, city.boundaries[10,])
hamilton_boundary <- st_transform(city.boundaries[10,],crs=4326)

park <-  c(city.box$xmin[10],city.box$ymin[10],
		   city.box$xmax[10],city.box$ymax[10]) %>%
	opq() %>%
	add_osm_feature("leisure","park") %>%
	osmdata_sf(quiet=F)	
hamilton_parks.p <- park$osm_polygons %>% st_transform(crs=4328)
hamilton_parks.m <- park$osm_multipolygons %>% st_transform(crs=4328)

png("Figures/Exploratory/TL_hamilton_map.png", width = 2000, height = 1700, units ="px", res=300)
ggmap(hamilton) +
	geom_sf(data=city.boundaries$geometry[10],inherit.aes = FALSE, 
			color="red", fill="transparent", size=1) +
	geom_sf(data=hamilton_parks.p, inherit.aes = FALSE,
			color="transparent", fill="forestgreen") +
	geom_sf(data=hamilton_parks.m, inherit.aes = FALSE,
			color="transparent", fill="forestgreen") +
	geom_sf(data=hamilton_data, inherit.aes = FALSE, size=0.5, aes(color=factor(year))) +
	scale_color_viridis(discrete=TRUE,option="plasma")
dev.off()
#--------------Hypothesis 2----------------#
#map
spc.rich.park <- left_join(CityTotal,cities_info_clean,by=c("city" = "Geographic.name"))

plot(spc.rich.park$spc_richness~spc.rich.park$park.area.percentage)

#linear model between park and species richness
lm.park <- lm(spc_richness~park.area.percentage,data=spc.rich.park)
summary(lm.park)

#lm with year as a random effect
lme.fit <- lme(spc_richness~park.area.percentage,
			   random=~1|year,
			   data=spc.rich.park)
summary(lme.fit)

#lm with city as a random effect
lme.fit <- lme(spc_richness~park.area.percentage,
			   random=~1|city,
			   data=spc.rich.park)
summary(lme.fit)

#--------------species abundance-----------------#
###lets look at some histograms??###
#histogram of abundance per year
abund.year <- dcast(inat, scientific_name~year, fun.aggregate = length, 
					value.var = "scientific_name") %>%
				melt(id.vars="scientific_name",
					 measure.vars=c("2016","2017","2018","2019","2020"))

ggplot(abund.year, aes(value)) + 
	geom_histogram() +
	facet_wrap(abund.year$variable)
#histogram of abundance per city
abund.city <- dcast(inat, scientific_name~Ggrphc_n, fun.aggregate = length, 
					value.var = "scientific_name") %>%
				melt(id.vars="scientific_name",
					 measure.vars=c("Vancouver","Surrey", "Calgary","Edmonton", 
					 			   "Saskatoon","Winnipeg","London","Hamilton", 
					 			   "Brampton", "Mississauga","Toronto", "Markham",
					 			   "Vaughan", "Ottawa", "Gatineau" , "Laval" , 
					 			   "Montreal","Longueuil",  "Quebec",  "Halifax"))
ggplot(abund.city, aes(value)) + 
	geom_histogram() +
	facet_wrap(abund.city$variable)
#lol lets not

###how do observations of species change###
#trend in observation of each species across years
abund.year.sort <- arrange(abund.year,scientific_name)

png("Figures/Species_composition/TL_change_in_abund_per_spc.png", width = 900, height = 700, units ="px", res=120)
ggplot(abund.year.sort, aes(variable,value,group=scientific_name)) +
	geom_line(alpha=0.4) +
	labs(x="Year",y="Number of Observations") +
	theme_bw()
dev.off()

###trend in mallards###
mallard <- inat %>% filter(scientific_name=="Anas platyrhynchos") %>%
					dcast(year~scientific_name, fun.aggregate = length, 
						  value.var = "scientific_name")
colnames(mallard) <- c("year","mallard")
mallard$quarantine <- c(rep("no",4),"yes")
#model
mallard.lm <- lm(mallard~year,data=mallard[1:4,])
summary(mallard.lm)
mallard.predict<-predict.lm(mallard.lm,newdata=data.frame(year=2020),
							interval = "prediction")
#plot
png("Figures/Species_composition/TL_Pandemic_mallard_lm.png", width = 900, height = 700, units ="px", res=120)
ggplot(mallard, aes(x=year, y=mallard, col=quarantine)) + 
	geom_point() +
	stat_smooth(data=subset(mallard,year <= 2019),method = "lm", color="red")
dev.off()

###trend in all species???###
#list of species that have occurrences every year
species.years <- abund.year %>% 
					filter(value!=0) %>% 
					count(scientific_name) %>% 
					filter(n==5) #111 species out of 386 species
#dataset of species that appear every year
allyears.species <- abund.year %>%
						filter(scientific_name %in% as.character(species.years$scientific_name))
list.species <- as.character(species.years$scientific_name)

#linear model for each of the species
list.models <- list()
list.pvalues <- c()
for(i in 1:111){
	sub.spc <- filter(abund.year, scientific_name==list.species[i])
	sub.spc$variable <- as.numeric(as.character(sub.spc$variable))
	subset.lm <- lm(value~variable, data=subset(sub.spc,variable<=2019))
	list.models[[i]] <- subset.lm
	list.pvalues[i] <- glance(subset.lm)$p.value
}
names(list.models) <- list.species

#only 94 models pvalue >=0.05
sig.models <- list.models[which(list.pvalues>=0.05)]

#predict
predict.spc <- data.frame(matrix(nrow=94,ncol=5))
colnames(predict.spc) <- c("spc","predicted","lower","upper","actual")

for(m in 1:94){
	sub.predict <- predict.lm(sig.models[[m]],newdata=data.frame(variable=2020),
							  interval = "prediction")
	predict.spc[m,1] <- names(sig.models[m])
	predict.spc[m,2:4] <- sub.predict
	predict.spc[m,5] <- abund.year %>% 
							filter(variable==2020) %>% 
							filter(scientific_name==names(sig.models[m])) %>% 
							select(value)
	if(predict.spc[m,5]>predict.spc[m,2]){
		predict.spc$result[m] <- "greater"
	} else {
		if(predict.spc[m,5]<predict.spc[m,2]){
			predict.spc$result[m] <- "less"
		} else {
			predict.spc$result[m] <- "expected"
		}
	}
}

#cool so 69 species observed more than expected, 21 less than, 4 as expected
modeled.spcs <- left_join(abund.year.sort,predict.spc,by=c("scientific_name"="spc")) %>% filter(!is.na(result))
ggplot(modeled.spcs, aes(variable,value,group=scientific_name,col=result)) +
	geom_line(alpha=0.4) +
	labs(x="Year",y="Number of Observations") +
	theme_bw()
