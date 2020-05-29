# THIS SCRIPT IS TO DOWNLOAD DATA FROM GBIF
# THE DATA IS THEN CLEANED
# AND SAVED AS Data/R_GBIF_data.csv 

#installation
# install.packages("rgbif")
# install.packages('bit64')
library(rgbif)
library(dplyr)

###for all the iNat bird data of Canada since 2020 from Janurary to May
Canada_gbif1 <- occ_download_get(key = "0069150-200221144449610", overwrite = TRUE)  %>%  
	occ_download_import(Canada_gbif_download, na.strings = c("", NA))
###for all the iNat bird data of Canada from Janurary to May from 2011-2019
Canada_gbif2 <- occ_download_get(key = "0070513-200221144449610", overwrite = TRUE)  %>%  
	occ_download_import(Canada_gbif_download, na.strings = c("", NA))
# quick check that we can row bind the datasets
sum(colnames(Canada_gbif1)==colnames(Canada_gbif2))
# Binding the two datasets
Canada_gbif <- rbind(Canada_gbif1, Canada_gbif2)

##selecting those has been identified to species level
### Ok but also we don't want to exclude subspecies!
Canada_occurrence <- Canada_gbif # keep everything

## selecting those has both longitute and latitude coordinates, coordinate uncertainty <1000 m, in March and April, 
# that are IDed at the species or subspecies level (not genus or family), and that are from 2016-2020
Canada_occurrence <- Canada_gbif %>% 
	drop_na(decimalLatitude) %>% 
	drop_na(decimalLongitude)%>%
	drop_na(coordinateUncertaintyInMeters)%>%
	filter(coordinateUncertaintyInMeters<1000)%>%
	filter(month %in% c(3,4))%>% 
	filter(taxonRank %in% c("SPECIES", "SUBSPECIES"))%>%
	filter(year>2015.5)

dim(Canada_occurrence)
dim(Canada_gbif)

##creating data frame with relevant variables
Canadadata<-as.data.frame(Canada_occurrence)
finaldata<-Canadadata %>%
	dplyr::select(recordedBy,eventDate,year,month,day, stateProvince
				  , verbatimLocality, decimalLatitude, decimalLongitude
				  , coordinateUncertaintyInMeters, identificationID
				  , dateIdentified, taxonID, taxonRank, scientificName, order, family
				  , genus, species, genericName, acceptedScientificName
				  , verbatimScientificName) # add in taxon rank

# let's save it so that we can use it in other scripts
write.csv(finaldata, "Data/R_GBIF_data.csv", rownames = FALSE)
finaldata <- read.csv("Data/R_GBIF_data.csv")
#####filtering birds in the cities
#import data "cities_boundingboxes"
citymap<-read.csv("Data/cities_boundingboxes.csv", header=T)
finaldata$city <- NA
for (i in 1: nrow(citymap)){
	lat_between <- between(finaldata$decimalLatitude, citymap$ymin[1], citymap$ymax[1])
	lon_between <- between(finaldata$decimalLongitude, citymap$xmin[1], citymap$xmax[1])
	finaldata$city[lat_between&lon_between] <- citymap$city[1]
}
citybird <- finaldata[!is.na(finaldata$city),] 
write.csv(citybird, "Data/R_GBIF_citybird.csv")


