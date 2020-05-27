#installation
# install.packages("rgbif")
# install.packages('bit64')
# library(rgbif)
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
Canada_occurrence <- Canada_gbif %>% 
	filter(taxonRank == "SPECIES")
# losing 2728 occurences
## selecting those has both longitute and latitude coordination
Canada_occurrence <- Canada_occurrence %>% 
	drop_na(decimalLatitude) %>% 
	drop_na(decimalLongitude)
# nothing happens here for me, can someone confirm?

##creating data frame with relevant variables
Canadadata<-as.data.frame(Canada_occurrence)
finaldata<-Canadadata %>%
	dplyr::select(recordedBy,eventDate,year,month,day, stateProvince
				  , verbatimLocality, decimalLatitude, decimalLongitude
				  , coordinateUncertaintyInMeters, identificationID
				  , dateIdentified, taxonID, scientificName, order, family
				  , genus, species, genericName, acceptedScientificName
				  , verbatimScientificName)

# let's save it so that we can use it in other scripts
write.csv(finaldata, "Data/Rgbif_data.csv")
