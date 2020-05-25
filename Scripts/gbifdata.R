#installation
install.packages("rgbif")
install.packages('bit64')
library(rgbif)
library(dplyr)
###for all the iNat bird data of Canada since 2020 from Janurary to May
Canada_gbif <- occ_download_get(key = "0069150-200221144449610", overwrite = TRUE)  %>%  occ_download_import(Canada_gbif_download, na.strings = c("", NA))
###for all the iNat bird data of Canada from Janurary to May from 2011-2020
Canada_gbif <- occ_download_get(key = "0069154-200221144449610", overwrite = TRUE)  %>%  occ_download_import(Canada_gbif_download, na.strings = c("", NA))
##selecting those has been identified to species level
Canada_occurrence <- Canada_gbif %>% filter(taxonRank == "SPECIES")
## selecting those has both longitute and latitude coordination
Canada_occurrence <- Canada_occurrence %>% drop_na(decimalLatitude) %>% drop_na(decimalLongitude)
##creating data frame with relevant variables
Canadadata<-as.data.frame(Canada_occurrence)
finaldata<-Canadadata%>%select(recordedBy,eventDate,year,month,day, stateProvince, verbatimLocality, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, identificationID, dateIdentified, taxonID, scientificName,order,family,genus, species, genericName, acceptedScientificName, verbatimScientificName)
