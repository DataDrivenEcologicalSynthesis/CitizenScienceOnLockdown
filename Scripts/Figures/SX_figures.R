# Generating figures related to hypothesis 1
#import data
# INIT ----
# INIT ----
rm(list = ls())
# Libraries
library(dplyr)
library(ggplot2)
# library(car)

# end INIT ----

# I. Importing datasets ----
#cities_info <- read.csv("Data/02_cities_pop_park.csv")
inat <- read.table("Data/04_Inat_from_clipping_NoDuplicates-csv.tsv"
				   , quote= "\""
				   , fill=T
				   , sep='\t'
				   , comment.char = ""
				   , na.strings=c("NA", "")
				   , header=T)
#write.table(inat,"inat.csv",sep=",")
#for checking all the record with duplicated "id"
# 
# dup<-inat%>% 
#   group_by(id) %>% 
# 	filter(n() != 1)
# write.table(dup,"dup.csv",sep=",")
#final data with unique id
inat_identity<-distinct(inat, id, .keep_all= TRUE)
inat_clean_observation <- inat_identity%>%
	dplyr::group_by(Ggrphc_n, year,quality_grade) %>%
	dplyr::summarise(spc_richness = length(unique(scientific_name))
					 , nb_observations = length(user_login))
####quality composition for observation in each city
observation<-ggplot(inat_clean,aes(x=year,y=nb_observations,fill=quality_grade))+
      geom_bar(width = 1, stat = "identity")+
	     	facet_wrap(~Ggrphc_n)
####quality composition for observers in each city
observedrichness<-ggplot(inat_clean,aes(x=year,y=spc_richness,fill=quality_grade))+
	geom_bar(width = 1, stat = "identity")+
	facet_wrap(~Ggrphc_n)



#write.table(inat_identity,"inat_identity.csv",sep=",")
