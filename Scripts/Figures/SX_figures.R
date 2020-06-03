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
#write.table(inat_identity,"inat_identity.csv",sep=",")
###figures for quality_grade
inat_clean_observation <- inat_identity%>%
	dplyr::group_by(Ggrphc_n, year,quality_grade) %>%
	dplyr::summarise(spc_richness = length(unique(scientific_name))
					 , nb_observations = length(user_login))
####quality composition for observation in each city
jpeg("Figures/Hypothesis_1/SX_quality_grade_observation.jpg", width = 1000, height = 700)
ggplot(inat_clean_observation,aes(x=year,y=nb_observations,fill=quality_grade))+
      geom_bar(width = 1, stat = "identity")+
	     	facet_wrap(~Ggrphc_n)
dev.off()
####quality composition for observers in each city
jpeg("Figures/Hypothesis_1/SX_quality_grade_richness.jpg", width = 1000, height = 700)
ggplot(inat_clean_observation,aes(x=year,y=spc_richness,fill=quality_grade))+
	geom_bar(width = 1, stat = "identity")+
	facet_wrap(~Ggrphc_n)
dev.off()
###figures for users
inat_clean_users <- inat_identity%>%
	dplyr::group_by(Ggrphc_n, year) %>%
	dplyr::summarise(spc_richness = length(unique(scientific_name))
					 , nb_observations = length(user_login)
					 , nb_observators = length(unique(user_login)))
inat_clean_users$quarantine <- ifelse(inat_clean_users$year == 2020, "yes", "no")
inat_clean_users$quarantine <- factor(inat_clean_users$quarantine)
rm(inat)
jpeg("Figures/Hypothesis_1/SX_users.jpg", width = 1000, height = 700)
ggplot(inat_clean_users,aes(x=year,y=nb_observators,fill=quarantine ))+
	geom_bar(width = 1, stat = "identity")+
	facet_wrap(~Ggrphc_n)
dev.off()
###figures for every user
inat_clean_perusers <- inat_identity%>%
		dplyr::group_by(user_login, year,Ggrphc_n,)%>%
	   dplyr::summarise(nb_observations = length(user_login))
jpeg("Figures/Hypothesis_1/SX_peruser.jpg", width = 1000, height = 700)
ggplot(inat_clean_perusers,aes(fill=year,user_login,nb_observations))+
	geom_bar(position="dodge", stat="identity")+
	facet_wrap(~year)
dev.off()
###figures for time series 
inat_clean_time<- inat_identity%>%
	mutate(observed_on = as.Date(observed_on))%>%
    mutate(ym = format(observed_on, '%Y-%m')) %>%
   dplyr::group_by(ym,Ggrphc_n)%>%
	dplyr::summarise(spc_richness = length(unique(scientific_name))
					 , nb_observations = length(user_login)
					 , nb_observators = length(unique(user_login)))
#number of observations
jpeg("Figures/Hypothesis_1/SX_timeym_observation.jpg", width = 1000, height = 700)
ggplot(inat_clean_time, aes(x =ym, y = nb_observations,group=1)) +
	geom_line(aes(color=Ggrphc_n)) +
	facet_wrap(~Ggrphc_n)+
	theme_classic()+
	theme(axis.text.x=element_text(angle=60, hjust=1)) 
dev.off()
#number of observers
jpeg("Figures/Hypothesis_1/SX_timeym_observers.jpg", width = 1000, height = 700)
ggplot(inat_clean_time, aes(x =ym, y = nb_observators,group=1)) +
	geom_line(aes(color=Ggrphc_n)) +
	facet_wrap(~Ggrphc_n)+
	theme_classic()+
	theme(axis.text.x=element_text(angle=60, hjust=1)) 
dev.off()
#number of richness
jpeg("Figures/Hypothesis_1/SX_timeym_richness.jpg", width = 1000, height = 700)
ggplot(inat_clean_time, aes(x =ym, y = spc_richness,group=1)) +
	geom_line(aes(color=Ggrphc_n)) +
	facet_wrap(~Ggrphc_n)+
	theme_classic()+
	theme(axis.text.x=element_text(angle=60, hjust=1)) 
dev.off()