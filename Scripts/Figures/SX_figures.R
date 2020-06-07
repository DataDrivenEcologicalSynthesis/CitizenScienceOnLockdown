# Generating figures related to hypothesis 1
#import data
# INIT ----
# INIT ----
rm(list = ls())

install.packages("vegan")
install.packages("fuzzySim")
install.packages("fossil")
# Libraries
library(dplyr)
library(ggplot2)
# library(vegan)
# library(reshape2)
# library(fuzzySim)
library(fossil)
# data(BCI, package = "vegan")
# BCI2 <- BCI[1:26, ]
# data(rotifier)

# 		
# 	plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
# 	abline(0, 1)
# 	rarecurve(rare, step = 20, sample = raremax, col = "blue", cex = 0.6)
# biodiversity<-inat_clean_rarefaction

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
# 
# dup<-inat%>% 
# 	group_by(id) %>% 
# filter(n() != 1)
# write.table(dup,"Data/04_dup.csv",sep=",")

#final data with unique id
inat_identity<-distinct(inat, id, .keep_all= TRUE)
#write.table(inat_identity,"Data/04_inat_identity.csv",sep=",")

#figures of number of species and richness over years
###figures for quality_grade
inat_clean_observation <- inat_identity%>%
	dplyr::group_by(Ggrphc_n, year,quality_grade) %>%
	dplyr::summarise(spc_richness = length(unique(scientific_name))
					 , nb_observations = length(user_login))
quality_percent<-inat_clean_observation%>%
	dplyr::group_by(Ggrphc_n, year) %>% 
	mutate(percent_nb_observations  = nb_observations /sum(nb_observations),
           percent_spc_richness   = spc_richness /sum(spc_richness ))

####quality percentage composition for observation in each city
jpeg("Figures/Hypothesis_1/SX_quality_grade_observation_percent.jpg", width = 1000, height = 700)
ggplot(quality_percent,aes(x=year,y=percent_nb_observations,fill=quality_grade))+
      geom_bar(width = 1, stat = "identity")+
	     	facet_wrap(~Ggrphc_n)
dev.off()
####quality composition percentage  for species richness in each city
jpeg("Figures/Hypothesis_1/SX_quality_grade_richness_percent.jpg", width = 1000, height = 700)
ggplot(quality_percent,aes(x=year,y=percent_spc_richness,fill=quality_grade))+
	geom_bar(width = 1, stat = "identity")+
	facet_wrap(~Ggrphc_n)
dev.off()
####quality composition for observation in each city
jpeg("Figures/Hypothesis_1/SX_quality_grade_observation.jpg", width = 1000, height = 700)
ggplot(inat_clean_observation,aes(x=year,y=nb_observations,fill=quality_grade))+
	geom_bar(width = 1, stat = "identity")+
	facet_wrap(~Ggrphc_n)
dev.off()
####quality composition for species richness in each city
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

inat_clean_timenew<- inat_identity%>%
	mutate(observed_on = as.Date(observed_on))%>%

	dplyr::group_by(observed_on ,Ggrphc_n)%>%
	dplyr::summarise(spc_richness = length(unique(scientific_name))
					 , nb_observations = length(user_login)
					 , nb_observators = length(unique(user_login)))
# inat_clean_timenewnew<-inat_clean_timenew%>%
# 	mutate(observed_on = as.Date(observed_on))%>%
# mutate(year= format(observed_on, '%Y')) 
# mutate(date= format(observed_on, '%m-%d')) 
#number of observations
jpeg("Figures/Hypothesis_1/SX_timeym_observationnew.jpg", width = 1000, height = 700)
ggplot(inat_clean_timenewnew, aes(x =observed_on, y = nb_observations,group=1)) +
	geom_line(aes(color=Ggrphc_n)) +
	facet_wrap(~year)+

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
## compare community
inat_clean_rarefaction_time<- inat_identity%>%
	dplyr::group_by(observed_on,quality_grade)%>%
	#dplyr::summarise(spc_richness = length(unique(scientific_name)))%>%
	select(observed_on, scientific_name,quality_grade,year)

inat_clean_rarefaction_time$freq<-1
community_time<-aggregate(freq~scientific_name+quality_grade+year,inat_clean_rarefaction_time,sum)
communty_time_percent<-community_time%>%
	dplyr::group_by(year,scientific_name) %>% 
	mutate(percent_freq = freq /sum(freq),totalfreq=sum(freq))
communty_time_percent$quarantine <- ifelse(communty_time_percent$year == 2020, "yes", "no")
communty_time_percent$quarantine <- factor(communty_time_percent$quarantine)

jpeg("Figures/Hypothesis_1/SX_time_speciesfreq.jpg", width = 1000, height = 700)
ggplot(communty_time_percent,aes(x=totalfreq,y=percent_freq ))+
geom_point(aes(color=quality_grade))+
	facet_wrap(~quarantine)+
theme_classic()+
	xlab("Number of observations of a species")+
	ylab("percentage of each quality grade")+
	theme(axis.text.x=element_text(angle=60, hjust=1)) 
#write.table(communty_time_percent,"Data/speciesabundanceyear.csv",sep=",")
write.csv(communty_time_percent, "Data/speciesabundanceyear.csv", row.names = FALSE)
dev.off()
#####the obseration of frequncy for each species over years
jpeg("Figures/Hypothesis_1/SX_time_speciesfreqyear.jpg", width = 1000, height = 700)
ggplot(communty_time_percent,aes(x=year,y=totalfreq ))+
	geom_point()+
		theme_classic()+
	xlab("year")+
	ylab("species frequency")

dev.off()

A<-create.matrix(community, tax.name = "scientific_name", locality = "observed_on",
				 abund=TRUE, abund.col="freq")
timemt<-as.data.frame(t(A))
rare<-rarenew%>%
	filter(rowSums(rarenew)!=1)
#rare<- splist2presabs(inat_clean_rarefaction, sites.col = 1, sp.col = 2, keep.n = FALSE)
#rarenew<-rare[,2:387]
raremax <- min(rowSums(rare))
S <- specnumber(rare) # observed number of species
Srare <- rarefy(rare, raremax)
##making more appendix figures
inat <- read.csv("Data/inat_identity.csv")
inat_clean <- inat %>%
	dplyr::group_by(Ggrphc_n, year) %>%
	dplyr::summarise(spc_richness = length(unique(scientific_name))
					 , nb_observations = length(user_login)
					 , nb_observators = length(unique(user_login)))
# coding the quarantine
inat_clean$quarantine <- ifelse(inat_clean$year == 2020, "yes", "no")
inat_clean$quarantine <- factor(inat_clean$quarantine)
rm(inat)
jpeg("Figures/Hypothesis_1/SX_supp_observersandobservation.jpg", width = 1000, height = 700)
ggplot(inat_clean,aes(x=nb_observators,y=nb_observations))+
	geom_point(aes(color=as.factor(inat_clean$year),size=5))+
		stat_smooth(method=lm)+
	labs(color="year",x="Number of observators",y="Number of observations")+
	theme_classic(base_size=20)

dev.off()
#figures of number of species and richness over years
jpeg("Figures/Hypothesis_1/SX_supp_yearobservators.jpg", width = 1000, height = 700)
ggplot(inat_clean,aes(x=year,y=nb_observators))+
	geom_point(aes(color=quarantine),size=5)+
	stat_smooth(method=lm)+
	facet_wrap(~Ggrphc_n)+
	labs(x="year",y="Number of observators")+
	theme_classic(base_size=16)

dev.off()
jpeg("Figures/Hypothesis_1/SX_supp_yearrichness.jpg", width = 1000, height = 700)
ggplot(inat_clean,aes(x=year,y=spc_richness))+
	geom_point(aes(color=quarantine),size=5)+
	facet_wrap(~Ggrphc_n)+
	labs(x="year",y="species richness")+
	theme_classic(base_size=16)

dev.off()