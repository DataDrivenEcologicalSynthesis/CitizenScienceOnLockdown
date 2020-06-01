#######################################
#   Figure generation - Timothy Law   #
#######################################

#libraries
library(tidyverse)

#import data
inat <- read.table("Data//04_Inat_from_clipping_NoDuplicates-csv.tsv", 
				   quote= "\"", fill=T, sep='\t',comment.char = "", 
				   na.strings=c("NA", ""), header=T)
cities <- read.csv("Data/02_cities_pop_park.csv")

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
