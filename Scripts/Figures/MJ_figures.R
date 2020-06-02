# Generating figures related to hypothesis 2
# Are we observing different species because the cities are quieter?

# Load useful librairies
rm(list = ls())
library(dplyr)
library(tidyverse)
library(ggplot2)
library(car)
library(cowplot)

# Importing datasets - Based on Aliénor's AS_Figrues.R code
cities_info <- read.csv("Data/02_cities_pop_park.csv")
inat <- read.table("Data/04_Inat_from_clipping_NoDuplicates-csv.tsv"
				   , quote= "\""
				   , fill=T
				   , sep='\t'
				   , comment.char = ""
				   , na.strings=c("NA", "")
				   , header=T)

# Cities
cities_info_clean <- cities_info[, c("Geographic.name"
									 , "Population..2016"
									 , "Total.private.dwellings..2016"
									 , "Land.area.in.square.kilometres..2016"
									 , "Population.density.per.square.kilometre..2016"
									 , "park.area"
									 , "park.area.percentage"
)]
rm(cities_info)

# Species composition
# Create an observed community composition table
# This table needs to have cities and years as rows, species as columns and number of observations in values
iNat_species=inat %>%
	select(Ggrphc_n, year, scientific_name)%>%
	unite(col="City_Year", Ggrphc_n, year, sep = "_", remove=FALSE)%>%
	count(City_Year, scientific_name)%>%# This generates the number of reports of a species in a city per year
	pivot_wider(id_cols=City_Year, names_from=scientific_name, values_from=n)%>%
	separate(City_Year, into=c("city", "year"), remove=FALSE)

# coding the quarantine
iNat_species$quarantine <- ifelse(iNat_species$year == 2020, "yes", "no")
iNat_species$quarantine <- factor(iNat_species$quarantine)
rm(inat)

# C. Fusing the datasets into one
#final_dataset <- left_join(inat_clean
#						   , cities_info_clean
#						   , by=c("Ggrphc_n" = "Geographic.name"))
#rm(inat_clean, cities_info_clean)
# end III. ----

# Calculate species turnover for each city among years
library(codyn)
# Get 
iNat_spe_counts=inat %>%
	select(Ggrphc_n, year, scientific_name)%>%
	unite(col="City_Year", Ggrphc_n, year, sep = "_", remove=FALSE)%>%
	count(City_Year, scientific_name)%>%
	separate(City_Year, into=c("city", "year"), remove=FALSE)

iNat_spe_counts$scientific_name=as.factor(iNat_spe_counts$scientific_name)
iNat_spe_counts$city=as.factor(iNat_spe_counts$city)
iNat_spe_counts$year=as.integer(iNat_spe_counts$year)
iNat_spe_counts$n=as.numeric(iNat_spe_counts$n)

iNat_spe_counts$city <- factor(iNat_spe_counts$city, levels = c("Vancouver","Surrey", "Calgary","Edmonton", "Saskatoon",
														"Winnipeg","London","Hamilton", "Brampton", "Mississauga",
														"Toronto", "Markham","Vaughan", "Ottawa", "Gatineau" , "Laval" , 
														"Montreal","Longueuil",  "Quebec",  "Halifax"))

# Calculate relative total turnover within replicates
spe.turn=turnover(iNat_spe_counts, time.var="year", species.var="scientific_name", abundance.var="n",replicate.var="city",metric = "total")
spe.turn=data.frame(spe.turn)

# Calculate relative species appearances within replicates
spe.appear=turnover(iNat_spe_counts, time.var="year", species.var="scientific_name", abundance.var="n",replicate.var="city",metric = "appearance")
spe.appear=data.frame(spe.appear)

# Merge the two and add city info
spe.change=left_join(spe.turn, spe.appear)

spe.change.tot=left_join(spe.change, cities_info_clean, by=c("city"="Geographic.name"))%>%
	rename("Pop.Density" = "Population.density.per.square.kilometre..2016")

# Plot both variables
p.turnover=ggplot(data=spe.change.tot, aes(x=year, y=total, group=city))+
	geom_point(aes(col=city))+
	stat_smooth(method="lm", alpha=0, aes(col=city))+
	scale_y_continuous(name="Relative total species turnover", limits=c(0,1.25))+
	facet_wrap(~city)+
	theme_bw()+
	theme(legend.position = "none")
p.turnover

p.appear=ggplot(data=spe.change.tot, aes(x=year, y=appearance, group=city))+
	geom_point(aes(col=city))+
	stat_smooth(method="lm", alpha=0, aes(col=city))+
	scale_y_continuous(name="Relative species appearance within cities", limits=c(0,1.25))+
	facet_wrap(~city)+
	theme_bw()+
	theme(legend.position = "none")
p.appear

# Overall, it seems like species turnover was higher in the earlier day of use of the app, and has decreased since.
# We do not see a massive change in reported species composition due to quarantine.
# No clear trend emerge for the appearance of new species being reported between cities. It's very variable. 




