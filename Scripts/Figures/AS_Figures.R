# Generating figures related to hypothesis 3

# INIT ----
	rm(list = ls())
	# Libraries
	library(dplyr)
	library(ggplot2)

# end INIT ----

# I. Importing datasets ----
cities_info <- read.csv("Data/02_cities_pop_park.csv")
inat <- read.table("Data/04_Inat_from_clipping_NoDuplicates-csv.tsv"
				   , quote= "\""
				   , fill=T
				   , sep='\t'
				   , comment.char = ""
				   , na.strings=c("NA", "")
				   , header=T)
# end I. ----

# II. Formatting the datasets ----
# A. Cities ----
cities_info_clean <- cities_info[, c("Geographic.name"
									 , "Population..2016"
									 , "Total.private.dwellings..2016"
									 , "Land.area.in.square.kilometres..2016"
									 , "Population.density.per.square.kilometre..2016"
									 , "park.area"
									 , "park.area.percentage"
									 )]
rm(cities_info)

# B. Inat ----
# getting the species richness
inat_clean <- inat %>%
	dplyr::group_by(Ggrphc_n, year) %>%
	dplyr::summarise(spc_richness = length(unique(scientific_name))
					 , nb_observations = length(user_login)
					 , nb_observators = length(unique(user_login)))
inat_clean$quarantine <- ifelse(inat_clean$year == 2020, "yes", "no")
rm(inat)

# C. Fusing the datasets into one
final_dataset <- left_join(inat_clean
						   , cities_info_clean
						   , by=c("Ggrphc_n" = "Geographic.name"))
rm(inat_clean, cities_info_clean)
# end III. ----

# III. Figures ----

# Per city
# nb of observators against species richness
jpeg("Figures/Hypothesis_1/AS_Observators_vs_Richness_wCitiesYear.jpg", width = 350, height = 350)
ggplot(data = final_dataset) +
	geom_jitter(aes(x = nb_observators, y = spc_richness, col=factor(year))) +
	facet_wrap(~as.factor(Ggrphc_n))
dev.off()

# nb of observations against species richness
jpeg("Figures/Hypothesis_1/AS_Observations_vs_Richness_wCitiesYear.jpg", width = 350, height = 350)
ggplot(data = final_dataset) +
	geom_jitter(aes(x = nb_observations, y = spc_richness, col=factor(year))) +
	facet_wrap(~as.factor(Ggrphc_n))
dev.off()

