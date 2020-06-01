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
	# getting the species richness, nb of observations and observers
	inat_clean <- inat %>%
		dplyr::group_by(Ggrphc_n, year) %>%
		dplyr::summarise(spc_richness = length(unique(scientific_name))
						 , nb_observations = length(user_login)
						 , nb_observators = length(unique(user_login)))
	# coding the quarantine
	inat_clean$quarantine <- ifelse(inat_clean$year == 2020, "yes", "no")
	rm(inat)

# C. Fusing the datasets into one
	final_dataset <- left_join(inat_clean
							   , cities_info_clean
							   , by=c("Ggrphc_n" = "Geographic.name"))
	rm(inat_clean, cities_info_clean)
# end III. ----

# III. Figures ----
# A. Hypothesis 1----
	# Per city
	# nb of observators against species richness
	jpeg("Figures/Hypothesis_1/AS_Observators_vs_Richness_wCitiesYear.jpg", width = 900, height = 500)
	ggplot(data = final_dataset) +
		geom_jitter(aes(x = nb_observators, y = spc_richness, col=factor(year))) +
		facet_wrap(~as.factor(Ggrphc_n))
	dev.off()
	
	# nb of observations against species richness
	jpeg("Figures/Hypothesis_1/AS_Observations_vs_Richness_wCitiesYear.jpg", width = 1000, height = 700)
	ggplot(data = final_dataset) +
		geom_jitter(aes(x = nb_observations, y = spc_richness, col=factor(year))) +
		facet_wrap(~as.factor(Ggrphc_n))
	dev.off()

# B. Hypothesis 2 ----
	# species richness against park size
	jpeg("Figures/Hypothesis_2/AS_ParkArea_vs_Richness_wCitiesQuarantine.jpg", width = 900, height = 500)
	ggplot(data = final_dataset) +
		geom_jitter(aes(x = park.area, y = spc_richness, col=factor(quarantine))) +
		facet_wrap(~as.factor(Ggrphc_n))
	dev.off()
	
	# species richness against percentage of park coverage
	jpeg("Figures/Hypothesis_2/AS_ParkAreaPercentage_vs_Richness_wCitiesQuarantine.jpg", width = 900, height = 500)
	ggplot(data = final_dataset) +
		geom_jitter(aes(x = park.area.percentage, y = spc_richness, col=factor(quarantine))) +
		facet_wrap(~as.factor(Ggrphc_n))
	dev.off()
	jpeg("Figures/Hypothesis_2/AS_ParkAreaPercentage_vs_Richness_wQuarantine.jpg", width = 900, height = 500)
	ggplot(data = final_dataset) +
		geom_jitter(aes(y = park.area.percentage, x = spc_richness, col=factor(quarantine)))
	dev.off()

# end III. ----

# IV. Analysis ----
# A. Hypothesis 1 ----
	plot(spc_richness ~ nb_observators, data = final_dataset)
	# we can see a relation but it's not linear
	# transformations to try to make it linear
	
	plot(spc_richness ~ log(nb_observators), data = final_dataset)
	plot(spc_richness ~ sqrt(nb_observators), data = final_dataset)
	
	# checking the models
	summary(lm(spc_richness ~ nb_observators, data = final_dataset))
	summary(lm(spc_richness ~ log(nb_observators), data = final_dataset))
	summary(lm(spc_richness ~ sqrt(nb_observators), data = final_dataset))
	# pick the sqrt
	
	# checking the assumptions and all
	mod1_a <- lm(spc_richness ~ sqrt(nb_observators), data = final_dataset)
	plot(mod1_a, which = 2)
	plot(mod1_a, which = 3)
		# not perfect for heteroscedacticity, normality is okayish
		# species richness increases with nb of observators
	
	# let's try adding the city to see if it improves the model
	anova(lm(spc_richness ~ nb_observators + Ggrphc_n, data = final_dataset))
	# it increases differently depending on the city
	mod1_b <- lm(spc_richness ~ nb_observators + Ggrphc_n, data = final_dataset)
	plot(mod1_b, which = 2)
	plot(mod1_b, which = 3)
	# much better!
	# should use this model
	# technically should probably use a random effect on the cities
	# so that we can generalize to Canada

# B. Hypothesis 2----
	plot(spc_richness ~ park.area.percentage, data = final_dataset)
	# can't really see relationship
	summary(lm(spc_richness ~ park.area.percentage, data = final_dataset))
	# doesn't appear to have a significant effect

# C. Hypothesis 3----
	# full model
	mod3 <- lm(spc_richness ~ nb_observators + park.area.percentage + Ggrphc_n + quarantine, data = final_dataset)
	anova(mod3)
	# apparently no effect of the park area
	plot(mod3, which = 2)
	plot(mod3, which = 3)
	# pretty okay on the assumptions

# end IV. ----