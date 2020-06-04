# Generating figures related to hypothesis 3

# INIT ----
	rm(list = ls())
	# Libraries
	library(dplyr)
	library(ggplot2)
	library(car)

# end INIT ----

# I. Importing datasets ----
# A. Cities ----
	cities_info <- read.csv("Data/02_cities_pop_park.csv")
	
# B. Inat ----
	# DO NOT RUN
	# inat <- read.table("Data/04_Inat_from_clipping_NoDuplicates-csv.tsv"
	# 				   , quote= "\""
	# 				   , fill=T
	# 				   , sep='\t'
	# 				   , comment.char = ""
	# 				   , na.strings=c("NA", "")
	# 				   , header=T)
	# inat_identity<-distinct(inat, id, .keep_all= TRUE)
	# write.csv(inat_identity, "Data/inat_identity.csv", row.names = FALSE)
	# RUN AGAIN
	inat <- read.csv("Data/inat_identity.csv")
	
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
	inat_clean$quarantine <- factor(inat_clean$quarantine)
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
	png("Figures/Hypothesis_2/AS_ParkAreaPercentage_vs_Richness_wQuarantine.png", width = 25, height = 20,units="cm",res=300)
	ggplot(data = final_dataset) +
		geom_jitter(aes(x = park.area.percentage, y = spc_richness, col=quarantine)) +
		theme_classic() + 
		labs(x="Percentage of park area", y="Species richness")
	dev.off()
	png("Figures/Hypothesis_2/AS_ParkAreaPercentage_vs_Richness.png", width = 25, height = 20,units="cm",res=300)
	ggplot(data = final_dataset) +
		geom_jitter(aes(x = park.area.percentage, y = spc_richness), size=5) +
		theme_classic() + 
		labs(x="Percentage of park area", y="Species richness")
	dev.off()
	
# end III. ----

# IV. Analysis ----
# 0. Nb of observers through time ----
	plot(nb_observators ~ year, data = final_dataset)
	mod0_a <- nlme::lme(nb_observations ~ year
						, random = ~1|Ggrphc_n
						, data = final_dataset)
	summary(mod0_a)
	# effect of the year on the nb of observators for sure
	# 48% of variation explained by the city considered
	qqnorm(mod0_a)
	e <- resid(mod0_a
			   , type = "normalized")
	plot(mod0_a$fitted[,"fixed"], e) 
	# assumptions aren't perfect but let's roll with it
	png("Figures/Hypothesis_1/AS_NObs_LMMCities_colQuarantine_bars.png", width = 25, height = 20,units="cm",res=300)
	ggplot(data=final_dataset, aes(x=year, y=nb_observations, fill = quarantine)) +
		geom_bar(stat="identity") + 
		geom_smooth(data=subset(final_dataset,year <= 2019), method=lm, col="black", fill="grey")+
		facet_wrap(~as.factor(Ggrphc_n)) + 
		theme_classic() + 
		labs(x="Year", y="Numbers of observations")
	dev.off()
	png("Figures/Hypothesis_1/AS_NObs_LMMCities_colQuarantine_points.png", width = 25, height = 20,units="cm",res=300)
	ggplot(data=final_dataset, aes(x=year, y=nb_observators, col = quarantine)) +
		geom_jitter(size=5) + 
		stat_smooth(method=lm)+
		facet_wrap(~as.factor(Ggrphc_n)) + 
		theme_classic() + 
		labs(x="Year", y="Numbers of observations")
	dev.off()
	
# A. Hypothesis 1 ----
	plot(spc_richness ~ nb_observations, data = final_dataset)
	# we can see a relation but it's not linear
	# transformations to try to make it linear
	plot(spc_richness ~ log(nb_observations), data = final_dataset)
	plot(spc_richness ~ sqrt(nb_observations), data = final_dataset)
	
	# checking the models
	summary(lm(spc_richness ~ nb_observations, data = final_dataset))
	summary(lm(spc_richness ~ log(nb_observations), data = final_dataset))
	summary(lm(spc_richness ~ sqrt(nb_observations), data = final_dataset))
	# pick the sqrt (higher R^2)
	
	# checking the assumptions and all
	mod1_a <- lm(spc_richness ~ sqrt(nb_observations)
				 , data = final_dataset)
	plot(mod1_a, which = 2)
	plot(mod1_a, which = 3)
	summary(mod1_a)
		# not perfect for heteroscedacticity, normality is okayish
		# species richness increases with nb of observators
	
	# let's try adding the city to see if it improves the model
	mod1_b <- lm(spc_richness ~ nb_observations + Ggrphc_n
				 , data = final_dataset)
	plot(mod1_b, which = 2)
	plot(mod1_b, which = 3)
	Anova(mod1_b, type=3)
	# much better!
	# should use this model
	# technically should probably use a random effect on the cities
	# so that we can generalize to Canada
	
	# Plot to represent that
	png("Figures/Hypothesis_1/AS_LMglobal_SqrtObs_richnes.png", width = 25, height = 20,units="cm",res=300)
	ggplot(data=final_dataset, aes(x=sqrt(nb_observations), y=spc_richness)) +
		geom_jitter(size=5) + 
		stat_smooth(method=lm)+
		theme_classic() + 
		labs(x="Square root of the number of observations", y="Species richness")
	dev.off()
	png("Figures/Hypothesis_1/AS_LMglobal_Obs_richness.png", width = 25, height = 20,units="cm",res=300)
	ggplot(data=final_dataset, aes(x=nb_observations, y=spc_richness)) +
		geom_jitter(size=5) + 
		theme_classic() + 
		labs(x="Number of observations", y="Species richness")
	dev.off()
	jpeg("Figures/Hypothesis_1/AS_LMcities_SqrtObs_richness.jpg", width = 900, height = 500)
	ggplot(data=final_dataset, aes(x=sqrt(nb_observations), y=spc_richness, col=Ggrphc_n)) +
		geom_jitter() + 
		stat_smooth(method=lm)
	dev.off()
	jpeg("Figures/Hypothesis_1/AS_LMglobal_SqrtObs_richness_ColCities.jpg", width = 900, height = 500)
	ggplot(data=final_dataset, aes(x=sqrt(nb_observations), y=spc_richness)) +
		geom_jitter(aes(col=Ggrphc_n)) + 
		stat_smooth(method=lm)
	dev.off()
	
	# Is there an effect of the quarantine?
	mod1_c <- lm(spc_richness ~ sqrt(nb_observations) + quarantine + Ggrphc_n
				 , data = final_dataset)
	plot(mod1_c, which = 2)
	plot(mod1_c, which = 3)
	Anova(mod1_c, type=3)
	# apparently not
	jpeg("Figures/Hypothesis_1/AS_LMglobal_SqrtObs_richness_ColQuarantine.jpg", width = 900, height = 500)
	ggplot(data=final_dataset, aes(x=sqrt(nb_observators), y=spc_richness)) +
		geom_jitter(aes(col=quarantine)) + 
		stat_smooth(method=lm)
	dev.off()
	jpeg("Figures/Hypothesis_1/AS_LMQuarantine_SqrtObs_richness.jpg", width = 900, height = 500)
	ggplot(data=final_dataset, aes(x=sqrt(nb_observators), y=spc_richness, col=quarantine)) +
		geom_jitter() + 
		stat_smooth(method=lm)
	dev.off()

	# With a random effect of the cities
	mod1_d <- nlme::lme(spc_richness ~ sqrt(nb_observations) + quarantine
						, random = ~1|Ggrphc_n
						, data = final_dataset)
	summary(mod1_d)
	# 44% of variation come from the cities
	# Should be put as a random effect
	# no effect of the quarantine
	qqnorm(mod1_d)
	e <- resid(mod1_d
			   , type = "normalized")
	plot(mod1_d$fitted[,"fixed"], e) 
	# would be fig AS_LMcities_SqrtObs_richness.jpg

# B. Hypothesis 2----
	plot(spc_richness ~ park.area.percentage
		 , data = final_dataset)
	# can't really see relationship
	summary(lm(spc_richness ~ park.area.percentage
			   , data = final_dataset))
	# doesn't appear to have a significant effect
	# with actual area
	plot(spc_richness ~ park.area
		 , data = final_dataset)
	# can't really see relationship
	summary(lm(spc_richness ~ park.area
			   , data = final_dataset))
	# R = 0.17 so i don't feel like it's relevant
	
	# what about before and after quarantine
	plot(spc_richness ~ quarantine
		 , data = final_dataset)
	mod2_a <- lm(spc_richness ~ park.area.percentage + quarantine
				 , data = final_dataset)
	Anova(mod2_a, type=3)
	# definitely not normal
	# DO NOT USE THIS MODEL
	plot(mod2_a, which = 2)
	plot(mod2_a, which = 3)

# C. Hypothesis 3----
	# full model
	
	# okay so 2 options here:
# 1. using a lm where the order of the variables matter
	mod3_a <- lm(spc_richness ~ sqrt(nb_observators) + park.area.percentage + quarantine + Ggrphc_n
				 , data = final_dataset)
	anova(mod3_a)
	plot(mod3_a, which = 2)
	plot(mod3_a, which = 3)
	# pretty okay on the assumptions
	# but doesn't feel right to use it

# 2. using a lm where the order of the variables doesn't matter
	# BUT we can't add the cities names because of fucking Winnipeg being too correlated to other cities
	mod3_b <- lm(spc_richness ~ sqrt(nb_observators) + park.area.percentage + quarantine
				 , data = final_dataset)
	Anova(mod3_b, type=3)
	# apparently no effect of the park area, effect of the quarantine though
	# can't add the cities because of fucking winnipeg
	# Maybe should like to add the size of the cities too
	# DO NOT USE THIS MODEL
	plot(mod3_b, which = 2)
	plot(mod3_b, which = 3)
	# not so good on the assumptions
	
	
	# Adding the size of the city now
	# option 1
	mod3_c <- lm(spc_richness ~ sqrt(nb_observators) + park.area.percentage + Land.area.in.square.kilometres..2016 + Ggrphc_n + quarantine
				 , data = final_dataset)
	anova(mod3_c)
	plot(mod3_c, which = 2)
	plot(mod3_c, which = 3)
	# okayish on the assumptions
	# But it doesn't feel right to use this model
	
	# option 2
	mod3_d <- lm(spc_richness ~ sqrt(nb_observators) + park.area.percentage + Land.area.in.square.kilometres..2016 + quarantine
				 , data = final_dataset)
	Anova(mod3_d, type = 3)
	plot(mod3_d, which = 2)
	plot(mod3_d, which = 3)
	# assumptions are okayish
	# still feel like we should have the cities as a random effect

# TO RECAP HYPO 3 SO FAR:
	# The models don't really fit what we're trying to say
	# or the assumptions aren't met
	# so let's go one step deeper and use a random effect model
	
	# what if I wanna do a model with random effect for the cities
	mod3_e <- nlme::lme(spc_richness ~ sqrt(nb_observations) + park.area.percentage + Land.area.in.square.kilometres..2016 + quarantine + Population.density.per.square.kilometre..2016
						, random = ~1|Ggrphc_n
						, data = final_dataset)
	summary(mod3_e)
	# intercept / intercept + residual gives tot variation coming from cities
	# 10.2 / (10.2 + 12.2) = 0.48% 
	# MUST BE A RANDOM EFFECT
	qqnorm(mod3_e)
	e <- resid(mod3_e
			   , type = "normalized")
	plot(mod3_e$fitted[,"fixed"], e) 
	
	# With this model we have 48% variation coming from the cities
	# an effect of the nb of observators

	# Trying to make a plot for this
	# can pretty much say that there's not an effect of the quarantine
	# THIS IS ONLY FOR VISUALIZATING 
	# You can't plot a model as complicated as the one we have
	# Or idk how to
	# trying a facet
	
	jpeg("Figures/Hypothesis_3/AS_LMCities_SqrtObs_richness_facet.jpg", width = 900, height = 500)
	ggplot(data=final_dataset, aes(x=sqrt(nb_observators), y=spc_richness, col=Ggrphc_n)) +
		geom_jitter() + 
		stat_smooth(method=lm) +
		facet_wrap(~as.factor(Ggrphc_n))
	dev.off()
	png("Figures/Hypothesis_3/AS_LMQuarantines_SqrtObs_richness_facet.png", width = 25, height = 20,units="cm",res=300)
	ggplot(data=final_dataset, aes(x=sqrt(nb_observations), y=spc_richness, col=quarantine)) +
		geom_jitter(size=3) + 
		stat_smooth(method=lm) +
		theme_classic() + 
		labs(x="Square root of the number of observations", y="Species richness") +
		facet_wrap(~as.factor(Ggrphc_n))
	dev.off()
	jpeg("Figures/Hypothesis_3/AS_LMCities_SqrtObs_richness_facet_colQuarantine.jpg", width = 900, height = 500)
	ggplot(data=final_dataset, aes(x=sqrt(nb_observators), y=spc_richness)) +
		geom_jitter(aes(col=quarantine)) + 
		stat_smooth(method=lm) +
		facet_wrap(~as.factor(Ggrphc_n))
	dev.off()
	png("Figures/Hypothesis_2/AS_PopulationDensity_vs_Richness.png", width = 25, height = 20,units="cm",res=300)
	ggplot(data=final_dataset, aes(x=Population.density.per.square.kilometre..2016, y=spc_richness)) +
		geom_jitter(size=5) +
		theme_classic() + 
		labs(x="Population density in 2016", y="Species richness", size=4)
	dev.off()
	png("Figures/Hypothesis_3/AS_EffectQuarantine.png", width = 25, height = 20,units="cm",res=300)
	
	ggplot(data=final_dataset, aes(x=sqrt(nb_observations), y=spc_richness, col=quarantine)) +
		geom_jitter(size=5) + 
		stat_smooth(data=subset(final_dataset,year <= 2019), method=lm, col="black") +
		theme_classic() +
		labs(x="Square root of the number of observations", y="Species richness", size=4)
	dev.off()
	
	
# end IV. ----