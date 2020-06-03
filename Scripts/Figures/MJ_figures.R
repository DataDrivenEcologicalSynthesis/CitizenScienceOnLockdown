# Generating figures related to hypothesis 2
# Are we observing different species because the cities are quieter?

# Load useful librairies
rm(list = ls())
library(dplyr)
library(tidyverse)
library(ggplot2)
library(car)
library(cowplot)
library(magrittr)
library(vegan)
install.packages("remotes")
remotes::install_github("jfq3/QsRutils")
library(jfq3/QsRutils)

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

png("Figures/Exploratory/MJ_spc_turnover.png", width = 1700, height = 1000, units ="px", res=150)
p.turnover
dev.off()

p.appear=ggplot(data=spe.change.tot, aes(x=year, y=appearance, group=city))+
	geom_point(aes(col=city))+
	stat_smooth(method="lm", alpha=0, aes(col=city))+
	scale_y_continuous(name="Relative species appearance within cities", limits=c(0,1.25))+
	facet_wrap(~city)+
	theme_bw()+
	theme(legend.position = "none")

png("Figures/Exploratory/MJ_spc_appear.png", width = 1700, height = 1000, units ="px", res=150)
p.appear
dev.off

# Overall, it seems like species turnover was higher in the earlier day of use of the app, and has decreased since.
# We do not see a massive change in reported species composition due to quarantine.
# No clear trend emerge for the appearance of new species being reported between cities. It's very variable. 

############################################################################
# Species composition
# Create an observed community composition table
# This table needs to have cities and years (2019 and 2020 only) as rows, species as columns and number of observations in values
# The rationale for not doing it by year is to simplify the plot at the end and deal with missing data (missing 6 city x year combos)

inat1=inat %>%
	select(Ggrphc_n, year, scientific_name)%>%
	filter(year %in% c(2019, 2020))%>%
	unite(col="city_year", Ggrphc_n, year, sep = "_", remove=FALSE)%>%
	count(city_year, scientific_name)%>%	# This generates the number of reports of a species in a city per year
	separate(city_year, into=c("Ggrphc_n", "year"), remove=FALSE)
inat1$n=as.numeric(inat1$n)

inat2=left_join(inat1, city_years)%>%
	pivot_wider(id_cols=city_year, names_from=scientific_name, values_from=n)%>%
	replace(., is.na(.), "0")%>%
	separate(city_year, into=c("Ggrphc_n", "year"), remove=FALSE)

# Convert to factors or integers
inat2$Ggrphc_n=as.factor(inat2$Ggrphc_n)
inat2$year=as.integer(inat2$year)
inat2$city_year=as.factor(inat2$city_year)

inat3=inat2 %>%
	mutate_if(is.character,as.numeric)
str(inat3)

inat4=inat3[,4:357]

# Multivariate anova per permutation of the observed community to test for year effect (city as a random effect)
spe.bray=vegdist(inat3[,4:357], method="bray") # create distance matrix

spe.adonis<- adonis(formula = spe.bray ~ as.factor(inat3$year), strata=inat3$Ggrphc_n,
			  data = inat3, 
			  perm = 5000) # no strata
spe.adonis # Significant year effect in sp composition when cities are used as random effect

# Now let's use a non-metric multidimentional scaling ordination to visualise the results

###Run the NMDS with two dimensions.  ###
set.seed(5) # random number selected to get repeatable results

spe.nmds <- metaMDS(spe.bray, k = 2, trymax = 500) # the 3 dimensions is a better fit, but we will use 2 dimensions for the project
# Our interpretation is likely to focus on axes 1 and 2 though
spe.nmds$stress # stress of 0.12 
stressplot(spe.nmds)

#Plot the NMDS using ggplot2

spp.fit <- envfit(spe.nmds, inat4, permutations = 999) # this fits species vectors
site.scrs <- as.data.frame(scores(spe.nmds, display = "sites")) #save NMDS results into dataframe
# Add gps coordinates and site depth to the scores
#gps.bda=gps.sites%>%
#	filter(!Sites %in% c("LA05", "LA06"))
site.scrs <- cbind(site.scrs, inat3[1:3]) 

site.scrs$Ggrphc_n <- factor(site.scrs$Ggrphc_n, levels = c("Vancouver","Surrey", "Calgary","Edmonton", "Saskatoon",
																"Winnipeg","London","Hamilton", "Brampton", "Mississauga",
																"Toronto", "Markham","Vaughan", "Ottawa", "Gatineau" , "Laval" , 
																"Montreal","Longueuil",  "Quebec",  "Halifax"))

#Prep species data
spp.scrs <- as.data.frame(scores(spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs <- subset(spp.scrs, pval<=0.001) #subset data to show species significant at 0.1
#Basic plot with ggplot2. Colors represent cities
#Code obtained from https://www.rpubs.com/RGrieger/545184

# Generate standard deviation ellipses
NMDS = data.frame(MDS1 = spe.nmds$points[,1], MDS2 = spe.nmds$points[,2],group=as.factor(inat3$year))
NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$group),mean)
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
	theta <- (0:npoints) * 2 * pi/npoints
	Circle <- cbind(cos(theta), sin(theta))
	t(center + scale * t(Circle %*% chol(cov)))
}

df_ell <- data.frame()
for(g in levels(NMDS$group)){
	df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g,],
													 veganCovEllipse(cov.wt(cbind(MDS1,MDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2)))))
								  ,group=g))
}
site.scrs$year=as.factor(site.scrs$year)

# Plot the NMDS
nmds.plot <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
	geom_path(data=df_ell, aes(x=MDS1, y=MDS2,colour=group), size=1, linetype=1, show.legend = F)+
	geom_point(aes(NMDS1, NMDS2, colour = site.scrs$Ggrphc_n,fill=site.scrs$Ggrphc_n, shape=year), size = 3)+ #adds site points to plot, colour determined by Depth
	annotate("text",x=NMDS.mean$MDS1,y=NMDS.mean$MDS2,label=NMDS.mean$group)+
	theme_bw()+ 
	labs(colour = "Cities")+ # add legend labels for Management and Landuse
	scale_shape_manual(values=c(1,19))+
	theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10))+ # add legend at right of plot
	theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+ 
	guides(fill=FALSE)
nmds.plot

#Adding the species correlated with site positionning in ordination space at p<0.0005
# Aaaahhhh so all species are more abundant in 2020 when showing p<0.05 there are too many
nmds.plot.spe=nmds.plot+
	geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of  species significant at alpha=0.05
	ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for  species, use ggrepel::geom_text_repel so that labels do not overlap
	scale_alpha(guide = 'none')
nmds.plot.spe
