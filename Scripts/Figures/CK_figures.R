### Generating figures based on # common species observations

# Load required packages
library(tidyverse)

#Load inaturalist data
nat <- read.table('04_Inat_from_clipping_NoDuplicates-csv.tsv',
                  quote= "\"", 
                  fill=T, 
                  sep='\t',
                  comment.char = "", 
                  na.strings=c("NA", ""),
                  header=T)

# Convert "datetime" variable to yyyy-mm format
nat$datetime <- gsub(" .*", "", nat$datetime)
nat$datetime <- as.Date(nat$datetime)
nat$datetime <- format(nat$datetime, format = "%y-%m")

# Create dataframe for red-wing blackbird observation totals for each applicable city
nat_rwbb <- nat %>%
        select(Ggrphc_n, year, common_name) %>%
        filter(common_name == "Red-winged Blackbird") %>%
        group_by(Ggrphc_n, year) %>%
        summarise(Red_winged_Blackbird_Obs = length(common_name)) %>%
        mutate(quarantine = ifelse(year == 2020, "yes", "no"))

# Repeat for Canada goose & Mallard
# Canada Goose:
nat_cg <- nat %>%
        select(Ggrphc_n, year, common_name) %>%
        filter(common_name == "Canada Goose") %>%
        group_by(Ggrphc_n, year) %>%
        summarise(Canada_Goose_Obs = length(common_name)) %>%
        mutate(quarantine = ifelse(year == 2020, "yes", "no"))

# Mallard:
nat_mal <- nat %>%
        select(Ggrphc_n, year, common_name) %>%
        filter(common_name == "Mallard") %>%
        group_by(Ggrphc_n, year) %>%
        summarise(Mallard_Obs = length(common_name)) %>%
        mutate(quarantine = ifelse(year == 2020, "yes", "no"))

# Create single data frame with all three common species
nat_common <- nat %>%
        select(Ggrphc_n, year, common_name) %>%
        filter(common_name == c("Mallard", "Canada Goose", "Red-winged Blackbird")) %>%
        group_by(Ggrphc_n, year, common_name) %>%
        summarise(Common_sp_obs = length(common_name))

#####################################################################################################

# Plot common species observations by year for each available city
# Red-winged blackbird:
jpeg("figures/CK_red_winged_bb_obs.jpg", width = 1000, height = 700)
ggplot(nat_rwbb) + 
        geom_jitter(aes(x = year, y = Red_winged_Blackbird_Obs, col = factor(quarantine))) +
        facet_wrap(~as.factor(Ggrphc_n))
dev.off()

# Canada goose:
jpeg("figures/CK_canada_goose_obs.jpg", width = 1000, height = 700)
ggplot(nat_cg) + 
        geom_jitter(aes(x = year, y = Canada_Goose_Obs, col = factor(quarantine))) +
        facet_wrap(~as.factor(Ggrphc_n))
dev.off()        

# Mallard:
jpeg("figures/CK_mallard_obs.jpg", width = 1000, height = 700)
ggplot(nat_mal) +
        geom_jitter(aes(x = year, y = Mallard_Obs, col = factor(quarantine))) +
        facet_wrap(~as.factor(Ggrphc_n))
dev.off()

# All three common species:
jpeg("figures/CK_common_sp_obs.jpg", width = 1000, height = 700)
ggplot(nat_common) +
        geom_jitter(aes(x = year, y = Common_sp_obs, col = factor(common_name))) +
        facet_wrap(~as.factor(Ggrphc_n))
dev.off()

