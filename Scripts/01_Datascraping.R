#### Downloading data from inaturalist ####

# INIT ----
	# Libraries ----
	library(rinat)
	library(rebird)

# end INIT

# I. Downloading data ----
	# with Inaturalist ----
test <- get_inat_obs(year = 2020
					 , month = 4
					 , day = 5
					 , geo = T
					 , bounds = c(42, -141, 71, -52)
					 , maxresults = 1000, 
					 taxon_name="Aves")# I think this last argument selects only birds
# we get the 1000 max results
# filter it to only keep the birds (Aves), reduces to 282 results
test <- test[test$iconic_taxon_name=="Aves",]

	# with Ebird ----
test2 <- ebirdregion(loc="CA", key = "hth3fsfparb7", max=100)
# get the 100 results


# I. Downloading data take 2---- With the current filters I can download data for one month at the time. 
# If we focus on just April and March and on the years >2015 it's possibily feasible
# with Inaturalist ----
test2 <- get_inat_obs(year = 2020
					 , month = 4
					 , geo = T
					 , bounds = c(40, -130, 60, -52) # updated the coordinates to select just southern Canada
					 ,taxon_name="Aves")# I think this last argument selects only birds
