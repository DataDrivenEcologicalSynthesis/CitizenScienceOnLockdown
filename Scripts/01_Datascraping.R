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
					 , bounds = c(42, 141, 71, 52)
					 , maxresults = 1000)
# we get the 1000 max results
# filter it to only keep the birds (Aves), reduces to 282 results
test <- test[test$iconic_taxon_name=="Aves",]

	# with Ebird ----
test2 <- ebirdregion(loc="CA", key = "hth3fsfparb7", max=100)
# get the 100 results