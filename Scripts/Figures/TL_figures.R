#######################################
#   Figure generation - Timothy Law   #
#######################################

#libraries
library(tidyverse)

#import data
inat <- read.table("Data//04_Inat_from_clipping_NoDuplicates-csv.tsv",
				   + 				   quote= "\"", fill=T, sep='\t',comment.char = "", 
				   + 				   na.strings=c("NA", ""), header=T)
cities <- read.csv("Data/02_cities_pop_park.csv")

#-----------------------------------------------------------#
#data visualization figures