# stop(paste("This script was used to compile the gbif data sets used in the",
#            "analysis. Running it requires setting your GBIF credentials in",
#            "some way (usually in .Renviron). See help('Startup') or the rgbif",
#            "documentation, especially rgbif::occ_download.Running this script",
#            "again will generate different downloads as there would have been",
#            "changes in the data on GBIF since its been last run."))


# Setup -------------------------------------------------------------------

# load libraries
library("rgbif")
library("taxize")
library("dplyr")
library("tidyr")
library("stringr")
library("magrittr")

# load pollinator taxa
polltax <- readLines("static_data/pollinator_taxa_all.txt")

# get taxon keys from species list
keys.poll <- taxize::get_gbifid(polltax, ask = FALSE,  messages = FALSE,
                                phylum = "Arthropoda",
                                rows = 1) %>%
  as.data.frame() %>%
  drop_na() %>%
  select(ids) %>%
  unlist() 

# count number of keys returned
n.keys.poll <- length(keys.poll)

# convert poll keys to string
keys.poll <- paste(keys.poll, collapse = ",")
 
# Select plant species ---------------------------------

# load plant traits data set
plant_traits <- read.csv("static_data/bioflor_traits.csv")

# get taxon keys from species list
keys.plant <- plant_traits %>%
  extract2("GbifKey")

# count number of keys returned
n.keys.plant <- length(keys.plant)


# break up occurrence requests, as requests have a character limit --------

# max characters in one substring
str.max <- 8000

# median length of keys + 1 for separating commas later on
key.length <- median(str_length(keys.plant) + 1) 

# number of keys in one substring
key.n <- floor(str.max/key.length)

# final length of substrings
str.length <- key.n * key.length

# initialize list for storing keys
key.list <- list()

# break up plant keys
for (i in seq(1, ceiling(n.keys.plant / key.n))) {
  
  # make selection vec for keys.plant
  sel_vec <- seq(from = ((i - 1) * key.n) + 1,
                 
                 # prevent taking more keys than we have
                 to   = min(i * key.n, length(keys.plant)))
  
  
  # take only current segment of all keys
  key.list[[i]] <- keys.plant[sel_vec]
  
}

# write poll keys into key list
key.list[[i + 1]] <- keys.poll


# generate download requests ----------------------------------------------

# initialize list for storing prepped download requests
dl.list <- list()

# specify predicates for the occ search in GBIF
basisOfRecord <- c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN", "LIVING_SPECIMEN",
                   "OBSERVATION")

country       <- "DE"

hasCoordinate <- TRUE

# construct calls for occ dl queue
for (i in seq_along(key.list)) {
  
  dl.list[[i]] <- occ_download_prep(
    pred(   "country",       country),
    pred(   "hasCoordinate", hasCoordinate),
    pred_in("basisOfRecord", basisOfRecord),
    pred_in("taxonKey",      key.list[[i]])
  )
  
}

# run download requests
occ_download_queue(.list = dl.list)

# write last keys into file
writeLines(occ_download_list(limit = length(key.list))[["results"]]$key,
          "static_data/last_keys_2.txt")
