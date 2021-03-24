stop(paste(
  c(
    "This script was used to compile the gbif data sets used in the",
    "analysis. Running it requires setting your GBIF credentials in some way",
    "(usually in .Renviron). See help('Startup') or the rgbif documentation, ",
    "especially rgbif::occ_download. Running this script again will generate ",
    "different downloads as there would have been changes in the  data on GBIF ",
    "since it has been last run."
  )
))


# Setup -------------------------------------------------------------------

#load libraries
library('data.table')
library("beepr")
library("rgbif")
library("taxize")
library("tidyverse")


# Get GBIF taxon keys -----------------------------------------------------

# read file with pollinator taxa of interest, file was previously compiled
polltax <- read_lines('static_data/pollinator_taxa_all.txt')

# get taxon keys for pollinator taxa
# WARNING: Taxa without keys will be dropped without a warning
keys.poll <- taxize::get_gbifid(polltax, ask = FALSE,  messages = FALSE,
                                phylum = "Arthropoda",
                                rows = 1) %>%
  as.data.frame() %>%
  drop_na() %>%
  select(ids)

# count number of keys returned
n.keys.poll <- nrow(keys.poll)

# get plant taxon keys from species list
keys.plant <- fread('static_data/bioflor_traits.csv', select = c('GbifKey'))

# count number of keys returned
n.keys.plant <- nrow(keys.plant)

# breaking gbif requests up -----------------------------------------------

# as we are dealing with a lot of plant taxon keys we are going over the gbif
# APIs character limit, so we need to break the request up into smaller bits

# ~max characters in one substring, can be +- a key legnth
str.max <- 10000

avg_key_length <- mean(str_length(keys.plant$GbifKey))

# number of keys in one substring, adding one to the mean key length for the
# commas and adding one to the maximum string size to account for the last key,
# which won't have a comma
key.n <- floor((str.max + 1)/(avg_key_length + 1))

# number of substrings we'll need
str.n <- ceiling(n.keys.plant/key.n)

# initialize list for holding keys
keys <- list()

# break up plant keys, loop starts from 0
for (i in seq(0, str.n - 1)) {
  
  #take only current segment of all keys
  keys.i <- keys.plant$GbifKey[((i * key.n) + 1):((i + 1) * key.n)]
  keys[[i + 1]] <- keys.i[!is.na(keys.i)]
  
}

#write poll keys into an object
keys[[i + 2]] <- keys.poll$ids
# assign(paste("keys", i+2, sep = ""), keys.poll)

#specify basis of record:
record.base <- c("HUMAN_OBSERVATION",
                 "PRESERVED_SPECIMEN",
                 "LIVING_SPECIMEN",
                 "OBSERVATION")

# initialize list for occ downloads
dpreps <- list()

#construct calls for occ dl queue
for (i in 1:length(keys)) {
  
  # dpreps[[i]] <- call("occ_download",
  #                     "pred('country', 'DE')",
  #                     "pred_in('basisOfRecord', record.base)",
  #                     "pred_in('taxonKey', keys[i])")
  dpreps[[i]] <- occ_download_prep(
    pred('country', 'DE'),
    pred_in('basisOfRecord', record.base),
    pred_in('taxonKey', keys[[i]]))
}

# kickoff queue, i.e. do the actual downloads
# do.call(occ_download_queue, dpreps)
res <- occ_download_queue(.list = dpreps)

#write last keys into file
writeLines(text = occ_download_list(limit = length(keys))[["results"]]$key,
           con = './static_data/last_keys.txt')

#beep for being done
beep()
