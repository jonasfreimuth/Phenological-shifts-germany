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
  # %>%
  # unlist() %>%
  # paste(collapse = ",")

# count number of keys returned
n.keys.poll <- nrow(keys.poll)

# get plant taxon keys from species list
keys.plant <- fread('static_data/bioflor_traits.csv', select = c('GbifKey'))

# count number of keys returned
n.keys.plant <- nrow(keys.plant)

# breaking gbif requests up -----------------------------------------------

# as we are dealing with a lot of plant taxon keys we are going over the gbif
# APIs character limit, so we need to break the request up into smaller bits

# max characters in one substring
str.max <- 3000

# length of a key
# WARNING: This does not check if all keys have the same length, it is just
# assumed as these keys all come from plant species
key.length <- str_length(keys.plant$GbifKey[1])

# number of keys in one substring, adding one to the key length for the commas
# and adding one to the maximum string size to account for the last key, which
# won't have a comma
key.n <- floor((str.max + 1)/(key.length + 1))

# final length of substrings
str.length <- key.n * key.length

# number of substrings we'll need
str.n <- ceiling(n.keys.plant/key.n)

# initialize list for holding keys
keys <- list()

# break up plant keys, loop starts from 0
for (i in seq(0, str.n - 1)) {
  
  #take only current segment of all keys
  keys[i] <- paste0(
    unlist(keys.plant$GbifKey[((i * key.n) + 1):((i + 1) * key.n)]
    ),
    collapse = ",") 

}

#write poll keys into an object
assign(paste("keys", i+2, sep = ""), keys.poll)

#specify basis of record:
record.base <- ("HUMAN_OBSERVATION, PRESERVED_SPECIMEN, LIVING_SPECIMEN, OBSERVATION")

#initialize counting var
j <- 1 

#construct calls for occ dl queue
for (i in ls(pattern = "keys[[:alnum:]]+")) {
  
  assign(
    #use same number as in keys
    paste("dprep", str_extract(i, "[:digit:]+"), sep = ""),
    call(
      "occ_download",
      paste("basisOfRecord = ", record.base),
      "country = DE",
      paste("taxonKey = ", eval(as.name(
        ls(pattern = "keys[[:alnum:]]+")[j]
      )))
    )
  )
  
  j <- j + 1
  
}

#construct queue call
dpreps <- lapply(lapply(ls(pattern = "dprep[[:digit:]]+"), as.name), eval)
do.call(occ_download_queue, dpreps)

#write last keys into file
write.csv(occ_download_list(limit = ceiling(n.keys.plant / key.n) + 1)[["results"]]$key,
          here("Data", "last_keys.txt"), row.names = FALSE)

#beep for being done
beep()
