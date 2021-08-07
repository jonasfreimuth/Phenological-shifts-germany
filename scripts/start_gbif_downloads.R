stop("This script was used to compile the gbif data sets used in the analysis.
      Running it requires setting your GBIF credentials in some way (usually in .Renviron).
      See help('Startup') or the rgbif documentation, especially rgbif::occ_download.
      Running this script again will generate different downloads as there would have been changes in the 
      data on GBIF since its been last run.")


# Setup -------------------------------------------------------------------

#load libraries
library("here")
library("beepr")
library("rgbif")
library("taxize")
library("dplyr")
library("tidyr")
library("stringr")



#load pollinator taxa
polltax <- read.delim(here("static_data", "pollinator_taxa_all.txt"), header = FALSE) %>%
  transmute(taxon = as.character(V1))

#get taxon keys from species list
keys.poll <- taxize::get_gbifid(polltax$taxon, ask = FALSE,  messages = FALSE, phylum = "Arthropoda",
                                rows = 1) %>%
  as.data.frame() %>%
  drop_na() %>%
  select(ids) %>%
  unlist() %>%
  paste(collapse = ",")

#count number of keys returned
n.keys.poll <- str_count(keys.poll, ",") + 1
 
# Select plant species ---------------------------------

#load plant traits data set
plant_traits <- read.csv(here("data", "bioflor_traits.csv")) %>%
  mutate(species = as.character(species),
         SciName = as.character(AccName)) 

#get taxon keys from species list
keys.plant <- plant_traits %>%
  select(GbifKey) %>%
  drop_na(GbifKey)

#count number of keys returned
n.keys.plant <- nrow(keys.plant)

# break up occurrence requests, as requests have a character limit --------

#max characters in one substring
str.max <- 2500

#median length of keys
key.length <- median(str_length(keys.plant$GbifKey))

#number of keys in one substring
key.n <- floor(str.max/key.length)

#final length of substrings
str.length <- key.n * key.length

#break up plant keys
for (i in seq(0, ceiling(n.keys.plant/key.n)-1)) {
  
  #take only current segment of all keys
  keys.i <- paste0(unlist(keys.plant$GbifKey[((i*key.n)+1):((i+1)*key.n)]), collapse = ",") %>%
    str_extract("[[:blank:][:punct:][:digit:]]+") %>%
    gsub(x = ., pattern = "\\, $", replacement = "")
  
  
  #write keys into an object
  assign(paste("keys",
               #pad leading zeroes
               formatC(
                 i + 1,
                 #determine the number of digits will be necessary
                 width = str_length(ceiling(n.keys.plant / key.n)),
                 flag = 0
               ), sep = ""), keys.i)
  
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
          here("data", "last_keys.txt"), row.names = FALSE)

#beep for being done
beep()
