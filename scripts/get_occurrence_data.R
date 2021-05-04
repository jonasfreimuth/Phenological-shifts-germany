
#################################################
#
#   TO BE RUN FROM WITHIN THE MAIN NOTEBOOK
#
#################################################

# make sure data folder exists
dir.check(here("data"))

# Setup: Additional Data ---------------------------------------------------

# check if bioflor trait data is available
# if not run script for obtaining it
if ( !(file.exists(here("static_data", "bioflor_traits.csv")))) {
  
  log_msg('Plant trait data not found, running download script.')
  
  source(here("scripts", "get_bioflor_traits.R"))
  
}

#load bioflor traits
bioflor_traits <- fread(here("static_data", "bioflor_traits.csv"),
                        showProgress = FALSE) %>%
  mutate(species = as.character(species)) 


# check whether climate data is available
# if not, run the script for obtaining climate data

if (!(file.exists(here("static_data", "overall_mean_temperature.csv")) &
      file.exists(here("static_data", "overall_mean_temperature.csv")))) {
  
  log_msg('Climate data not found, running download script.')
  
  source(here("scripts", "get_german_climate_data.R"))
  
}

# Download and refinement -------------------------------------------------

# check if download has already run with current gbif dataset, first
# see if all files are even present

files_to_check <- c('data/occurrences_full.csv',
                    'data/occurrences_full_refined.csv',
                    'data/download_ran.txt')

files_exist <- file.exists(files_to_check)

if(all(files_exist)) {
  
  # get download keys (uids of the datasets used, order is plants,
  # last one is polls)
  keys <- readLines('static_data/last_keys.txt')
  
  # check whether all downloads and extractions completed sucessfully
  dl_ran_txt <- readLines('data/download_ran.txt')
  
  key_in <- rep(NA, length(keys))
  
  for (i in 1:length(keys)) {
    
    key_in[i] <- any(str_detect(dl_ran_txt, keys[i]))
    
  }
  
  # now check if the file is older than the record for the gbif requests
  if (file.mtime(here("static_data", "last_keys.txt")) < 
      file.mtime(here("data", "occurrences_full.csv")) &
      all(key_in)) {
    
    log_msg('Downloads are older than occurrence file, running refinement is not',
            'necessary if not forced.')
    
    # since the file for gbif requests is older, we're up to date with the occurrences
    # and we already have our occurrence dataset
    # we don't run the download
    
    run.occ.refine <- FALSE
    
  } else {
    
    log_msg('Either downloads are not older than occurrence file or not all',
            'downloads ran, refinement will be run.')
    
    # the occurrence dataset is older
    # the occurrences are out of date
    # we run the download
    
    run.occ.refine <- TRUE
    
  } 
  
} else {
  
  log_msg('Files', paste(files_to_check[!files_exist], collapse = ', '),
          'do not exist, running occurrence refining.')
  
  # there is no file
  # we don't have downloads yet
  # run the download
  
  run.occ.refine <- TRUE
  
}

if (run.occ.refine) {
  
  # make sure directory exists
  dir.check(here("download"))
  
  # create file for storing occurrences in (overwrites previous file)
  file.create(here("data", "occurrences_full.csv"))
  
  # also create file for storing keys that have finished the process
  writeLines(text = c('File that keeps track of which downloads ran fully',
                      'and whoose occurrences have been extracted.\n',
                      'If this file is deleted, every occurrence request',
                      'will download again!',
                      '\n\n'),
             con = 'data/download_ran.txt'
  )
  
  #start for loop for each key
  for (k in keys) {
    log_msg('Running refinement for key', k, )
    # check if we already have an extracted occurrence file
    # if not then download + extract + delete zip
    if (! file.exists(here("download",  paste0("occurrence_", k, ".txt")))) {
      log_msg('Occurrence txt file for key', k, 'not found.')
      # if (TRUE) {
      # retrieve compiled dataset, if  the zip is not already in the downloads
      # folder
      if (! file.exists(paste0('download/', k, '.zip'))) {
        dl_link <- occ_download_meta(k)[["downloadLink"]]
        
        log_msg('Zipped occurrence download not on disk, downloading.',
                '\nIf download takes to long, manually download zip file from',
                dl_link, ', place in /download and run script again.')
        
        # if this keeps hanging it might be because this function appears to 
        # have a problem with large downloads (>2GB). In that case either 
        # do it like me and just download those manually and place them in the
        # downloads folder, or put in the work and find a way around it.
        # Maybe tweak the maximum string length in start_gbif_downloads
        occ_download_get(k, here("download"), overwrite = TRUE)
        
      }
      
      log_msg('Extracting occurrence txt file...')
      
      # extract occurrence.txt
      unzip(here("download", paste0(k, ".zip")),
            file = "occurrence.txt", exdir = here("download"), overwrite = TRUE,
            # if an error occurs, try commenting out the line below
            # it seems the internal unzip can't deal with too large files
            # i hope this option works across platforms
            unzip = getOption('unzip')
      )
      
      # give the extracted file the name of its key
      file.rename(from = here("download", "occurrence.txt"),
                  to = here("download",  paste0("occurrence_", k, ".txt")))
      
      # remove the zip file
      file.remove(here("download", paste0(k, ".zip")))
      
    }
    
    log_msg('Extracting occurrence records...')
    
    #read extracted occurrence.txt
    exp <- fread(here("download", paste0("occurrence_", k, ".txt")),
                 quote = "", showProgress = FALSE,
                 select = c("kingdom", "phylum", "order", "family", "genus",
                            "species", "institutionCode", "collectionCode",
                            "datasetName","decimalLatitude", "decimalLongitude",
                            "year", "month", "day", "eventDate", "basisOfRecord",
                            "hasGeospatialIssues", "issue")
    ) 
    
    #exclude records without days
    exp <- filter(exp, day != "")
    
    #reformat the date to POSIXct and calculate DOY
    exp <- mutate(exp, date = as.Date(substr(eventDate, 1, 10))) %>%
      select(-eventDate) %>%
      mutate(doy = as.integer(strftime(date, "%j"))) %>%
      mutate(decade = floor(year / 10) * 10)
    
    
    #add group label
    if (exp$kingdom[1] == "Plantae") {
      
      exp$id.grp <- as.character("Plants")
      
      #also make sure only plants with traits are used
      exp <- filter(exp, species %in% bioflor_traits$species)
      
    } else {
      
      exp$id.grp <- exp$order
      
    }
    
    log_msg('Appending occurrences to full csv...')
    
    
    #Save data as csv
    fwrite(exp, here("data", "occurrences_full.csv"),
           append = TRUE)
    
    # save the col names to add again later
    occ.names <- names(exp)
    
    # if not disabled, also remove occurrence txt file again
    if (delete.occ.download) {
      
      file.remove(here("download", paste0("occurrence_", k, ".txt")))
      
    }
    
    # save the key and time of the last occurrence set
    # WARINING: not timezone aware 
    dl_ran <- file('data/download_ran.txt', open = 'at')
    writeLines(paste(k, Sys.time(), sep = '\t'), dl_ran)
    close(dl_ran)
    
    log_msg('Done with this dataset.')
  }
  
  # remove unneeded objects
  rm("exp")
  
  #load  full data 
  dat.occ <- fread(here("data", "occurrences_full.csv"))
  
  #add row names
  names(dat.occ) <- occ.names
  
  #save dataset to add names
  fwrite(dat.occ, here("data", "occurrences_full_refined.csv"),
         showProgress = FALSE)
  
  #remove dat.occ for memory reasons
  rm(dat.occ)
  
}

# Prune data --------------------------------------------------------------

# check if th next part has already run (e.g. the output file exists)
# also check if the next part needs to be run bc the output is outdated
# (e.g. the output file from the previous part is mor recent than this part's
# output) 

if (file.exists(here("data", "occurrences_full_pruned.csv"))) {
  
  if(file.mtime(here("data", "occurrences_full_refined.csv")) >
     file.mtime(here("data", "occurrences_full_pruned.csv")) ) {
    
    log_msg('Refined occurrences are not older than pruned occurrences, running
        pruning.')
    
    run.pruning <- TRUE
    
  } else {
    
    log_msg('Refined occurrences are older than pruned occurrences,',
            'skipping pruning if not forced.')
    
    run.pruning <- FALSE
    
  }
  
} else {
  
  log_msg('File of pruned occurrences not found, running pruning.')
  
  run.pruning <- TRUE
  
}

# run pruning if necessary or forced
if (run.pruning |
    force.pruning) {
  # if there is no occurrence data loaded yet (e.g. the script is run from here on)
  # load occurrence data
  
  log_msg('Pruning...')
  
  if (!exists("dat.occ.ref")) {
    
    #load full dataset
    dat.occ <- fread(here("data", "occurrences_full_refined.csv"),
                     showProgress = FALSE)
    
  }
  
  # check whether finding indices of records outside germany is necessary
  if (file.exists("data/indices_in_region.csv")) { 
    if(file.mtime("data/indices_in_region.csv") > 
       file.mtime("data/occurrences_full_refined.csv")) {
      
      # everything in order, don't run that part
      run.idcs.germany <- FALSE
      
    } else {
      
      # indices possibly out of date, redo index getting
      run.idcs.germany <- TRUE
      
    }
    
  } else {
    
    # no index data available, run index getting
    run.idcs.germany <- TRUE
    
  }
  
  
  if (run.idcs.germany) {
    
    
    log_msg("Identifying records outside Germany...")
    
    ## Get indices of records not within the german borders
    
    germany <- germany <- raster::getData("GADM", country = "DEU", level = 1, 
                                          path = "data")
    
    # extract only polygons from Spatial Polygons data frame
    germany_pol <- SpatialPolygons(germany@polygons,
                                   proj4string = germany@proj4string)
    rm(germany)
    
    # extract spatial points from data
    # WARNING:  this assumes that the GBIF data shares the projection of the 
    #           polygonal data used
    dat.occ.sp <- SpatialPoints(coords = as.matrix(dat.occ %>% 
                                                     drop_na(decimalLatitude) %>% 
                                                     select(decimalLongitude, 
                                                            decimalLatitude)),
                                proj4string = germany_pol@proj4string)
    
    # get logical vector of whether a given record is within one of the polygons
    in_region <- !is.na(over(dat.occ.sp, germany_pol)) 
    
    log_msg("Done.")
    
    # save vector as it is time intensive to calculate
    fwrite(list(in_region), "data/indices_in_region.csv")
    
  } else {
    
    in_region <- fread("data/indices_in_region.csv")
    
  }
  
  
  ## Cleanup 
  
  log_msg("Cleanup...")
  
  dat.occ <- dat.occ %>% 
    
    # exclude non - georeferenced and records outside germany
    drop_na(decimalLatitude) %>% 
    filter(in_region) %>%
    
    #remove records without determined species
    filter(species != "") %>%
    
    #exclude first and last days
    filter(doy != 1, doy != 366, doy != 365) %>%
    
    #exclude collections
    filter(
      !(
        # exclude spikes from colection GEO (GEO - Tag der Artenvielfalt),
        # a german bioblitz event
        (institutionCode == "GEO" & doy %in% c(163, 164, 166)) |
          
          # the data set of higher plants in Mecklenburg - Vorpommern from
          # the Ernst-Moritz-Arndt-University Greifswald
          (institutionCode == "EMAU" & 
             collectionCode == "Floristic Databases MV - Higher Plants" &
             doy == 163) |
          
          # records without institution and collection on those days
          (institutionCode == "" & collectionCode == ""  &
             doy %in% c(73, 95, 121)) |
          
          
          (id.grp == "Coleoptera" &
             institutionCode == "" & collectionCode == ""  &
             doy == 122) |
          
          # suspicious naturgucker data from 2018 & 2019 concerning only 
          # Diptera and Hymenoptera on those days. The cause could not be 
          # determined by a rudimentary analysis
          (id.grp %in% c("Hymenoptera", "Diptera") &
             institutionCode == "naturgucker" &
             year %in% c(2018, 2019) &
             doy %in% c(216, 160, 224, 153, 161, 217, 154, 223, 215, 152, 156,
                        158, 220, 159, 155, 218, 157, 222, 221, 219, 214, 151))
        
      )
    ) %>%
    
    #only include records from year.start on
    filter(year >= year.start & year <= year.stop)
  
  rm(in_region)
  
  log_msg("Done.")
  
  log_msg("Saving additional info...")
  
  ## summarize data set
  fwrite(sum_df(dat.occ %>%
                  select(order,
                         id.grp,
                         species,
                         year,
                         doy), id.grp),
         "data/occurrences_full_pruned_summary.csv")
  
  ## Check how many issues the data has 
  
  #generate vector of every single occurrence of an issue
  issues <- as.character(dat.occ$issue) %>%
    str_split(";", simplify = TRUE )%>%
    str_c(sep = ", ") %>%
    #exclude all empty fields
    str_subset("^$", negate = TRUE)
  
  #count # of issues
  issuecount <- count(data.frame(issue = issues), issue)
  
  # save issues
  fwrite(issuecount, here("data", "pruned_occ_geospatial_issue_count.csv"))
  
  #remove issue object (uses quite a lot of space)
  rm(issues)
  
  ## Count the number of records in each institution
  dat.occ %>%
    count(institutionCode) %>%
    fwrite(here("data", "pruned_occ_institution_count.csv"))
  
  
  ## Add climate data and save
  
  #join overall climate data and plant trait data to raw observational data
  dat.occ <- left_join(dat.occ,
                       read.csv(here("static_data", "overall_mean_temperature.csv")),
                       by = c("year")) %>% 
    left_join(bioflor_traits, by = "species")
  
  #save full dataset 
  fwrite(dat.occ,
         here("data", "occurrences_full_pruned.csv"),
         showProgress = FALSE)
  
  
  # also save some metrics for the full data set
  data.frame(
    nRow = nrow(dat.occ),
    nPlants = as.numeric(count(dat.occ, kingdom)[2,2]),
    nInsects = as.numeric(count(dat.occ, kingdom)[1,2]),
    # should always be 1 now, kept for compatability reasons
    fracGeoref = sum(!is.na(dat.occ$decimalLatitude))/nrow(dat.occ)
  ) %>%
    fwrite(here("data", "occurrence_full_pruned_meta.csv"))
  
  #remove dat.occ again for memory reasons
  rm(dat.occ)
  
  log_msg('Done pruning.')
  
}


# Calculate yearly species means ------------------------------------------

select_vars <- c("kingdom",
                 "phylum",
                 "order",
                 "family",
                 "genus",
                 "species",
                 "id.grp",
                 "decade",
                 "year",
                 "month",
                 "doy")

log_msg('Calculating yearly average DOYs...')

#calculate species means
dat.occ.mean <- fread(here("data", "occurrences_full_pruned.csv"),
                      showProgress = FALSE,
                      select = select_vars) %>%
  
  group_by(kingdom, phylum, order, family, genus, id.grp, decade) %>%
  group_by(species, .add = TRUE) %>%
  group_by(year, .add = TRUE) %>%
  
  # filter out years with insufficient number of records
  filter(n() >= thr.spec) %>%
  
  summarise(
    mean.doy = mean(doy),
    sd.doy = sd(doy),
    ci.min.doy = ci.min(doy),
    ci.max.doy = ci.max(doy),
    min.doy = min(doy),
    max.doy = max(doy),
    duration = max(doy) - min(doy),
    median.doy = median(doy),
    quant25 = quantile(doy, probs = 0.25),
    quant75 = quantile(doy, probs = 0.75),
    n.rec = length(doy)
  ) %>%
  ungroup() %>%
  
  # filter out species that don't span enough years
  group_by(species) %>%
  filter(length(year) >= round(thr.perc * (max(year) - min(year)))) %>%
  ungroup() %>% 
  
  #join trait data
  left_join(bioflor_traits, by = "species") %>%
  left_join(fread(here("static_data", "overall_mean_temperature.csv")),
            by = c("year"))

#save species yearly mean doy data
fwrite(dat.occ.mean,
       here("data", "species_yearly_mean_doy_raw_based.csv"),
       showProgress = FALSE,
       na = NA
)

log_msg('Calculating summary data for raw based yearly data...')

# summarise dataset
fwrite(sum_df(dat.occ.mean %>% 
                select(id.grp,
                       species,
                       year,
                       mean.doy,
                       duration,
                       n.rec
                ), id.grp),
       "data/species_yearly_mean_doy_raw_based_summary.csv")

rm(dat.occ.mean)

log_msg('Done')

# Calculate decadal means -------------------------------------------------


tax_groups <- c("kingdom",
                "phylum",
                "order",
                "family",
                "genus",
                "species",
                "id.grp")

species_vars <- c("AccName", 
                  "GbifKey", 
                  "OrigName", 
                  "bioflor_id", 
                  "LifeForm", 
                  "LifeSpan", 
                  "FlStart", 
                  "FlEnd", 
                  "FlDur", 
                  "ReprType", 
                  "Dicliny", 
                  "Dichogamy", 
                  "SelfComp", 
                  "PollVec", 
                  "PollVecGrp", 
                  "BreedSys", 
                  "FlowClass", 
                  "Habitat", 
                  "PollDep")

decade_vars <- c('species', 'decade')

log_msg('Calculating decadal DOYs from raw occurrences...')

thr.dec <- length(seq(dec.start, dec.stop, 10))

#load data again and delete years before decadal cutoff 
dat.occ.dec.raw <- fread(here("data", "occurrences_full_pruned.csv"),
                         showProgress = FALSE,
                         select = select_vars) %>%
  #delete years before cutoff
  filter(decade >= dec.start & year <= dec.stop) %>%
  select(-year) %>%
  #drop unnecessary columns
  group_by(kingdom, phylum, order, family, genus, id.grp) %>%
  group_by(species, .add = TRUE) %>%
  group_by(decade, .add = TRUE) %>%
  #exclude species with less than x records per decade
  filter(n() >= thr.dec.rec) %>%
  summarise(
    mean.doy = mean(doy),
    sd.doy = sd(doy),
    ci.min.doy = ci.min(doy),
    ci.max.doy = ci.max(doy),
    min.doy = min(doy),
    max.doy = max(doy),
    duration = max(doy) - min(doy),
    median.doy = median(doy),
    quant25 = quantile(doy, probs = 0.25),
    quant75 = quantile(doy, probs = 0.75),
    n.rec = length(doy)
  ) %>%
  ungroup() %>%
  group_by(species, .add = TRUE) %>%
  #exclude species without records in every decade
  filter(length(unique(decade)) >= thr.dec) %>%
  ungroup() %>% 
  #join trait data again (lost in averaging)
  left_join(bioflor_traits, by = "species") %>%
  # join climate data
  left_join(fread(here("static_data",
                       "decadal_mean_temperature.csv")),
            by = "decade")

fwrite(dat.occ.dec.raw,
       here("data", "species_decadal_mean_doy_raw_based.csv"),
       showProgress = FALSE)

# summarise dataset
fwrite(sum_df(dat.occ.dec.raw %>% 
                select(id.grp,
                       species,
                       decade,
                       mean.doy,
                       duration
                ), id.grp),
       "data/species_decadal_mean_doy_raw_based_summary.csv")

rm(dat.occ.dec.raw)

log_msg('Done')


log_msg('Calculating decadal mean doys from yearly mean doys...')

dat.occ.dec.yearly <- fread('data/species_yearly_mean_doy_raw_based.csv') %>% 
  
  # use only data within limits
  filter(decade >= dec.year.start, decade <= dec.year.stop) %>% 
  # use only decades where a species has enough years covered
  group_by(species) %>% 
  filter(n_distinct(year) >= thr.dec.year) %>%
  ungroup %>% 
  # drop year column
  select(-year) %>% 
  
  # Summarizing 
  group_by(across(matches(tax_groups) | matches(species_vars) | decade)) %>% 
  summarise(
    mean.doy.dec = mean(mean.doy),
    sd.doy = sd(mean.doy),
    ci.min.doy = ci.min(mean.doy),
    ci.max.doy = ci.max(mean.doy),
    min.mean.doy = min(mean.doy),
    max.mean.doy = max(mean.doy),
    median.doy = median(mean.doy),
    duration.dec = max(max.doy) - min(min.doy),
    n.year = length(mean.doy),
    n.rec.dec =  sum(n.rec)
  ) %>% 
  ungroup() %>% 
  
  # Pruning
  group_by(species) %>% 
  # remove species without percentage of full decadal coverage
  filter(n() >= length(seq(dec.year.start, dec.year.stop, 10))) %>%
  # join climate data
  left_join(fread(here("static_data",
                       "decadal_mean_temperature.csv")),
            by = "decade")

fwrite(dat.occ.dec.yearly,
       here("data", "species_decadal_mean_doy_year_based.csv"),
       showProgress = FALSE)

# summarise dataset
fwrite(sum_df(dat.occ.dec.yearly %>% 
                select(id.grp,
                       species,
                       decade,
                       mean.doy.dec,
                       n.year,
                       n.rec.dec
                ), id.grp),
       "data/species_decadal_mean_doy_year_based_summary.csv")

rm(dat.occ.dec.yearly)

log_msg('Done.')

log_msg('Done with occurrence getting script.')

