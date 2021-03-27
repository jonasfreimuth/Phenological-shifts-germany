
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
  
  source(here("scripts", "get_german_climate_data.R"))
  
}

# Download and refinement -------------------------------------------------

# check if download has already run with current gbif dataset, first
# see if all files are even present
if(file.exists(here("data", "occurrences_full.csv")) &
   file.exists(here("data", "occurrences_full_refined.csv")) &
   file.exists('data/download_ran.txt')) {
  
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
    
    # since the file for gbif requests is older, we're up to date with the occurrences
    # and we already have our occurrence dataset
    # we don't run the download
    
    run.occ.refine <- FALSE
    
  } else {
    
    # the occurrence dataset is older
    # the occurrences are out of date
    # we run the download
    
    run.occ.refine <- TRUE
    
  } 
  
} else {
  
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
    
    # check if we already have an extracted occurrence file
    # if not then download + extract + delete zip
    if (! file.exists(here("download",  paste0("occurrence_", k, ".txt")))) {
    # if (TRUE) {
      # retrieve compiled dataset, if  the zip is not already in the downloads
      # folder
      if (! file.exists(paste0('download/', k, '.zip'))) {
        # if this keeps hanging it might be because this function appears to 
        # have a problem with large downloads (>2GB). In that case either 
        # do it like me and just download those manually and place them in the
        # downloads folder, or put in the work and find a way around it.
        # Maybe tweak the maximum string lenth in start_gbif_downloads
        occ_download_get(k, here("download"), overwrite = TRUE)
      }
      
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
    
    run.pruning <- TRUE
    
  } else {
    
    run.pruning <- FALSE
    
  }
  
} else {
  
  run.pruning <- TRUE
  
}

# run pruning if necessary or forced
if (run.pruning |
    force.pruning) {
  # if there is no occurrence data loaded yet (e.g. the script is run from here on)
  # load occurrence data
  if (!(exists("dat.occ"))) {
    
    #load full dataset
    dat.occ <- fread(here("data", "occurrences_full_refined.csv"), showProgress = FALSE)
    
  }
  
  # load names of institutions to be excluded due to data bias or exceedingly
  # high content of suspicious occurrence records.
  # identified via source(here("scripts", "plot_occurrence_distribution.R"))
  # plots are located in plots/additional. To view the script in RStudio use
  # file.edit(here("scripts", "plot_occurrence_distribution.R"))
  excl.inst <- readLines('static_data/excluded_institutions.txt')
  
  ## Cleanup
  
  #remove records without determined species
  dat.occ <- filter(dat.occ, species != "") %>%
    
    #exclude the current year from further analysis
    filter(year != as.integer(substr(
      Sys.Date(), start = 1, stop = 4
    ))) %>%
    
    #exclude first and last days
    filter(doy != 1, doy != 366, doy != 365) %>%
    
    #exclude collections
    filter(
      !(institutionCode %in% excl.inst)
    ) %>%
    
    #only include records from year.start on
    filter(year >= year.start & year.stop)
  
  
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
    fracGeoref = sum(!is.na(dat.occ$decimalLatitude))/nrow(dat.occ)
  ) %>%
    fwrite(here("data", "occurrence_full_meta.csv"))
  
  #remove dat.occ again for memory reasons
  rm(dat.occ)
  
}


# Calculate yearly species means ------------------------------------------


#calculate species means
dat.occ.mean <- fread(here("data", "occurrences_full_pruned.csv"),
                      showProgress = FALSE,
                      select = c("kingdom",
                                 "phylum",
                                 "order",
                                 "family",
                                 "genus",
                                 "species",
                                 "id.grp",
                                 "decade",
                                 "year",
                                 "month",
                                 "doy")) %>%
  group_by(kingdom, phylum, order, family, genus, id.grp, decade) %>%
  group_by(species, .add = TRUE) %>%
  group_by(year, .add = TRUE) %>%
  # filter  out years with too few records
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
  #join trait data
  left_join(bioflor_traits, by = "species") %>%
  left_join(fread(here("static_data", "overall_mean_temperature.csv")),
            by = c("year")) %>%
  #save species yearly mean doy data
  fwrite(
    here("data", "occurrences_species_yearly_mean_doy_pruned.csv"),
    showProgress = FALSE,
    na = NA
  )

# Calculate decadal means -------------------------------------------------

#load data again and dlete years before decadal cutoff 
dat.occ.dec <- fread(here("data", "occurrences_full_pruned.csv"),
                     showProgress = FALSE,
                     select = c("kingdom",
                                "phylum",
                                "order",
                                "family",
                                "genus",
                                "species",
                                "id.grp",
                                "decade",
                                "year",
                                "month",
                                "doy")) %>%
  #delete years before cutoff
  filter(year >= dec.start & year <= dec.stop) 

#calculate number of decades in data set
thr.dec <- length(unique(dat.occ.dec$decade))

# calculate mean and stuff for decades
dat.occ.dec %>%
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
  left_join(fread(here("static_data",
                       "decadal_mean_temperature.csv")),
            by = "decade") %>% 
  fwrite(here("data", "occurrences_species_decadal_mean_doy_pruned.csv"), showProgress = FALSE)

rm(dat.occ.dec)

