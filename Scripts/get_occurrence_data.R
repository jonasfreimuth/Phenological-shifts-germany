
#################################################
#
#   TO BE RUN FROM WITHIN THE MAIN NOTEBOOK
#
#################################################

# make sure data folder exists
dir.check(here("data"))

# Setup: Aditional Data ---------------------------------------------------

# check if bioflor trait data is available
# if not run script for obtaining it
if ( !(file.exists(here("static_data", "bioflor_traits.csv")))) {
  
  source(here("scripts", "get_bioflor_traits.R"))
  
}

#load bioflor traits
bioflor_traits <- fread(here("static_data", "bioflor_traits.csv"), showProgress = FALSE) %>%
  mutate(species = as.character(species)) 


# check whether climate data is available
# if not, run the script for obtaining climate data

if (!(file.exists(here("static_data", "overall_mean_temperature.csv")) &
      file.exists(here("static_data", "overall_mean_temperature.csv")))) {
  
  source(here("scripts", "get_german_climate_data.R"))
  
}

# Download and refinement -------------------------------------------------


# check if download has already run with current gbif dataset
if(file.exists(here("data", "occurrences_full.csv")) &
   file.exists(here("data", "occurrences_full_refined.csv"))) {
  
  # now check if the file is older than the record for the gbif requests
  if (file.mtime(here("static_data", "last_keys.txt")) < 
      file.mtime(here("data", "occurrences_full.csv"))) {
    
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
  
  #set keys (uids of my datasets used, order is pollinators, plants)
  # keys <- occ_download_list(limit = 13)[["results"]][["key"]]
  keys <- read.delim(here("static_data", "last_keys.txt"), sep = " ", stringsAsFactors = FALSE)[,1]
  
  # create file for storing occurrences in (overwrites previous file)
  file.create(here("data", "occurrences_full.csv"))
  
  #start for loop for each key
  for (k in keys) {
    
    # check if we already have an extracted occurrence file
    # if not then download + extract + delete zip
    if (! file.exists(here("Download",  paste0("occurrence_", k, ".txt")))) {
      
      # retrieve compiled dataset
      occ_download_get(k, here("Download"), overwrite = TRUE)
      
      # extract occurence.txt
      unzip(here("Download", paste0(k, ".zip")),
            file = "occurrence.txt", exdir = here("Download"), overwrite = TRUE)
      
      # give the extracted file the name of its key
      file.rename(from = here("Download", "occurrence.txt"),
                  to = here("Download",  paste0("occurrence_", k, ".txt")))
      
      # remove the zip file
      file.remove(here("download", paste0(k, ".zip")))
      
      # create/overwrite the file for download status
      sink(here("data", "download_ran.txt"))
      
      cat("The last key downloaded was:"); k
      
      cat("\nOccurrences were downloaded last from gbif at: ")
      
      date()
      
      cat("\n\nPlease don't delete this file.\nYour script will take needlessly long...")
      
      sink()
      
    }
    
    #read extracted occurence.txt
    exp <- fread(here("Download", paste0("occurrence_", k, ".txt")),
                 quote = "", showProgress = FALSE,
                 select = c("kingdom", "phylum", "order", "family", "genus",
                            "species", "institutionCode", "collectionCode",
                            "datasetName","decimalLatitude", "decimalLongitude",
                            "year", "month", "day", "eventDate", "hasGeospatialIssues", "issue")
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
    if (exp$kingdom[1] == "Plantae") {
      
      #save to csv
      fwrite(exp, here("Data", "occurrences_full.csv"),
             append = TRUE)
      
      
    } else  if (exp$kingdom[1] == "Animalia") {
      
      #save to csv
      fwrite(exp, here("Data", "occurrences_full.csv"),
             append = TRUE)
      
    }
    
    # save the col names to add again later
    occ.names <- names(exp)
    
    # if not disabled, also remove occurrence txt file again
    if (delete.occ.download) {
      
      file.remove(here("download", paste0("occurrence_", k, ".txt")))
      
    }
    
  }
  
  # remove unneeded objects
  rm("exp")
  
  #load  full data 
  dat.occ <- fread(here("data", "occurrences_full.csv"))
  
  #add row names
  names(dat.occ) <- occ.names
  
  #save dataset to add names
  fwrite(dat.occ, here("data", "occurrences_full_refined.csv"), showProgress = FALSE)
  
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
    dat.occ <- fread(here("Data", "occurrences_full_refined.csv"), showProgress = FALSE)
    
  }
  
  # make a vactor of institutionCodes to exclude bc of distribution
  # identified via source(here("scripts", "plot_occurrence_distribution.R"))
  # plots are located in plots/additional. To view the script in RStudio use
  # file.edit(here("scripts", "plot_occurrence_distribution.R"))
  excl.inst <- c(
    "Administration de la gestion de lâ€™eau (AGE)",
    "Administration de la nature et des forÃªts (ANF)",
    "MinistÃ¨re de l'Environnement, du Climat et du DÃ©veloppement durable (MECDD)",
    "MusÃ©e national d'histoire naturelle Luxembourg (MnhnL)",
    "Naturpark Ã–ewersauer",
    "SICONA - Naturschutzsyndikat",
    "SPW-DEMNA",
    "STOWA"
  )
  
  
  
  ## Cleanup
  
  #remove records without determined species
  dat.occ <- filter(dat.occ, species != "") %>%
    
    #exclude the current year from further analysis
    filter(year != as.integer(substr(
      Sys.Date(), start = 1, stop = 4
    ))) %>%
    
    #exclude first and last days
    filter(doy != 1, doy != 366, doy != 365) %>%
    
    #exclude weird collections
    filter(
      # for having to many records on just one day
      institutionCode != "GEO",
      # for having mainly records from not actually within germany
      !(institutionCode %in% excl.inst)
    ) %>%
    
    #only include records from cutoff.year on
    filter(year >= cutoff.year)
  
  
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
  
  #join overall climate data  and plant trait data to raw observational data
  dat.occ <- left_join(dat.occ,
                       read.csv(here("static_data", "overall_mean_temperature.csv")),
                       by = c("year")) %>% 
    left_join(bioflor_traits, by = "species")
  
  #save full dataset 
  fwrite(dat.occ,
         here("Data", "occurrences_full_pruned.csv"),
         showProgress = FALSE)
  
  
  # also save some metrics for the full data set
  data.frame(
    nRow = nrow(dat.occ),
    nPlants = as.numeric(count(dat.occ, kingdom)[2,2]),
    nInsects = as.numeric(count(dat.occ, kingdom)[1,2])
  ) %>%
    fwrite(here("Data", "occurrence_full_meta.csv"))
  
  #remove dat.occ again for memory reasons
  rm(dat.occ)
  
}


# Calculate yearly species means ------------------------------------------


#calculate species means
dat.occ.mean <- fread(here("Data", "occurrences_full_pruned.csv"),
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
    here("Data", "occurrences_species_yearly_mean_doy_pruned.csv"),
    showProgress = FALSE,
    na = NA
  )



# Calculate decadal means -------------------------------------------------

#load data again and dlete years before decadal cutoff 
dat.occ.dec <- fread(here("Data", "occurrences_full_pruned.csv"),
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
  filter(year >= cutoff.dec) 

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
  #join trait data again (lost in avaraging)
  left_join(bioflor_traits, by = "species") %>%
  left_join(fread(here("static_data",
                       "decadal_mean_temperature.csv")),
            by = "decade") %>% 
  fwrite(here("data", "occurrences_species_decadal_mean_doy_pruned.csv"), showProgress = FALSE)

rm(dat.occ.dec)

# Plots for data quality assessment ---------------------------------------

if (any(c("data.quality.assessment") %in% opts)) {
  
  dir.check(here("plots"))
  
  dat.occ <- fread(here("Data", "occurrences_full_pruned.csv"), showProgress = FALSE)
  
  png(here("Plots", "raw_plant_distribution.png"), height = 5000, width = 8000)
  
  print(
    ggplot(filter(dat.occ, kingdom == "Plantae")) +
      geom_point(aes(y_mean_temp, doy, col = floor(year / 10) * 10),
                 size = 10) +
      # facet_wrap(~institutionCode) +
      theme(axis.title = element_text(size = 160),
            axis.text = element_text(size = 120),
            axis.ticks = element_line(colour = "gray31", size = 3),
            axis.ticks.length.y.left = unit(8, "bigpts"),
            axis.ticks.length.x.bottom = unit(8, "bigpts"),
            axis.line = element_line(colour = "gray31", size = 3),
            legend.position = "bottom",
            legend.text = element_text(size = 160),
            legend.title = element_text(size = 160),
            panel.background = element_blank(),
            panel.grid.major = element_blank())
  )
  
  dev.off()
  
  png(here("Plots", "raw_insect_distribution.png"), height = 5000, width = 8000)
  
  print(
    ggplot(filter(dat.occ, kingdom == "Animalia")) +
      geom_point(aes(y_mean_temp, doy, col = floor(year / 10) * 10),
                 size = 10) +
      # facet_wrap(~institutionCode) +
      theme(axis.title = element_text(size = 160),
            axis.text = element_text(size = 120),
            axis.ticks = element_line(colour = "gray31", size = 3),
            axis.ticks.length.y.left = unit(8, "bigpts"),
            axis.ticks.length.x.bottom = unit(8, "bigpts"),
            axis.line = element_line(colour = "gray31", size = 3),
            legend.position = "bottom",
            legend.text = element_text(size = 160),
            legend.title = element_text(size = 160),
            panel.background = element_blank(),
            panel.grid.major = element_blank())
  )
  
  dev.off()
  
  rm(dat.occ)
}