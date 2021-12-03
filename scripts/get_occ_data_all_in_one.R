
#################################################
#
#   TO BE RUN FROM WITHIN THE MAIN NOTEBOOK
#
#################################################

# make sure data folder exists
dir.check(data_dir)

# set names of columns retrieved by fread from raw data
data_cols <- c("kingdom", "phylum", "order", "family", "genus",
               "species", "institutionCode", "collectionCode",
               "datasetName","decimalLatitude", "decimalLongitude",
               "year", "month", "day", "eventDate",
               "hasGeospatialIssues", "issue")

# Setup: Additional Data ---------------------------------------------------

# check if bioflor trait data is available
# if not run script for obtaining it
if ( !(file.exists("static_data/bioflor_traits.csv"))) {
  
  source("scripts/get_bioflor_traits.R")
  
}

# load bioflor traits
bioflor_traits <- fread("static_data/bioflor_traits.csv",
                        showProgress = FALSE) %>%
  mutate(species = as.character(species)) 


# Download and refinement -------------------------------------------------


# check if download has already run with current gbif dataset
if(file.exists(paste0(data_dir, "occurrences_full.csv")) &&
   file.exists(paste0(data_dir, "occurrences_full_refined.csv"))) {
  
  # now check if the file is older than the record for the gbif requests
  if (file.mtime(paste0(data_dir, "static_data", "last_keys.txt")) < 
      file.mtime(paste0(data_dir, "data", "occurrences_full.csv"))) {
    
    # since the file for gbif requests is older, we're up to date with the 
    # occurrences and we already have our occurrence dataset
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
  dir.check("download")
  
  #set keys (uids of my datasets used, order is pollinators, plants)
  # keys <- occ_download_list(limit = 13)[["results"]][["key"]]
  keys <- readLines("static_data/last_keys.txt")
  
  # create file for storing occurrences in (overwrites previous file)
  file.create(paste0(data_dir, "occurrences_full.csv"))
  
  # create file for keeping track which occurrence finished processing when
  sink("download/download_ran.txt")
  
  cat("# Do not delete this file.\n")
  cat("# Re-executing the occurrence getting script will take ")
  cat("needlessly long...\n")
  cat("key\tfinish_time\n")
  
  sink()
  
  # check if k exists and if so trim keys down to have k as its firs entry
  # this is a rudimentary way to be able to just restart the script and
  # have the downloads continue
  if (exists("k")) {
    k_ind <- which(keys == k)
    keys <- keys[k_ind:length(keys)]
  }
  
  # start for loop for each key
  for (k in keys) {
    
    # check if we already have an extracted occurrence file
    # if not then download + extract + delete zip
    if (! file.exists(paste(sep = "/", "download",
                            paste0("occurrence_", k, ".txt")))) {
      
      # if the corresponding raw zip file is not already present
      if (! file.exists(paste0("download/", k, ".zip"))){
        # retrieve compiled dataset
        occ_download_get(k, "download",
                         overwrite = TRUE)
      }
      
      # extract occurence.txt
      unzip(paste(sep = "/", "download", paste0(k, ".zip")),
            file = "occurrence.txt",
            exdir = paste(sep = "/", "download"),
            overwrite = TRUE,
            unzip = "unzip")
      
      # give the extracted file the name of its key
      file.rename(from = "download/occurrence.txt",
                  to = paste(sep = "/", "download",  paste0("occurrence_",
                                                            k, ".txt")))
      
      # check if its necessary to delete any files
      if (any(c("all", "zip") %in% delete.occ.download)) {
        # remove the zip file
        file.remove(paste(sep = "/", "download", paste0(k, ".zip")))
      }
      
      # create/overwrite the file for download status
      sink("download/download_ran.txt", append = TRUE)
      
      cat(format(Sys.time(), "%Y%m%d_%H%M%S"), "\t", k, "\n")
      
      sink()
      
    }
    
    # read extracted occurence.txt, do stuff with it and save it
    fread(paste(sep = "/", "download", paste0("occurrence_", k, ".txt")),
          quote = "", showProgress = FALSE,
          select = data_cols) %>% 
      
      
      # exclude records without days
      filter(day != "") %>% 
      
      # reformat the date to POSIXct and calculate DOY
      mutate(date = as.Date(substr(eventDate, 1, 10))) %>%
      select(-eventDate) %>%
      mutate(doy = as.integer(strftime(date, "%j"))) %>%
      mutate(decade = floor(year / 10) * 10) %>% 
      
      # add group label
      mutate(id.grp = if_else(kingdom == "Plantae",
                              "Plants",
                              order)) %>% 
      
      # filter out plants without trait data
      filter(!(id.grp == "Plants" & !(species %in% bioflor_traits$species))) %>% 
      
      # save data as csv
      fwrite(paste0(data_dir, "occurrences_full.csv"),
             append = TRUE)
    
    # if not disabled, also remove occurrence txt file again
    if (any(c("all", "txt") %in% delete.occ.download)) {
      
      file.remove(paste(sep = "/", "download",
                        paste0("occurrence_", k, ".txt")))
      
    }
    
  }
  
  # define renaming vec
  rename_vec <- c(data_cols[data_cols != "eventDate"],
                  'date', 'doy', 'decade', 'id.grp')
  
  # load  full data 
  dat.occ <- fread(paste0(data_dir, "occurrences_full.csv"),
                   showProgress = FALSE) %>% 
    
    # rename cols 
    set_names(rename_vec) %>% 
    
    # save dataset to add names
    fwrite(paste0(data_dir, "occurrences_full_refined.csv"),
           showProgress = FALSE)
  
  # delete full dataset without colnames
  invisible(file.remove(paste0(data_dir, "occurrences_full.csv")))
  
}


# Pruning and addition of extra data --------------------------------------


# Below code has been adapted from work that Franziska M. Willems did
#  see Data.Rmd for her version


if (exists("force.occ.prune") && file.older.check(
  paste0(data_dir, "occurrences_full_refined.csv"),
  paste0(data_dir, "occurrences_full_pruned_clim_elev.csv"))) {
  
  run.occ.prune <- force.occ.prune
  
} else {
  
  run.occ.prune <- TRUE
  
}

if (run.occ.prune) {
  
  
  dat.occ.prepruned <- fread(paste0(data_dir, "occurrences_full_refined.csv"),
                             showProgress = FALSE) %>% 
    
    # filter out records without determined species
    filter(species != "") %>% 
    
    # filter out records without recorded location
    drop_na(decimalLatitude, decimalLongitude) %>% 
    
    # filter out all years before 1980 and after 2020
    filter(year >= 1980, year <= 2020) %>% 
    
    # filter out certain days of the year with unnaturally high numbers of 
    #   records
    #   In Franziskas notebook, the last three days are not actually filtered
    filter(!(doy %in% c(1, 95, 121, 163, 164, 166, 181))) %>% 
    
    # additionally (also not in Franziskas script) filter out the last
    #  doy in the MnhnL (Musée national d’histoire naturelle Louxembourg) 
    #  data set
    filter(!(doy %in% c(365, 366) & institutionCode == "MnhnL")) %>% 
    
    # add extra decade column, which includes 2020 in the 2010s decade
    mutate(decade2 = str_replace_all(decade, "2020", "2010")) %>% 
    
    # rename synonymous names
    mutate(species = recode(species, "Inachis io" = "Aglais io")) %>% 
    
    # sort data by year for later addition of climate data
    arrange(year)
  
  
  # Addition of temp and elevation data -------------------------------------
  
  # ensure climate data is present
  if (!(file.exists("static_data/cru_climate_data.RDS"))) {
    
    warning(paste("Cropped climate data not present, attempting to run",
                  "script for generating that. If you get errors ensure",
                  "all necessary files are downloaded."))
    
    source("scripts/extract_cru_data.R")
    
  }
  
  cru_data <- readRDS("static_data/cru_climate_data.RDS")
  
  # download (if not already present) and load rastered elevation (altitude) data
  elev <- raster::getData("alt", country = "DEU", path = "data")
  
  # intialize elevation and temp vectors
  temp_vec <- rep(0, nrow(dat.occ.prepruned))
  elev_vec <- rep(0, nrow(dat.occ.prepruned))
  
  # intialize index for elevation and temp vectors
  i <- 0
  j <- 0
  
  # jointly extract climate and elevation data
  for (brick in names(cru_data)) {
    
    # extract beginning and end year of brick data
    start_stop_year <- str_split(brick, "-", simplify = TRUE)
    
    # get subset of occ data corresponding to period of the current brick
    dat.occ.brick <- dat.occ.prepruned %>% 
      select(species, year, decimalLatitude, decimalLongitude) %>% 
      filter(start_stop_year[1] <= year, year <= start_stop_year[2])
    
    # set indices for saving temp and elev data
    i <- j + 1
    j <- i + nrow(dat.occ.brick) - 1
    
    # generate df with only coordinates of points, sorted by year of record
    dat.occ.sp <- dat.occ.brick %>%
      select(decimalLatitude,
             decimalLongitude)
    coordinates(dat.occ.sp) <- ~ decimalLongitude + decimalLatitude
    
    # extract elevation data for the current dataset
    elev_vec[i:j] <- raster::extract(elev, dat.occ.sp)
    
    # extract temperature data and calculate the yearly mean temperature at 
    #   the location of the record
    
    temp_df <- raster::extract(cru_data[[brick]], dat.occ.sp) %>%
      as.data.frame() %>% 
      mutate(year = dat.occ.brick$year) 
    
    # remove spatial points df again
    rm(dat.occ.sp)
    
    # get a vector of all the years
    year_var_vec <- as.character(sort(unique(temp_df$year)))
    
    # calculate annual temp means, order is still the same as the records, sorted
    #   by year
    temp_df <- as.data.frame(sapply(year_var_vec,
                                    function(x) {
                                      rowMeans(temp_df[, grep(x, names(temp_df))])
                                    }))
    names(temp_df) <- year_var_vec
    
    # select the correct cells corresponding to the year
    # done in a lazy way without preallocation because i want to be done with this
    temp_vec_brick <- numeric(nrow(dat.occ.brick))
    for (year_var in year_var_vec) {
      year_select_vec <- which(dat.occ.brick$year == year_var)
      temp_vec_brick[year_select_vec] <- temp_df[[year_var]][year_select_vec]
    }
    
    rm(year_select_vec)
    
    temp_vec[i:j] <- temp_vec_brick
    
    rm(temp_vec_brick)
    
  }
  
  # add elevation and climate data and prune out records 
  dat.occ.prepruned <- dat.occ.prepruned %>% 
    
    # add both elevation and temperature data
    mutate(elev = elev_vec, temp = temp_vec) %>% 
    
    # remove records where that information could not be determined
    drop_na(elev) %>% 
    filter(temp > -Inf) %>% 
    
    # filter out records of species with less than 30 records overall
    # after records without elevation and temp pruned
    # this is made obsolete by the 10 records per decade requirement
    group_by(species) %>% 
    filter(n() >= 30) %>% 
    ungroup() %>% 
    
    # filter out species with less than 10 records in any decade
    group_by(species, decade2) %>% 
    # first filter out decades with less than 10 records
    filter(n() >= 10) %>% 
    group_by(species) %>% 
    # then filter out species where not all decades are present
    filter(uniqueN(decade2) >= 4) %>% 
    
    # rename coordinate columns to fit with the rest
    rename(lat = decimalLatitude, long = decimalLongitude)
  
  
  # save number of records per species
  dat.occ.prepruned %>% 
    group_by(id.grp, species) %>% 
    count() %>%
    fwrite(paste0(data_dir, "n_species_pruned_sum.csv"),
           showProgress = FALSE)
  
  # save number of species per id.grp
  n_spec_id_grp <- dat.occ.prepruned %>% 
    select(id.grp, species) %>% 
    distinct() %>% 
    group_by(id.grp) %>% 
    count() %>% 
    rename(n_species = n)
  
  # save ranges of number of records per species
  range_spec_id_grp <- dat.occ.prepruned %>% 
    group_by(id.grp, species) %>% 
    count() %>%
    group_by(id.grp) %>% 
    summarise(min_species_rec    = min(n),
              max_species_rec    = max(n),
              median_species_rec = median(n))
  
  # save number of records per id.grp
  dat.occ.prepruned %>% 
    group_by(id.grp) %>% 
    count() %>%
    rename(n_rec = n) %>% 
    left_join(n_spec_id_grp, by = "id.grp") %>% 
    left_join(range_spec_id_grp, by = "id.grp") %>% 
    fwrite(paste0(data_dir, "n_records_by_idgrp_pruned.csv"),
           showProgress = FALSE)
  
  # save data
  fwrite(dat.occ.prepruned, paste0(data_dir,
                                   "occurrences_full_pruned_clim_elev.csv"),
         showProgress = FALSE)
  
  rm(dat.occ.prepruned, temp_vec, elev_vec)
  
}
