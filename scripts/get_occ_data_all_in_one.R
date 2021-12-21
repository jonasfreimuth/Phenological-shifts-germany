

# Setup: Dir stuff --------------------------------------------------------

# make sure data folder exists
dir.check(data_dir)


## check if analysis specific scripts exist, else use default scripts, 
##  checks this for each step

# get vector of analysis step script names
# ORDER MATTERS
script_name_vec <- c("run_occ_refine.R",
                     "run_occ_preprune.R",
                     "run_occ_prune.R")

# get vectors of default and analysis specific script paths
default_vec <- paste0("scripts/full_analysis_data/", script_name_vec)
dname_vec   <- paste0("scripts/", analysis_dname, script_name_vec)

# check which analysis specific files exist
sel_vec <- file.exists(dname_vec)

# get vector of scripts that should run
run_vec <- default_vec
run_vec[sel_vec] <- dname_vec[sel_vec]


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

# set names of columns retrieved by fread from raw data
data_cols <- c("kingdom", "phylum", "order", "family", "genus",
               "species", "institutionCode", "collectionCode",
               "datasetName","decimalLatitude", "decimalLongitude",
               "year", "month", "day", "eventDate",
               "hasGeospatialIssues", "issue", "basisOfRecord", 
               "accessRights", "license", "datasetKey")


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
    
    # run refinement scripts
    # has to use this environment
    # check if analysis specific script exists, if not use default
    source(run_vec[1])
    
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
  fread(paste0(data_dir, "occurrences_full.csv"),
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
  
  
  # do prepruning according to analysis
  # has to use this environment
  source(run_vec[2])
  
  # do data addition and pruning according to analysis
  # has to use this environment
  source(run_vec[3])
  
  # remove prepruned file again for disk space reasons
  file.remove(paste0(data_dir, "occurrences_full_prepruned.csv"))
  
}
