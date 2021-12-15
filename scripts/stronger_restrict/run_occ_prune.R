# load prepruned data
dat.occ.prepruned <- fread(paste0(data_dir, "occurrences_full_prepruned.csv"),
                           showProgress = FALSE)


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
  filter(n() >= 15) %>% 
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