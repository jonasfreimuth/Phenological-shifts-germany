dat_occ_file_full <- "data/occurrences_full_pruned_clim_elev.csv"

# ensure full dat.occ file is present
if (!file.exists(dat_occ_file_full)) {
  source("scripts/get_occ_data_all_in_one.R")
}

dat.occ <- fread(dat_occ_file_full,
                 showProgress = FALSE)  %>%
  
  # remove records w/o determined temp, shouldn't be necessary any more
  drop_na(temp, elev)

# calculate roughly how many species we want
#   this is likely lower than the # of species sampled at the end in order
#   to increase the probability of at least one species for each id.grp
#   to be present
samp_size <- ceiling((uniqueN(dat.occ$species) * 0.02))

# calculate how many species from each group we need to represent the 
#   proportions in the overall data
spec_sizes <- dat.occ %>% 
  group_by(id.grp) %>% 
  mutate(group_size = uniqueN(species)) %>%
  ungroup() %>% 
  distinct(id.grp, species, group_size) %>% 
  mutate(group_size = group_size / sum(unique(group_size))) %>% 
  mutate(sample_n = ceiling(group_size * samp_size))

# initialize overall species vector
spec_vec <- character(0)

# for each id.grp sample the determined amount of species
for (id.grp_var in unique(spec_sizes$id.grp)) {
  
  # filter down to current id.grp
  spec_size_id_grp <- spec_sizes %>% 
    filter(id.grp == id.grp_var) 
  
  # get amount of species to be sampled
  n <- spec_size_id_grp$sample_n[1]
  
  # sample species
  spec_size_id_grp <- spec_size_id_grp %>% 
    slice_sample(n = n)
  
  # add species to overall species vector
  spec_vec <- c(spec_vec, spec_size_id_grp$species)
}

# filter data down to sampled species
dat.occ <- dat.occ %>% 
  filter(species %in% spec_vec)

# save reduced dataset as test data
fwrite(dat.occ, dat_occ_file)

rm(dat.occ, spec_vec, spec_size_id_grp, n, spec_sizes)
