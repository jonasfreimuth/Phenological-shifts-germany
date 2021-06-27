# TODO: put this in front of pruning script so that this lookup includes every
# species potentially in the dataset
dat.occ <- readRDS("data/DT_after_nonGer_exclusion_SpeciesFilteredAgain.rds")

lookup <- unique(dat.occ %>% select(species, id.grp))

write.csv(lookup, "data/id_grp_species_lookup.csv",
          row.names = FALSE)
