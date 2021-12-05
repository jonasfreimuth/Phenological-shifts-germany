
# read refined occurrences and preprune them according to the current analysis
# prepruning means selecting data before addition of extra information and 
#  before another, final round of pruning
fread(paste0(data_dir, "occurrences_full_refined.csv"),
      showProgress = FALSE) %>% 
  
  # filter out records without determined species
  filter(species != "") %>% 
  
  # filter out records without recorded location
  drop_na(decimalLatitude, decimalLongitude) %>% 
  
  # filter out plant species not from specific institutions
  filter(!(id.grp == "Plants" & !(institutionCode %in% c("naturgucker",
                                                         "iNaturalist")))) %>%
  
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
  arrange(year) %>% 
  
  # save pruned file
  fwrite(paste0(data_dir, "occurrences_full_prepruned.csv"),
         showProgress = FALSE)