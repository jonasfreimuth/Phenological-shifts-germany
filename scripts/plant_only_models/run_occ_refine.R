# read extracted occurence.txt, do stuff with it and save it
# the actual refinement
fread(paste(sep = "/", "download", paste0("occurrence_", k, ".txt")),
      quote = "", showProgress = FALSE,
      select = data_cols) %>% 
  
  # exclude non-plant records
  filter(kingdom == "Plantae") %>% 
  
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