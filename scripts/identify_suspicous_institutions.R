
#load data
dat.occ.full <- fread(
  here("data", "occurrences_full_refined.csv")) %>% 
  
  #remove records without determined species
  filter(species != "") %>%
  
  # #exclude the current year from further analysis
  # filter(year != as.integer(substr(
  #   Sys.Date(), start = 1, stop = 4
  # ))) %>%
  
  #exclude first and last days
  filter(doy != 1, doy != 366, doy != 365) %>% 
  
  filter(year >= year.start & year <= year.stop)



# records in germany ------------------------------------------------------

germany_pol <- SpatialPolygons(germany@polygons,
                               proj4string = germany@proj4string)

dat.sub <- dat.occ.full %>% 
  slice_sample(n = 10000) %>%
  drop_na(decimalLatitude)


dat.sub.sp <- SpatialPoints(
  coords = as.matrix(dat.sub %>%
                       dplyr::select(decimalLongitude,
                                     decimalLatitude)),
  proj4string = germany@proj4string)

rec_to_region <- !is.na(over(dat.sub.sp, germany_pol)) 
  
dat.sub <- filter(dat.sub, rec_to_region)



# select records outside germanys extreme points
dat.occ.out <- dat.occ.full %>% 
  drop_na(decimalLatitude) %>% 
  filter(!(decimalLatitude >= 47.27015165787987 &
           decimalLatitude <= 55.05835230401308 &
           decimalLongitude >= 5.8663129806518555 &
           decimalLongitude <= 15.041742324829102)) 

# find out which institutions are represented outside
ins_out <- dat.occ.out%>% 
  group_by(institutionCode) %>% 
  summarise(n_rec = n()) %>% 
  mutate(perc_rec = (n_rec / sum(n_rec)) * 100)



