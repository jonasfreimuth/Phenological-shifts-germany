# Setup -------------------------------------------------------------------


#load packages
library("rdwd")
library("beepr")
library("RCurl") # important to get a current index for selectDWD
library("data.table")
library("tidyverse")

# supress dplyr::summarise() message about leftover groups
options(dplyr.summarise.inform = FALSE) 

#select a data set
link <- selectDWD(res = "monthly", var = "kl" , per = "hr", current = TRUE)

#import data set
fnames <- dataDWD(url = link, read = FALSE)
dat.clim <- readDWD(file = fnames, varnames = TRUE)

#only select historical records and
#turn object into regular data frame, drop unuseful columns, remove NAs
dat.clim.hist <- dat.clim[str_detect(names(dat.clim), "historical")] %>%
  bind_rows() %>%
  select(STATIONS_ID, date = MESS_DATUM, temp = MO_TT.Lufttemperatur) %>% 
  mutate(year = as.integer(substr(date, 1, 4))) %>%
  mutate(month = as.integer(substr(date, 6, 7))) %>%
  drop_na(temp)

# #same as above and only for the last year
# # this is a remnant from the original script compiled in 2019, where the data
# # for that year had not undergone rigorous quality control yet. Only use this
# # when you know you need data for the current year which the historical 
# # records cannot provide
# dat.clim.rec <- dat.clim[str_detect(names(dat.clim), "recent")] %>%
#   bind_rows() %>%
#   select(STATIONS_ID, date = MESS_DATUM, temp = MO_TT.Lufttemperatur) %>%
#   mutate(year = as.integer(substr(date, 1, 4))) %>%
#   mutate(month = as.integer(substr(date, 6, 7))) %>%
#   filter(year == as.integer(substr(Sys.Date(), start = 1, stop = 4))-1) %>%
#   drop_na(temp)
# 
# #bind both together
# dat.clim <- bind_rows(dat.clim.hist, dat.clim.rec)

dat.clim <- dat.clim.hist
rm(dat.clim.hist)

# Calculate overall mean and spring temperature ---------------------------

#calculate yearly mean
dat.clim.tot <- dat.clim %>% 
  group_by(year) %>%
  summarise(y_mean_temp = mean(temp, na.rm = TRUE))

# calculate spring mean (February until May) and add to 
# total mean
dat.clim.tot <- dat.clim %>% 
  filter(month >= 2 & month <= 5) %>%
  group_by(year, .add = TRUE) %>%
  summarise(spring1 = mean(temp, na.rm = TRUE))%>%
  left_join(dat.clim.tot, by = "year")



#  Calculate decadal mean and spring temperature --------------------------

#calculate yearly mean
dat.clim.dec <- dat.clim %>%
  mutate(decade = floor(year/10)*10) %>%
  group_by(decade) %>%
  summarise(y_mean_temp = mean(temp, na.rm = TRUE))

# calculate spring mean (February until May)
dat.clim.dec <- dat.clim %>% 
  filter(month >= 2 & month <= 5) %>%
  mutate(decade = floor(year/10)*10) %>%
  group_by(decade) %>%
  summarise(spring1 = mean(temp, na.rm = TRUE))%>%
  left_join(dat.clim.dec, by = "decade")


# Calculate mean and spring temperature by state --------------------------

#calculate yearly mean temperature
dat.climx <- dat.clim %>%
  group_by(STATIONS_ID, .add = TRUE) %>%
  group_by(year, .add = TRUE) %>%
  summarise(y_mean_temp = mean(temp, na.rm = TRUE))

#calculate mean spring temperature and add to yearly mean temperature data
dat.climx <- dat.clim %>% 
  filter(month >= 2 & month <= 5) %>%
  group_by(STATIONS_ID, .add = TRUE) %>%
  group_by(year, .add = TRUE) %>%
  summarise(spring1 = mean(temp, na.rm = TRUE))%>%
  right_join(dat.climx, by = c("year", "STATIONS_ID"))

#read meta files
data("metaIndex")

#join Bundesland to temperature data
dat.climx <- metaIndex %>%
  filter(per == "historical" &
           hasfile == TRUE & var == "kl" & res == "daily") %>%
  select(
    STATIONS_ID = Stations_id,
    Stationshoehe,
    geoBreite,
    geoLaenge,
    Stationsname,
    state = Bundesland
  ) %>%
  right_join(y = dat.climx, by = "STATIONS_ID")

#calculate state means for each year
dat.clim.mean <- dat.climx %>%
  group_by(state, .add = TRUE) %>%
  group_by(year, .add = TRUE) %>%
  summarise(y_mean_temp = mean(y_mean_temp, na.rm = TRUE),
            spring1 = mean(spring1, na.rm = TRUE))



# Save Datasets -----------------------------------------------------------


#save overall mean temperature
fwrite(dat.clim.tot, 'static_data/overall_mean_temperature.csv')

#save decadal means
fwrite(dat.clim.dec,  'static_data/decadal_mean_temperature.csv')

#save mean state temperature data
fwrite(dat.clim.mean, 'static_data/state_mean_temperature.csv')

beep()
