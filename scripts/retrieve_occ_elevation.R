library("raster")
library("sp")
library("dplyr")
library("tidyr")
library("data.table")

source("scripts/functions.R")

dat.occ <- fread("data/f_occurrences_full_pruned.csv") %>% 
  as.data.frame()

elev <- getData("alt", country = "DEU", path = "data")

dat.occ.sp <- SpatialPoints(coords = as.matrix(dat.occ %>% 
                                                 drop_na(lat) %>% 
                                                 select(long, 
                                                        lat)),
                            proj4string = elev@crs)

dat.occ.elev <- raster::extract(elev, dat.occ.sp)
dat.occ <- mutate(dat.occ, elev = dat.occ.elev)

dir.check("data")
fwrite(dat.occ, "data/f_occurrences_full_pruned_elev.csv")

if (sys.nframe() == 0) {
  library(ggplot2)
  
  germany <- getData(country = "DEU", level = 2)
  
  ggplot() + 
    geom_polygon(data = germany,
                 aes(long, lat, group = group)) +
    coord_sf() +
    geom_point(data = dat.occ %>% slice_sample(n = 20000),
               aes(decimalLongitude, decimalLatitude, col = elev)) + 
    theme_minimal()
  
}
