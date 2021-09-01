# Script based on work Franziska M. Willems did. Her work can be found in 
#   Get_CRUdata.RMD


# Load packages
library("raster")
library("ncdf4")
library("dplyr")
library("tidyr")
library("stringr")
library("data.table")

# individual files must be manually downloaded from 
#   https://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.05/data/tmp (account
#   required) and unzipped.

# A summary on how to deal with data from the Climate Research Unit can be 
#   found here: https://www.benjaminbell.co.uk/2018/01/getting-climate-data.html

# Individual file links are:
# https://dap.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.05/data/tmp/cru_ts4.05.1971.1980.tmp.dat.nc.gz?download=1
# https://dap.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.05/data/tmp/cru_ts4.05.1981.1990.tmp.dat.nc.gz?download=1
# https://dap.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.05/data/tmp/cru_ts4.05.1991.2000.tmp.dat.nc.gz?download=1
# https://dap.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.05/data/tmp/cru_ts4.05.2001.2010.tmp.dat.nc.gz?download=1
# https://dap.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.05/data/tmp/cru_ts4.05.2011.2020.tmp.dat.nc.gz?download=1

# Full citation: 
# University of East Anglia Climatic Research Unit; Harris, I.C.; Jones, P.D.;
#   Osborn, T. (2021): CRU TS4.05: Climatic Research Unit (CRU) Time-Series (TS)
#   version 4.05 of high-resolution gridded data of month-by-month variation in
#   climate (Jan. 1901- Dec. 2020). NERC EDS Centre for Environmental Data
#   Analysis, date of citation.
#   https://catalogue.ceda.ac.uk/uuid/c26a65020a5e4b80b20018f148556681

# load in unzipped files
climate_data <- list()

for (file in list.files("download", "cru_ts4.05*")) {
  
  # extract years covered by the current file
  dur_string <- paste(str_extract_all(file, "([0-9]{4})", simplify = TRUE),
                      collapse = "-")
  
  climate_data[[dur_string]] <- brick(paste0("download/", file),
                                      varname = "tmp")
}

# crop out the area we need, i.e. germany
GER.area <- extent(5.8663129806518555, 15.041742324829102,
                   47.27015165787987, 55.05835230401308)

for (brick in names(climate_data)) {
  climate_data[[brick]] <- crop(climate_data[[brick]], GER.area)
}

# save climate data object for later combining with data
saveRDS(climate_data, "static_data/cru_climate_data.RDS")
