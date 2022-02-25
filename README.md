# Phenological-shifts-germany

R scripts for the analysis of shifts in phenology of plants and their pollinators in Germany. The latest preprint of the manuscript can be found on 
[bioRxiv](https://doi.org/10.1101/2021.01.10.425984).

## Requirements

### Software:
* R 4.0 + 
* RStudio (for running notebooks, any other way will do)

### Hardware:
The models were run with about 24GB of physical memory and 100GB of swap memory.

## Setup

* Ensure ``renv`` is installed (Should be given if you are using Rstudio) 
* to install all necessary packages, simply call ``renv::restore()``
* To run the analysis some additional data sets need to be downloaded and placed in the ``static_data`` folder:
  * Plant pollinator interaction data set from: https://doi.org/10.5285/6d8d5cb5-bd54-4da7-903a-15bd4bbd531b 
    (login required, account creation should be possible for everyone)
  * Plant trait data from bioflor. This should be done automatically, but the script attempts to resolve the current accepted name
    for each species. Therefore running this script at a later date might result in different species being included in the analysis. The original
    data set used in the analysis is available on request.

## The Code

Everything necessary for obtaining the results from the paper is done from within ``analysis.Rmd``. Some data, for
example the averaged climate datasets and the dataset keys for the gbif datasets used, is already provided in the
``static_data`` folder. We also included the scripts we used to obtain plant trait data (``get_bioflor_traits.R``),
and the script used to compile that data from GBIF (``start_gbif_downloads.R``). They are located in the ``Scripts``
folder, along with additional scripts run from within the main analysis notebook. 
