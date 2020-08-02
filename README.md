# Phenological-shifts-germany

## Setup

* After downloading the repository, open the project in RStuio via ``Phenological-shifts-germany.Rproj`` in the main directory
* renv should be automatically intalling
* to install all necessary packages, simply call ``renv::restore() ``

## The Code

Everything necessary for obtaining the results from the paper is done from within ``Analysis_main.Rmd``.We also included the scripts we used to obtain plant trait data (``get_bioflor_traits.R``), the climate data for Germany (``get_german_climate_data.R``) and the script used to compile that data from GBIF (``start_gbif_downloads.R``). They are located in the ``Scripts`` folder, along with additional scripts run from within the main analysis notebook.
