

# Control behvaiour of analysis notebook ----------------------------------

# use the controls here
# if false uses controls laid out in the notebook
override_control <- TRUE

# name of analysis
# as a prerequisite, scripts have to be available under scripts/analysis_dname/
analysis_dname <- "full_analysis_data/"

# do an overall test run? 
# this will mean working with a dataset with reduce number of species
# so everything will be faster
test_run <- FALSE


## Data getting:

# run script for retrieving climate data from DWD regardless of data being 
# already present?
force.clim.get      <- FALSE

# run script for retrieving plant trait data from bioFlor regardless of data 
# being already present?
force.traits.get    <- FALSE

# force running of occurrence getting script
force.occ.get       <- FALSE

# control which of the downloaded files to delete
#   - "all": delete both .zip files and extracted occ .txt files
#   - "zip": delete only .zip files, keep .txt
#   - "txt": delete only .txt files, keep .zip
#   - "non": keep all files in download folder
delete.occ.download <- "non"

# force pruning of data and addition of climate and elevation
force.occ.prune     <- FALSE


## Modeling

# force running of model generation script
force.models           <- FALSE

# run model script independently? 
# * models will be taken from the testing model formula file
# * no checks will be performed whether the models are appropriate for the main
#   analysis
# * The results will not be integrated into the analysis
run.models.ind         <- FALSE

# how to treat predictor variables
# not currently used
center_preds           <- FALSE
scale_preds            <- FALSE

# save plot of random regression slopes over data?
# will add additional running time, increasing with # of data points
plot_rnd_slopes        <- TRUE

# save diagnostics plots
# will take a very long time on the full dataset
plot_diagnostics       <- TRUE

#save faceted plots of diagnostic plots 
plot_diagnostics_facet <- TRUE

# set number of times a model with failed convergence will attempt to restart
n_restart               <- 2


## Plot saving

# save plots to disk?
save_plots  <- TRUE

# save tables (as .docx)?
save_tables <- TRUE



# Render analysis notebook ------------------------------------------------

rmarkdown::render("analysis.Rmd",
                  output_file = paste0("analysis_",
                                       gsub("/", "", analysis_dname),
                                       ".nb.html"))