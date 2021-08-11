
# Setup -------------------------------------------------------------------

library("stringr")
library("data.table")
library("here")
library("parallel")
library("lme4")
library("JWileymisc")
# library("multilevelTools")
library("DHARMa")
library("dplyr")
library("tidyr")
library("ggplot2")
library("broom.mixed")

source("scripts/functions.R")

# do a testing run? 
# will reduce data size and save outputs into separate directories
test_run <- TRUE

# set number of cores for model fitting to maximum, not sure whether this will
# actually help
n_cores <- detectCores()
if (is.na(n_cores)) { n_cores <- 1}
options(glmmTMB.cores = n_cores)

# TODO: make all the path stuff relative

# set saving paths
if (!test_run) {
  log_path <- here("logs")
  plot_path <- here("plots")
  data_path <- here("data")
} else {
  log_path <- here("temp/logs")
  plot_path <- here("temp/plots")
  data_path <- here("temp/data")
}

# ensure paths are present
dir.check(log_path)
dir.check(plot_path)
dir.check(data_path)

model_log_file <- paste0(log_path, "/models_",
                         format(Sys.time(), format = "%Y%m%d_%H%M"),
                         ".log")

file.create(model_log_file)

options("log_file" = model_log_file)

# TODO
#   give residual vs fitted
#   give res vs fit for random groups
#   give cooks distances


# Data loading ------------------------------------------------------------

# read in data, reduce columns to only those relevant here
select_cols <- c('kingdom', 'order', 'family', 'species', 'year', 'doy',
                 'decade', 'id.grp', 'long', 'lat', 'temp')

log_msg("Loading and centering data...")

if (!(test_run && exists("dat.occ"))) {
  
  dat.occ <- fread(paste0("data/f_occurrences_full_pruned.csv"),
                   select = select_cols,
                   showProgress = FALSE) %>%
    
    # remove records w/o determined temp
    drop_na(temp) %>% 
    
    # center main independent variables to a mean of 0
    mutate(temp = temp - mean(temp),
           year = year - mean(year),
           lat = lat - mean(lat),
           long = long - mean(long))
  
  if (test_run) {
    
    # if we do a test run, restrict data to subset of species
    all_species <- unique(dat.occ$species)
    frac_species <- sample(all_species, length(all_species) * 0.01)
    
    log_msg("Test run, pruning data down to species:")
    
    log_msg(paste(frac_species, collapse = ", "))
    
    
    dat.occ <- dat.occ %>% 
      filter(species %in% frac_species)
    
  }
} 

log_msg("... Done.")


if (test_run) {
  
  # read in preset model formulas
  form_vec <- readLines("static_data/model_formulas_test.txt")
  
} else {
  
  # read in preset model formulas
  form_vec <- readLines("static_data/model_formulas.txt")
  
}



# Model loop --------------------------------------------------------------

# loop through all preset models
for (form in form_vec) {
  
  # split model formula into constituents
  mod_comps <- str_split(form, " ", simplify = TRUE)
  
  # extract response and fist independent variable
  simple_form <- mod_comps[, 1:3] %>%
    paste(collapse = "")
  
  # extract main independent var
  main_var <- mod_comps[, 3]
  
  log_msg("Starting", simple_form, "model...")
  
  # convert formula string to proper formula
  mod_form <- as.formula(form)
  
  # run the model, this step takes a lot of time
  glm_mod <- glmmTMB(mod_form, family = gaussian, data = dat.occ)
  
  
  # TODO: do a check for false convergence and take steps for ensuring this is
  #   not a problem
  
  
  log_msg("... Done.")
  log_msg("Saving model to disk...")
  
  time_stamp <- format(Sys.time(), format = "%Y%m%d_%H%M")
  
  # save model to disk
  saveRDS(glm_mod,
          file = paste0(data_path, "/glmm_model_",
                       str_replace(simple_form, "~", "_"), "_",
                       time_stamp, ".rds"))
  
  log_msg("... Done.")
  log_msg("Computing summary and saving to disk...")
  
  # save summary output to disk
  capture.output(summary(glm_mod), 
                 file = paste0(data_path, "/glmm_summary_",
                              str_replace(simple_form, "~", "_"), "_",
                              time_stamp, ".txt"))
  
  log_msg("... Done.")
  
  
  if (str_detect(form, "(?<=\\|\\s?)\\w+")) {
    
    # TODO: make sure this check for presence of random variable works as 
    #   intended
    
    log_msg("Extracting random effects for", simple_form, "model...")
    
    rnd_eff <- tidy(glm_mod, c("ran_vals"))
    
    fwrite(rnd_eff,
           paste0(data_path, "/glmm_rnd_eff_",
                  str_replace(simple_form, "~", "_"), "_",
                  time_stamp,
                  ".csv"))
    
    rm(rnd_eff)
  
    log_msg("... Done.")
  }
  
  log_msg("Generating and saving diagnostics plots...")
  
  # generate residuals
  glm_resid <- residuals(glm_mod)
  
  # remove model object
  rm(glm_mod)
  
  # save diagnostics plot
  png(paste0(plot_path, "/glmm_qq_overall_",
             str_replace(simple_form, "~", "_"), "_",
             time_stamp, ".png"),
      width = 2000,
      height = 1200
  )
  
  qqnorm(scale(glm_resid))
  abline(0, 1)
  
  dev.off()
  
  # remove objects due to memory limitations
  rm(glm_resid)
  
  log_msg("Done with", simple_form, "model...")
  
}

# if we are doing a test run, also save the dataset used
if (test_run) {
  fwrite(dat.occ, paste0(data_path, "/occurrences_temp_", time_stamp,
                         ".csv"))
}

log_msg("All done.")

# reset logging file
options("log_file" = NULL)
