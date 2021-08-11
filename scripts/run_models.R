
# Setup -------------------------------------------------------------------

library("stringr")
library("data.table")
library("here")
library("lme4")
library("JWileymisc")
library("multilevelTools")
library("dplyr")
library("tidyr")
library("ggplot2")
library("broom.mixed")

source("scripts/functions.R")

# do a testing run? 
# will reduce data size and save outputs into separate directories
test_run <- TRUE

# save diagnostics plots
# will take a very long time on the full dataset
plot_diagnostics <- TRUE

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

model_log_file <- paste0(log_path, "/",
                         format(Sys.time(), format = "%Y%m%d_%H%M"),
                         "_models",
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
    frac_species <- sample(all_species, length(all_species) * 0.005)
    
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
  
  # Formula stuff ---------------------
  
  # split model formula into constituents
  mod_comps <- str_split(form, " ", simplify = TRUE)
  
  # extract response and fist independent variable
  simple_form <- mod_comps[, 1:3] %>%
    paste(collapse = "")
  
  # extract main independent var
  main_var <- mod_comps[, 3]
  
  # convert formula string to proper formula
  mod_form <- as.formula(form)
  
  # Model running ---------------------
  
  log_msg("Starting", simple_form, "model...")
  
  time_stamp <- format(Sys.time(), format = "%Y%m%d_%H%M")
  
  # run the model, this step takes a lot of time
  glm_mod <- lmer(mod_form, data = dat.occ)
  
  
  # TODO: do a check for false convergence and take steps for ensuring this is
  #   not a problem
  
  log_msg("... Done.")
  log_msg("Saving model to disk...")
  
  # save model to disk
  saveRDS(glm_mod,
          file = paste0(data_path, "/", 
                        time_stamp, "_",
                        "glmm_model_",
                        str_replace(simple_form, "~", "_"),
                        ".rds"))
  
  log_msg("... Done.")
  
  # Model results extraction ----------
  
  log_msg("Computing summary and saving to disk...")
  
  # save summary output to disk
  capture.output(summary(glm_mod), 
                 file = paste0(data_path, "/",
                               time_stamp, "_",
                               "glmm_summary_",
                               str_replace(simple_form, "~", "_"),
                               ".txt"))
  
  log_msg("... Done.")
  
  # if a random effect is present, extract the coefficients for it
  if (str_detect(form, "(?<=\\|\\s?)\\w+")) {
    
    # TODO: make sure this check for presence of random variable works as 
    #   intended
    
    log_msg("Extracting random effects for", simple_form, "model...")
    
    rnd_eff <- tidy(glm_mod, c("ran_vals"))
    
    fwrite(rnd_eff,
           paste0(data_path, "/", 
                  time_stamp, "_",
                  "glmm_rnd_eff_",
                  str_replace(simple_form, "~", "_"),
                  ".csv"))
    
    rm(rnd_eff)
    
    log_msg("... Done.")
  }
  
  # Diagnostic plots ------------------
  
  if (plot_diagnostics) {
    
    log_msg("Extracting residuals and fitted values...")
    
    mod_resid <- residuals(glm_mod)
    mod_fitvl <- fitted   (glm_mod)
    
    log_msg("... Done.")
    
    log_msg("Generating and saving diagnostics plots...")
    
    # save diagnostics plot
    png(paste0(plot_path, "/", 
               time_stamp, "_",
               "glmm_generic_diagnostic_",
               str_replace(simple_form, "~", "_"),
               ".png"),
        width = 2000,
        height = 1200
    )
    
    plot(modelDiagnostics(glm_mod), nrow = 3, ncol = 2, ask = FALSE)
    
    dev.off()
    
    # save resid vs fitted
    ggsave(paste0(plot_path, "/",
                   time_stamp, "_",
                  "glmm_resid_fit_",
                   str_replace(simple_form, "~", "_"),
                  ".png"),
           lmResFitPlot(mod_resid, mod_fitvl, dat.occ$id.grp),
           width = 20, height = 12)
    
    # TODO: plot residuals vs every fixed effect 
    #   (should happen in modelDiagostics)
    # TODO: plot residuals for every random effect
    
    # hist residuals
    # resid vs predictors
    
    # TODO: check if everything is removed that needs to be
    rm(mod_resid, mod_fitvl)
    
  } else {
    
    # remove model object
    rm(glm_mod)
    
  }
  
  log_msg("Done with", simple_form, "model...")
  
}

log_msg("All done.")

# reset logging file
options("log_file" = NULL)
