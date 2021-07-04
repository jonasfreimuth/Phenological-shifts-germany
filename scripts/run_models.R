
# Setup -------------------------------------------------------------------

library("stringr")
library("data.table")
library("here")
library("parallel")
library("glmmTMB")
library("dplyr")
library("tidyr")

source("scripts/functions.R")

# set number of cores for model fitting to maximum, not sure whether this will
# actually help
n_cores <- detectCores()
if (is.na(n_cores)) { n_cores <- 1}
options(glmmTMB.cores = n_cores)

# set up logging
# TODO: make this fail save
dir.check(here("logs"))

model_log_file <- paste0("logs/models_",
                         format(Sys.time(), format = "%Y%m%d_%H%M"),
                         ".log")

file.create(model_log_file)

options("log_file" = model_log_file)


# Data loading ------------------------------------------------------------

# read in data, reduce columns to only those relevant here
select_cols <- c('kingdom', 'order', 'family', 'species', 'year', 'doy',
                 'decade', 'id.grp', 'long', 'lat', 'temp')

dat.occ <- readRDS("data/DT_after_nonGer_exclusion_SpeciesFilteredAgain.rds") %>% 
  
  # reduce dataset, only use relevant columns
  select(matches(select_cols)) %>% 
  
  # # for testing, only take small subset of observations
  # slice_sample(n = 1000) %>%
  
  # remove records w/o determined temp
  drop_na(temp) %>% 
  
  # center main independent variables to a mean of 0
  mutate(temp = temp - mean(temp),
         year = year - mean(year),
         lat = lat - mean(lat),
         long = long - mean(long))

# read in preset model formulas
form_vec <- readLines("static_data/model_formulas.txt")


# Model loop --------------------------------------------------------------

# loop through all preset models
for (form in form_vec) {
  
  # split model formula into constituents
  mod_comps <- str_split(form, " ", simplify = TRUE)
  
  # extract response and fist independent variable
  simple_form <- mod_comps[, 1:3] %>%
    paste(collapse = "")
  
  # extract random variable
  # one or more word characters preceded by a pipe symbol and zero or one 
  #   whitespace
  rnd_var <- str_extract(form, "(?<=\\|\\s?)\\w+")
  
  # extract main independent var
  main_var <- mod_comps[, 3]
  
  log_msg("Starting", simple_form, "model...")
  
  # convert formula string to proper formula
  mod_form <- as.formula(form)
  
  # run the model, this step takes a lot of time
  glm_mod <- glmmTMB(mod_form, family = gaussian, data = dat.occ)
  
  log_msg("Done.")
  log_msg("Saving model to disk...")
  
  time_stamp <- format(Sys.time(), format = "%Y%m%d_%H%M")
  
  # save model to disk
  saveRDS(glm_mod,
          file = paste("data/glmm_model_",
                       str_replace(simple_form, "~", "_"), "_",
                       time_stamp, ".rds",
                       sep = ""))
  
  # save summary output to disk
  capture.output(summary(glm_mod), 
                 file = paste("data/glmm_summary_",
                              str_replace(simple_form, "~", "_"), "_",
  
  if (str_detect(form, "(?<=\\|\\s?)\\w+")) {
    
    # TODO: make sure this check for presence of random variable works as 
    #   intended
  
  log_msg("Extracting random effects for", simple_form, "model...")
  
    # extract random variable
    # one or more word characters preceded by a pipe symbol and zero or one 
    #   whitespace
    rnd_var <- str_extract(form, "(?<=\\|\\s?)\\w+")
    
  # extract intercept and main slope
  # this is not very elegant and might break
  intercept <- glm_mod$fit$par[1]
  slope <- glm_mod$fit$par[2]

  # extract random effects, add them to overall effects to obtain proper
  # estimate
  rnd_eff <- ranef(glm_mod)$cond[[rnd_var]]

  # quick bodge to get an appropriate df for the weird ways i want to fill it
  rnd_eff_out <- data.frame(row.names = 1:nrow(rnd_eff))

  rnd_eff_out[[rnd_var]] <- row.names(rnd_eff)
  rnd_eff_out[["Intercept"]] <- rnd_eff[["(Intercept)"]] + intercept
  rnd_eff_out[[main_var]] <- rnd_eff[[main_var]] + slope
  
  # rnd_eff_out <- coef(glm_mod)
  
  fwrite(rnd_eff_out,
         paste0("data/glmm_rnd_eff_",
                str_replace(simple_form, "~", "_"), "_",
                time_stamp,
                ".csv"))
  
  }
  
  
  log_msg("Done with", simple_form, "model...")
  
}
