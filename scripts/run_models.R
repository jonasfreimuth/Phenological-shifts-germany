
# Setup -------------------------------------------------------------------

library("glmmTMB")
library("stringr")
library("data.table")
library("here")
library("dplyr")
library("tidyr")

source("scripts/functions.R")

# set up logging
# TODO: make this fail save
dir.check(here("logs"))

model_log_file <- paste0("logs/models_",
                         format(Sys.time(), format = "%Y%m%d_%H%M"),
                         ".log")

file.create(model_log_file)

options("log_file" = model_log_file)

# read in data
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
  
  mod_form <- as.formula(form)
  
  # split model formula into constituents
  mod_comps <- str_split(form, " ", simplify = TRUE)
  
  # extract response and fist independent variable
  simple_form <- mod_comps[, 1:3] %>%
    paste(collapse = "")
  
  # extract main independent var
  main_var <- mod_comps[, 3]
  
  log_msg("Starting", simple_form, "model...")
  
  # run the model, this step takes a lot of time
  glm_mod <- glmmTMB(mod_form, family = gaussian, data = dat.occ)
  
  log_msg("Done.")
  log_msg("Saving model to disk...")
  
  # save model to disk
  saveRDS(glm_mod,
          file = paste("data/glmm_model_",
                       str_replace(simple_form, "~", "_"), "_",
                       format(Sys.time(), format = "%Y%m%d_%H%M"), ".rds",
                       sep = ""))
  
  # save summary output to disk
  capture.output(summary(glm_mod), 
                 file = paste("data/glmm_summary_",
                       str_replace(simple_form, "~", "_"), "_",
                       format(Sys.time(), format = "%Y%m%d_%H%M"), ".txt",
                       sep = ""))
  
  log_msg("Extracting random effects for", simple_form, "model...")
  
  # extract intercept and main slope
  # this is not very elegant and might break
  intercept <- glm_mod$fit$par[1]
  slope <- glm_mod$fit$par[2]
  
  # extract random effects, add them to overall effects to obtain proper 
  # estimate
  rnd_eff <- ranef(glm_mod)$cond[[1]]
  rnd_eff[["(Intercept)"]] <- rnd_eff[["(Intercept)"]] + intercept
  rnd_eff[[main_var]] <- rnd_eff[[main_var]] + slope
  
  fwrite(rnd_eff, paste0("data/glmm_rnd_eff_",
                         str_replace(simple_form, "~", "_"), "_",
                         format(Sys.time(), format = "%Y%m%d_%H%M"),
                         ".csv"))
  
  # remove model due to memory limitations
  rm(glm_mod)
  
  log_msg("Done with", simple_form, "model...")
  
}
