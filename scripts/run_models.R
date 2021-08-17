
# Setup -------------------------------------------------------------------

library("stringr")
library("data.table")
library("lme4")
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

# set pattern for recognition of random effects in formulas
ranef_pattern <- "(?<=\\|\\s?)\\w+"

# set saving paths
if (!test_run) {
  log_path <- "logs"
  plot_path <- "plots"
  data_path <- "data"
} else {
  log_path <- "temp/logs"
  plot_path <- "temp/plots"
  data_path <- "temp/data"
}

# ensure paths are present
dir.check(log_path)
dir.check(plot_path)
dir.check(data_path)

model_log_file <- paste0(log_path, "/",
                         format(Sys.time(), format = "%Y%m%d_%H%M"),
                         "_models",
                         ".log")

options("log_file" = model_log_file)

# TODO
#   give cooks distances


# Data loading ------------------------------------------------------------

# read in data, reduce columns to only those relevant here
select_cols <- c('family', 'species', 'year', 'doy',
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
  
  # extract which components are fixed and which are random effects:
  #   TODO: consider effects of minus properly
  #   TODO: set this to only run when plots are enabled
  
  #   extract all independent vars
  ind_vars <- str_extract(form, "(?<=~).*")
  
  #   extract random variables
  rnd_vars <- str_extract_all(ind_vars, "(?<=\\().*(?=\\))")
  rnd_vars <- str_extract    (rnd_vars, "(?<=\\|\\s?)[\\w\\.]+")
  rnd_vars <- unique(rnd_vars)
  
  #   extract fixed variables
  #   WARNING: Interaction terms will be reduced to just their constituents
  fix_vars <- str_replace_all(ind_vars, "[\\s+]*\\(.*\\)", "")
  fix_vars <- str_replace_all(fix_vars, "\\s", "")
  fix_vars <- str_split(fix_vars, "[*:+-]+")[[1]]
  fix_vars <- unique(fix_vars)
  
  # Model running ---------------------
  
  log_msg("Starting", simple_form, "model...")
  
  time_stamp <- format(Sys.time(), format = "%Y%m%d_%H%M")
  
  # Check if the formula contains random effects, and if so use the right
  #   function (lmer acts up without random effect)
  # WARNING: This is potentially very stupid as lmer and lm return different 
  #   objects
  if (str_detect(form, ranef_pattern)) {
    
    # run the model, this step may take a lot of time
    #  although plotting will probably take longer
    glm_mod <- lmer(mod_form, data = dat.occ)
    
  } else {
    
    glm_mod <- lm(mod_form, dat.occ)
    
  }
  
  
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
  #  Currently, the script will not accept models without random effects anyway
  if (str_detect(form, ranef_pattern)) {
    
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
    
    log_msg("Extracting plotting data...")
    
    mod_resid <- residuals(glm_mod)
    mod_fitvl <- fitted   (glm_mod)
    
    rm(glm_mod)
    
    log_msg("... Done.")
    
    log_msg("Generating and saving diagnostics plots...")
    
    # save qq plot
    png(paste0(plot_path, "/",
               time_stamp, "_",
               "glmm_qq_",
               str_replace(simple_form, "~", "_"),
               ".png"),
        width = 2000,
        height = 1200
    )

    qqnorm(scale(mod_resid), ylab = "Scaled sample quantiles")
    abline(0, 1)

    dev.off()
    
    # TODO: Proper axis labels and titles
    
    # save resid vs fitted
    ggsave(paste0(plot_path, "/",
                   time_stamp, "_",
                  "glmm_resid_fit_",
                   str_replace(simple_form, "~", "_"),
                  ".png"),
           lmResFitPlot(mod_resid, mod_fitvl, dat.occ$id.grp),
           width = 20, height = 12)
    
    
    # save resid histogram
    ggsave(paste0(plot_path, "/",
                  time_stamp, "_",
                  "glmm_resid_hist_",
                  str_replace(simple_form, "~", "_"),
                  ".png"),
           ggplot(data = data.frame(resid = mod_resid),
                  aes(resid)) +
             geom_histogram() + 
             theme_minimal() +
             theme(panel.grid = element_blank()),
           width = 20, height = 12)
    
    for (fix_var in fix_vars) {
      # save resid vs fixed effects
      ggsave(paste0(plot_path, "/",
                    time_stamp, "_",
                    "glmm_resid_fix_eff_", fix_var, "_",
                    str_replace(simple_form, "~", "_"),
                    ".png"),
             ggplot(data = data.frame(resid = mod_resid,
                                      fix_var = dat.occ[[fix_var]],
                                      id.grp = dat.occ$id.grp),
                    aes(fix_var, resid, col = id.grp)) +
               geom_point() + 
               geom_hline(yintercept = 0) +
               geom_smooth() +
               labs(title = fix_var,
                    x = fix_var) +
               theme_minimal() +
               theme(panel.grid = element_blank()),
             width = 20, height = 12)
    }
    
    # plot resid against levels of rnd eff
    # TODO: hard code less
    for (rnd_var in rnd_vars) {
      for (rnd_val in unique(dat.occ[[rnd_var]])) {
        select_vec <- which(dat.occ[[rnd_var]] == rnd_val)
        
        ggsave(paste0(plot_path, "/",
                      time_stamp, "_",
                      "glmm_resid_rnd_eff_", rnd_var, "_",
                      "level_", rnd_val,
                      str_replace(simple_form, "~", "_"),
                      ".png"),
               ggplot(data = data.frame(
                 resid = mod_resid[select_vec],
                 rnd_var = dat.occ[[rnd_var]][select_vec]),
                 aes(rnd_var, resid)) +
                   geom_boxplot() + 
                   geom_jitter(alpha = 0.5, width = 0.1) +
                   labs(title = rnd_val,
                        x = rnd_var) +
                   theme_minimal() +
                   theme(panel.grid = element_blank()),
                 width = 20, height = 12)
      }
    }
    
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
