
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

# set number of times a model with failed convergence will attempt to restart
n_restart <- 3

# Directory structure:
#   - timestamp script start
#     - model formulas for script
#     - timestamp (and formula) of model start
#       - model data
#       - plot folder

# set saving paths
if (!test_run) {
  model_root <- "models/"
} else {
  model_root <- "temp_models/"
}

#  ensure path exists
dir.check(model_root)

# set up dir for this script run
script_time_stamp <- format(Sys.time(), format = "%Y%m%d_%H%M")
run_path <- paste0(model_root, script_time_stamp, "_model_data/")
dir.check(run_path)

# set up log file inside script run dir
model_log_file <- paste0(run_path,
                         script_time_stamp,
                         "_models",
                         ".log")

options("log_file" = model_log_file)


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

    log_msg("Test run, pruning data down to species: ",
            paste(frac_species, collapse = ", "))

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
  
  time_stamp <- format(Sys.time(), format = "%Y%m%d_%H%M")
  
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
  
  #   extract all independent vars
  ind_vars <- str_extract(form, "(?<=~).*")
  
  #   extract random variables
  rnd_vars <- str_extract_all(ind_vars, "(?<=\\().*(?=\\))")
  
  has_ranef <- FALSE
  
  # this check if rnd_vars is empty is rather convoluted, but i dont know how 
  #   to improve it
  if (! any(sapply(rnd_vars,
                   function (x) {identical(x, character(0))}
  ))) {
    
    rnd_vars <- str_extract    (rnd_vars, "(?<=\\|\\s?)[\\w\\.]+")
    rnd_vars <- unique(rnd_vars) 
    
    # save whether ranef was found
    has_ranef <- TRUE 
    
  }
  
  #   extract fixed variables
  #   WARNING: Interaction terms will be reduced to just their constituents
  fix_vars <- str_replace_all(ind_vars, "[\\s+]*\\(.*\\)", "")
  fix_vars <- str_replace_all(fix_vars, "\\s", "")
  fix_vars <- str_split(fix_vars, "[*:+-]+")[[1]]
  fix_vars <- unique(fix_vars)
  
  
  # Directory stuff -------------------
  
  mod_path <- paste0(run_path, time_stamp, "_",
                     str_replace(simple_form, "~", "_"), "/")
  
  dir.check(mod_path)
  
  # Model running ---------------------
  
  log_msg("Starting", simple_form, "model...")
  
  # Check if the formula contains random effects, and if so use the right
  #   function (lmer acts up without random effect)
  # WARNING: This is potentially very stupid as lmer and lm return different 
  #   objects
  if (has_ranef) {
    
    # run the model, this step may take a lot of time
    #  although plotting will probably take longer
    lm_mod <- lmer(mod_form, data = dat.occ)
    
  } else {
    
    lm_mod <- lm(mod_form, dat.occ)
    
  }
  
  # print(warnings())
  
  # It is probably not wise to set this bit up extracting stuff from deep within
  #   the model object, but i cant be asked to do it properly right now
  if (any(str_detect(lm_mod@optinfo$conv$lme4$messages,
                     "Model failed to converge"))) {
    
    i <- 1
    
    log_msg("  Convergence failure found!")
    log_msg("  Attempting to restart model fitting with current parameters...")
    
    max_grad_old <- as.numeric(str_extract(lm_mod@optinfo$conv$lme4$messages,
                                       "(?<=max\\|grad\\| = )[0-9\\.]+"))
    max_grad_new <- max_grad_old
    
    while (any(str_detect(lm_mod@optinfo$conv$lme4$messages,
                          "Model failed to converge")) &&
           i <= n_restart) {
      
      log_msg("  ... Attempt ", i, " of ", n_restart, "...")
      
      # TODO Disable testing stuff
      saveRDS(lm_mod,
              file = paste0(mod_path,
                            time_stamp, "_",
                            "lmm_model_CONVFAIL_i_",
                            str_replace(simple_form, "~", "_"),
                            ".rds"))
      
      
      
      if (has_ranef) {
        
        params <- getME(lm_mod, "theta")
        lm_mod <- update(lm_mod, start = params)
        
      } else {
        
        # Not sure whether convergence warnings are a thing with lms
        warning("Convergence failure mitigation not implemented for regular lms!")
        
        break
        
      }
      
      max_grad_old <- max_grad_new
      max_grad_new <- as.numeric(str_extract(lm_mod@optinfo$conv$lme4$messages,
                                              "(?<=max\\|grad\\| = )[0-9\\.]+"))
      
      i <- i + 1
      
    }
    
    if (i == n_restart && max_grad_old <= max_grad_new) {
      log_msg("  ... Out of tries but no improvement, continuing.")
    } else {
      log_msg("  ... Restart sucessfull")
    }
  }
  
  log_msg("... Done.")
  log_msg("Saving model to disk...")
  
  # save model to disk
  saveRDS(lm_mod,
          file = paste0(mod_path,
                        time_stamp, "_",
                        "lmm_model_",
                        str_replace(simple_form, "~", "_"),
                        ".rds"))
  
  log_msg("... Done.")
  
  # Model results extraction ----------
  
  log_msg("Computing summary and saving to disk...")
  
  # save summary output to disk
  capture.output(summary(lm_mod), 
                 file = paste0(mod_path,
                               time_stamp, "_",
                               "lmm_summary_",
                               str_replace(simple_form, "~", "_"),
                               ".txt"))
  
  log_msg("... Done.")
  
  # if a random effect is present, extract the coefficients for it
  #  Currently, the script will not accept models without random effects anyway
  if (has_ranef) {
    
    # TODO: make sure this check for presence of random variable works as 
    #   intended
    
    log_msg("Extracting random effects for", simple_form, "model...")
    
    rnd_eff <- tidy(lm_mod, c("ran_vals"))
    
    fwrite(rnd_eff,
           paste0(mod_path, 
                  time_stamp, "_",
                  "lmm_rnd_eff_",
                  str_replace(simple_form, "~", "_"),
                  ".csv"))
    
    rm(rnd_eff)
    
    log_msg("... Done.")
  }
  
  # Diagnostic plots ------------------
  
  if (plot_diagnostics) {
    
    # generate plotting dir
    plot_path <- paste0(mod_path, "plots/")
    dir.check(plot_path)
    
    log_msg("Extracting plotting data...")
    
    mod_resid <- residuals(lm_mod)
    mod_fitvl <- fitted   (lm_mod)
    
    rm(lm_mod)
    
    log_msg("... Done.")
    
    log_msg("Generating and saving diagnostics plots...")
    
    # save qq plot
    png(paste0(plot_path, "/",
               time_stamp, "_",
               "lmm_qq_",
               str_replace(simple_form, "~", "_"),
               ".png"),
        width = 2000,
        height = 1200
    )
    
    qqnorm(scale(mod_resid), ylab = "Scaled sample quantiles",
           sub = form)
    abline(0, 1)
    
    dev.off()
    
    # TODO: Proper axis labels and titles
    
    # save resid vs fitted
    ggsave(paste0(plot_path, "/",
                  time_stamp, "_",
                  "lmm_resid_fit_",
                  str_replace(simple_form, "~", "_"),
                  ".png"),
           lmResFitPlot(mod_resid, mod_fitvl, dat.occ$id.grp,
                        sub = form),
           width = 20, height = 12)
    
    
    # save resid histogram
    ggsave(paste0(plot_path, "/",
                  time_stamp, "_",
                  "lmm_resid_hist_",
                  str_replace(simple_form, "~", "_"),
                  ".png"),
           ggplot(data = data.frame(resid = mod_resid),
                  aes(resid)) +
             geom_histogram() + 
             labs(sub = form) +
             theme_minimal() +
             theme(panel.grid = element_blank()),
           width = 20, height = 12)
    
    # plot residuals vs each fixed effect
    for (fix_var in fix_vars) {
      
      fix_var_plot <- ggplot(data.frame(resid = mod_resid,
                                        fix_var = dat.occ[[fix_var]],
                                        id.grp = dat.occ$id.grp),
                             aes(fix_var, resid, col = id.grp)) 
      
      if (is.numeric(dat.occ[[fix_var]])){
        fix_var_plot <- fix_var_plot +
          geom_point() + 
          geom_smooth()
      } else {
        fix_var_plot <- fix_var_plot +
          geom_boxplot()
      }
        
        fix_var_plot <- fix_var_plot + 
          geom_hline(yintercept = 0) +
          labs(title = fix_var,
               x = fix_var) +
          labs(sub = form) +
          theme_minimal() +
          theme(panel.grid = element_blank())
        
      # save plot
      ggsave(paste0(plot_path, "/",
                    time_stamp, "_",
                    "lmm_resid_fix_eff_", fix_var, "_",
                    str_replace(simple_form, "~", "_"),
                    ".png"),
             fix_var_plot,
             width = 20, height = 12)
      
    }
    
    
    if (has_ranef) {
      
      # plot resid against levels of rnd eff
      for (rnd_var in rnd_vars) {
        
        n_rnd_var <- uniqueN(dat.occ[[rnd_var]])
        
        ggsave(paste0(plot_path, "/",
                      time_stamp, "_",
                      "lmm_resid_rnd_eff_", rnd_var, "_",
                      str_replace(simple_form, "~", "_"),
                      ".png"),
               ggplot(data = data.frame(
                 resid = mod_resid,
                 rnd_var = dat.occ[[rnd_var]]),
                 aes(rnd_var, resid)) +
                 geom_boxplot() +
                 labs(title = rnd_var,
                      x = rnd_var) +
                 geom_hline(yintercept = 0) +
                 geom_text(data = data.frame(
                   rnd_var = sort(unique(dat.occ[[rnd_var]])),
                   lab = paste0("n = ", table(dat.occ[[rnd_var]])),
                   ypos = ypos(mod_resid)
                 ), 
                 aes(rnd_var, ypos, label = lab)
                 ) +
                 facet_wrap(~ rnd_var, scale = "free_x") +
                 labs(sub = form) +
                 theme_minimal() +
                 theme(panel.grid = element_blank(),
                       axis.text.x = element_blank()),
               width = 3 * sqrt(n_rnd_var), height = 3 * sqrt(n_rnd_var))
        
      }
      
    }
    
    # TODO
    #   give cooks distances
    
    # TODO: check if everything is removed that needs to be
    rm(mod_resid, mod_fitvl)
    
  } else {
    
    # remove model object
    rm(lm_mod)
    
  }
  
  log_msg("Done with", simple_form, "model...")
  
}

log_msg("All done.")

# reset logging file
options("log_file" = NULL)
