
# Setup -------------------------------------------------------------------

# library("broom.mixed")
# library("data.table")
# library("lme4")
# library("stringr")
# library("dplyr")
# library("tidyr")
# library("tidyselect")
# library("ggplot2")
# 
# source("scripts/functions.R")
# 
# col.group.sci <- c(Coleoptera  = "#9815db",
#                  Diptera     = "#f41d0f",
#                  Hymenoptera = "#ffa500",
#                  Lepidoptera = "#4744ff",
#                  Plants      = "#008a00")



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

on.exit(options("log_file" = NULL))


# Data loading ------------------------------------------------------------

# read in data, reduce columns to only those relevant here
select_cols <- c('family', 'species', 'year', 'doy',
                 'decade', 'id.grp', 'long', 'lat', 'temp', 'elev')

log_msg("Loading and centering data...")


dat.occ <- fread(dat_occ_file,
                   select = select_cols,
                   showProgress = FALSE)  %>%
    
    # remove records w/o determined temp, shouldnt be necessary any more
    drop_na(temp, elev)


if (test_run) {
  
  spec_vec <- unique(dat.occ$species)
  
  log_msg("Test run, using data pruned down to species: ",
          paste(spec_vec, collapse = ", "))
  
}

# calulate mean and sd of dat occ (as it is necessary for later rescaling of 
# data) and save it
dat.occ %>%
  
  # save mean and sd of predictors
  summarise(across(tidyselect:::where(is.numeric),
                   list(mean = mean, sd = sd))) %>% 
  
  # record what we did to the predictor variables
  mutate(center = center_preds, scale = scale_preds,
         nrow = length(dat.occ)) %>% 
  
  fwrite(paste0(run_path,
                "data_summary", "_",
                script_time_stamp, ".csv"))


# Center and scale
dat.occ <- dat.occ %>%
  
  # if specified, center and / or scale data
  mutate(across(tidyselect:::where(is.numeric) & !doy, scale,
                center = center_preds, scale = scale_preds))


log_msg("... Done.")


# read in preset model formulas
form_vec <- readLines(mod_form_file)

# ensure we are only dealing with two formulas, one for temp and one for year
if (!(length(form_vec) == 2 &&
    sum(str_detect(form_vec, "temp"), str_detect(form_vec, "year")) == 2)) {
  stop("Fomulas not correctly specified.")
}


# Model loop --------------------------------------------------------------

# loop through all preset models
for (form in form_vec) {
  
  # model setup -----------------------
  
  # get timestamp identifying this model
  time_stamp <- format(Sys.time(), format = "%Y%m%d_%H%M")
  
  # Formula stuff ---------------------
  
  # split model formula into constituents
  mod_comps <- str_split(form, "\\s+", simplify = TRUE)
  
  # stitch formula back together to have the long spaces removed
  form <- paste(mod_comps, collapse = " ")
  
  # extract response and fist independent variable
  simple_form <- mod_comps[, 1:3] %>%
    paste(collapse = "")
  
  # extract main independent var
  main_var <- mod_comps[, 3]
  
  # extract the dependent var
  dep_var <- mod_comps[, 1]
  
  # convert formula string to proper formula
  mod_form <- as.formula(form)
  
  # extract which components are fixed and which are random effects:
  #   TODO: consider effects of minus properly
  
  #   extract all independent vars
  ind_vars <- str_extract(form, "(?<=~).*")
  
  #   extract random variables
  rnd_terms <- str_extract_all(ind_vars, "(?<=\\()[^\\)\\(]*(?=\\))",
                               simplify = TRUE)
  
  has_ranef <- FALSE
  
  # check if there are any random vars
  if (length(rnd_terms) >= 1) {
    
    # extract all the random effect variables
    rnd_vars     <- str_extract_all(rnd_terms, "(?<=\\|\\s?)[\\w\\.]+", 
                                    simplify = TRUE)
    
    # extract the groups those variables are nested in
    rnd_vars_grp <- str_extract_all(rnd_terms, "[\\w\\.]+(?=\\s?\\|)", 
                                    simplify = TRUE)
    
    # make a df compining both
    rnd_var_df <- data.frame(rnd_var = rnd_vars, group = rnd_vars_grp)
    nrow_asis <- nrow(rnd_var_df)
    
    rnd_var_df <- rnd_var_df %>% 
      distinct()
    
    if (nrow_asis != nrow(rnd_var_df)) {
      warning(paste0("Duplication of random effects detected. ",
                     "Currently, this will cause problems down the line..."))
    }
    
    # save that at least one ranef was found
    has_ranef <- TRUE 
    
  }
  
  #   extract fixed variables
  #   WARNING: Interaction terms will be reduced to just their constituents
  fix_vars <- str_replace_all(ind_vars, "[\\s+]*\\(.*\\)", "")
  fix_vars <- str_replace_all(fix_vars, "\\s", "")
  fix_vars <- str_split(fix_vars, "[*:+-]+")[[1]]
  fix_vars <- unique(fix_vars)
  
  
  # Directory stuff -------------------
  
  if (has_ranef) {
    
    mod_path <- paste0(run_path,
                       dep_var, "_vs_",
                       paste(fix_vars, collapse = "_"), "_",
                       paste(str_c(rnd_var_df$group, rnd_var_df$rnd_var,
                                   sep = "-"),
                             collapse = "_"), "_",
                       time_stamp, "/")
    
  } else {
    
    mod_path <- paste0(run_path,
                       dep_var, "_vs_",
                       paste(fix_vars, collapse = "_"), "_",
                       time_stamp, "/")
    
  }
  
  dir.check(mod_path)
  
  # save model formula in folder
  writeLines(form, paste0(mod_path,
                          "model_formula_",
                          time_stamp,
                          ".txt"))
  
  # Model running ---------------------
  
  log_msg("Starting model ", form, "...")
  
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
  
  # if we are dealing with a lmm, check for convergence errors
  if (has_ranef) {
    
    # It is probably not wise to set this bit up extracting stuff from deep 
    #   within the model object, but i cant be asked to do it properly right now
    if (any(str_detect(lm_mod@optinfo$conv$lme4$messages,
                       "Model failed to converge"))) {
      
      i <- 1
      
      log_msg("  Convergence failure found!")
      log_msg("  Attempting to restart model fitting with current parameters...")
      
      while (any(str_detect(lm_mod@optinfo$conv$lme4$messages,
                            "Model failed to converge")) &&
             i <= n_restart) {
        
        log_msg("  ... Attempt ", i, " of ", n_restart, "...")
        
        params <- getME(lm_mod, "theta")
        lm_mod <- update(lm_mod, start = params)
        
        i <- i + 1
        
      }
      
      # log whether restarting succeeded
      if (any(str_detect(lm_mod@optinfo$conv$lme4$messages,
                         "Model failed to converge")) &&
          i >= n_restart) {
        log_msg("  ... Out of tries but no improvement, continuing.")
      } else {
        log_msg("  ... Restart sucessfull")
      }
    } 
  }
  
  log_msg("... Done.")
  log_msg("Saving model to disk...")
  
  # save model to disk
  saveRDS(lm_mod,
          file = paste0(mod_path,
                        "lmm_model_",
                        str_replace(simple_form, "~", "_"), "_",
                        time_stamp,
                        ".rds"))
  
  log_msg("... Done.")
  
  # Model results extraction ----------
  
  log_msg("Computing summary and saving to disk...")
  
  mod_sum  <- summary(lm_mod)
  mod_coef <- mod_sum$coefficients
  
  # save summary output to disk
  capture.output(mod_sum, 
                 file = paste0(mod_path,
                               "lmm_summary_",
                               str_replace(simple_form, "~", "_"), "_",
                               time_stamp,
                               ".txt"))
  
  rm(mod_sum)
  
  log_msg("... Done.")
  
  # if a random effect is present, extract the coefficients for it
  #  Currently, the script will not accept models without random effects anyway
  if (has_ranef) {
    
    # TODO: make sure this check for presence of random variable works as 
    #   intended
    
    log_msg("Extracting random effects for model ", form, "...")
    
    rnd_eff <- tidy(lm_mod, c("ran_vals")) %>% 
      
      pivot_wider(id_cols = c(level, group), names_from = term,
                  values_from = c(estimate, std.error)) %>% 
      mutate(main_var = main_var) 
    
    # hacky way to rename everything based on formulas
    # needs to be coded custom to the way the order of names is
    names(rnd_eff)[1] <- rnd_vars[1]
    names(rnd_eff)[3] <- "intercept"
    names(rnd_eff)[4] <- "slope"
    names(rnd_eff)[5] <- "intercept_std_err"
    names(rnd_eff)[6] <- "slope_std_err"
    
    # add overall slope and intercept to rnd slope and intercept
    rnd_eff$intercept         <- rnd_eff$intercept              + mod_coef[1,1]
    rnd_eff$slope             <- rnd_eff$slope                  + mod_coef[2,1]
    
    # add std.error of overall slope and intercept to rnd slope and intercept
    #   uses error propagation formula found here:
    # https://stats.stackexchange.com/questions/70164/error-propagation-sd-vs-se
    rnd_eff$intercept_std_err <- sqrt(rnd_eff$intercept_std_err ^ 2 +
                                        mod_coef[1,2] ^ 2)
    rnd_eff$slope_std_err     <- sqrt(rnd_eff$slope_std_err     ^ 2 +
                                        mod_coef[2,2] ^ 2)
    
    rm(mod_coef)
    
    fwrite(rnd_eff,
           paste0(mod_path, 
                  "lmm_rnd_eff_",
                  str_replace(simple_form, "~", "_"), "_",
                  time_stamp,
                  ".csv"))
    
    # quick and dirty way to save ran effs as slope data for the main script
    # assumption of only two models must hold for this to work properly
    if (main_var == "year") {
      data_path <- "data/rnd_eff_year"
    } else if (main_var == "temp") {
      data_path <- "data/rnd_eff_temp"
    }
    
    if (test_run) {
      data_path <- paste0(data_path, "_test")
    }
    
    data_path <- paste0(data_path, ".csv")
    
    fwrite(rnd_eff, data_path, showProgress = FALSE)
    
    # plot random effect slopes
    if (plot_rnd_slopes) {
      
      # generate plotting dir
      plot_path <- paste0(mod_path, "plots/")
      dir.check(plot_path)
      
      log_msg("Plotting slopes for random variables...")
      
      for (rnd_var in rnd_vars) {
        
        log_msg("  ... for variable ", rnd_var, "...")
        
        for (group in unique(dat.occ$id.grp)) {
          
          dat.occ.plt <- dat.occ %>% 
            filter(id.grp == group)
          
          rnd_eff_plt <- rnd_eff %>% 
            filter(species %in% dat.occ.plt$species)
          
          n_rnd_var <- uniqueN(dat.occ.plt[[rnd_var]])
          
          png(paste0(plot_path,
                     "lmm_rnd_var_slopes_", rnd_var, "_", group, "_",
                     str_replace(simple_form, "~", "_"), "_",
                     time_stamp,
                     ".png"),
              width  = 250 * ceiling(sqrt(n_rnd_var)),
              height = 250 * ceiling(sqrt(n_rnd_var)))
          
          print(
            ggplot(data = data.frame(dep_var  = dat.occ.plt[[dep_var ]],
                                     main_var = dat.occ.plt[[main_var]],
                                     
                                     # TODO: change this in case col name
                                     #    for rnd_eff is changed
                                     species  = dat.occ.plt[[rnd_var ]],
                                     
                                     # TODO: make this variable
                                     group    = dat.occ.plt[["id.grp"]]),
                   
                   aes(main_var, dep_var, col = group)) +
              
              geom_point() +
              
              # add indication of high density of points
              geom_density2d(col = col.line) +
              
              # add gam curve to check if linear model is actually applicable
              geom_smooth(method = "gam", col = "red") +
              
              # plot model regression lines
              geom_abline(data = rnd_eff_plt,
                          aes(intercept = intercept,
                              slope = slope)) +
              
              labs(title = rnd_var, subtitle = form,
                   x = main_var, y = dep_var) +
              
              # add coloring
              scale_color_manual(name   = "Group",
                                 values = col.group.sci) +
              
              facet_wrap( ~ species) +
              
              theme_minimal()
          )
          
          dev.off()
        }
      }
      
      log_msg("  ... Done.")
    }
    
    rm(rnd_eff)
    
    log_msg("... Done.")
  }
  
  # Diagnostic plots ------------------
  
  if (plot_diagnostics) {
    
    # generate plotting dir
    plot_path <- paste0(mod_path, "plots/")
    dir.check(plot_path)
    
    log_msg("Extracting plotting data...")
    
    
    if (has_ranef) {
      # extraction of raw marginal residuals nicked from the redress package:
      # https://github.com/goodekat/redres/blob/714227ec6fb4821b6977743e38903dd83fb09e8d/R/resid_raw.R
      mod_resid <- lm_mod@resp$y - (lm_mod@pp$X %*% matrix(lm_mod@beta, ncol = 1))
    } else {
      mod_resid <- residuals(lm_mod)
    }
    mod_fitvl <- fitted(lm_mod)
    
    rm(lm_mod)
    
    log_msg("... Done.")
    
    log_msg("Generating and saving diagnostics plots...")
    
    # save qq plot
    png(paste0(plot_path,
               "lmm_resid_qq_",
               str_replace(simple_form, "~", "_"), "_",
               time_stamp,
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
    lm_res_fit_plot <- lmResFitPlot(mod_resid = mod_resid, mod_fit = mod_fitvl,
                                    col_vec = dat.occ$id.grp,
                                    main = "Residuals vs Fitted",
                                    sub = form) +
      
      # add coloring
      scale_color_manual(name   = "Group",
                         values = col.group.sci)
    
    ggsave(paste0(plot_path,
                  "lmm_resid_fit_",
                  str_replace(simple_form, "~", "_"), "_",
                  time_stamp,
                  ".png"),
           lm_res_fit_plot,
           width = 20, height = 12)
    
    if (plot_diagnostics_facet) {
      
      ggsave(paste0(plot_path,
                    "lmm_resid_fit_facet_",
                    str_replace(simple_form, "~", "_"), "_",
                    time_stamp,
                    ".png"),
             
             lm_res_fit_plot +
               
               # add indication of high density of points
               geom_density2d(col = col.line) +
               
               # add red regression curve for better visibility
               geom_smooth(col = "red") +
               
               facet_wrap( ~ cols ),
             
             width = 20, height = 12)
      
    }
    
    rm(lm_res_fit_plot)
    
    # save resid histogram
    ggsave(paste0(plot_path,
                  "lmm_resid_hist_",
                  str_replace(simple_form, "~", "_"), "_",
                  time_stamp,
                  ".png"),
           ggplot(data = data.frame(resid = mod_resid),
                  aes(resid)) +
             geom_histogram(aes(y = ..density..)) + 
             geom_density(group = "Data density") +  
             stat_function(fun = dnorm,
                           args = list(mean = mean(mod_resid),
                                       sd = sd(mod_resid)),
                           col = "red", group = "Normal distribution") +
             labs(title = "Historgram of residuals", subtitle = form,
                  xlab = "Residuals",
                  ylab = "Density") +
             scale_color_manual(name   = "Group",
                                values = col.group.sci) +
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
             subtitle = form,
             x = toupper(fix_var),
             y = "Residuals") +
        scale_color_manual(name   = "Group",
                           values = col.group.sci) +
        theme_minimal() +
        theme(panel.grid = element_blank())
      
      # save plot
      ggsave(paste0(plot_path,
                    "lmm_resid_fix_eff_", fix_var, "_",
                    str_replace(simple_form, "~", "_"), "_",
                    time_stamp,
                    ".png"),
             fix_var_plot,
             width = 20, height = 12)
      
      
      if (plot_diagnostics_facet && is.numeric(dat.occ[[fix_var]])) {
        
        ggsave(paste0(plot_path,
                      "lmm_resid_fix_eff_", fix_var, "_facet_",
                      str_replace(simple_form, "~", "_"), "_",
                      time_stamp,
                      ".png"),
               
               fix_var_plot + 
                 
                 # add indication of high density of points
                 geom_density2d(col = col.line) +
                 
                 # add red regression curve for better visibility
                 geom_smooth(col = "red") +
                 
                 facet_wrap( ~id.grp ),
               
               width = 20, height = 12)
        
      }
      
      rm(fix_var_plot)
    }
    
    if (has_ranef) {
      
      # plot resid against levels of rnd eff
      for (rnd_var in rnd_vars) {
        
        n_rnd_var <- uniqueN(dat.occ[[rnd_var]])
        
        png(paste0(plot_path,
                   "lmm_resid_rnd_eff_", rnd_var, "_",
                   str_replace(simple_form, "~", "_"),
                   "_", 
                   time_stamp,
                   ".png"),
            width  = 250 * ceiling(sqrt(n_rnd_var)),
            height = 250 * ceiling(sqrt(n_rnd_var)))
        
        print(
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
              ypos = ypos(mod_resid)), 
              aes(rnd_var, ypos, label = lab)) +
            facet_wrap(~ rnd_var, scale = "free_x") +
            labs(title = toupper(rnd_var),
                 subtitle = form,
                 xlab = toupper(rnd_var),
                 ylab = "Residuals") +
            scale_color_manual(name   = "Group",
                               values = col.group.sci) +
            theme_minimal() +
            theme(panel.grid = element_blank(),
                  axis.text.x = element_blank())
        )
        
        dev.off()
        
      }
      
    }
    
    # TODO
    #   give cooks distances
    
    # TODO: check if everything is removed that needs to be
    rm(mod_resid, mod_fitvl)
    
  } else {
    
    # remove stuff kept for potential plotting
    rm(lm_mod)
    
  }
  
  log_msg("Done with model ", form, "...")
  
}

rm(dat.occ)

log_msg("All done.")

# reset logging file
options("log_file" = NULL)

# explicitly print warnings
# necessary as warnings would not be displayed if there were too many
warnings()
