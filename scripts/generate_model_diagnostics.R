library("data.table")
library("rgbif")
library("raster")
library("broom.mixed")
library("lme4")

# tidyverse stuff last to avoid masking these functions
library("magrittr")
library("forcats")
library("stringr")
library("ggplot2")
library("tidyr")
library("dplyr")
library("tidyselect")

## Functions ----------------------------------------------

source("scripts/functions.R")


test_run <- TRUE

## Modeling

# force running of model generation script
force.models           <- TRUE

# how to treat predictor variables
center_preds           <- FALSE
scale_preds            <- FALSE

# save diagnostics plots
# will take a very long time on the full dataset
plot_diagnostics       <- TRUE

# additionally save facetted plots of diagnostic plots 
#   will have no effect if plot_diagnostics == FALSE
plot_diagnostics_facet <- TRUE

# set number of times a model with failed convergence will attempt to restart
n_restart               <- 4



## Set graphics parameters --------------------------------

# set colour for raw data points
col.pt   <- "gray31"

# set colour for static lines
col.stc.line <- "gray31"

# set colour for axis elements
col.ax   <- "gray31"

# set col for annotations
col.note <- "gray31"

# ribbon alpha
alpha.ribbon            <- 0.3

# annotation text size
annot_text              <- 20

# line types
lty.sec                 <- 3

# line size
hline_size_large        <- 2
line_size_large         <- 2.5

# text size
text_size_large         <- 50
legend_text_size_large  <- 40

# axis parameters
axis_size_large         <- 2
axis_ticks_size_large   <- 2
axis_ticks_length_large <- 16

# size of plot labels for cowplot
# when plots were optimized for 22 x 14 in
label_size              <- 50


col.grp <-
  data.frame(group  = c("Beetles",
                        "Flies",
                        "Bees",
                        "Butterflies/\nMoths",
                        "Insects overall",
                        "Plants",
                        "Hoverfly - Plant",
                        "Bee - Plant",
                        "Butterfly - Plant",
                        "Overall",
                        "Insect dependent",
                        "Intermediate",
                        "Insect independent"),
             
             colour = c("#9815db",
                        "#f41d0f",
                        "#ffa500",
                        "#4744ff",
                        "gold",
                        "#008a00",
                        "#f41d0f",
                        "#ffa500",
                        "#4744ff",
                        "deepskyblue",
                        "red",
                        "#ffa500",
                        "#008a00")
  )

# alternative: named vector
col.grp.vec        <- col.grp$colour
names(col.grp.vec) <- col.grp$group

#set colors for Plant-Pollinator comparisons
col.plapoll        <- col.grp[5:6,]

# static polls group (no exclusion)
col.poll.stc       <- col.grp[1:4,]

#only for polls
col.poll           <- col.grp[1:4,] 

#only for plants
col.plant          <- col.grp[6,]

#set colors for group comparisons (add plant group at the end)
col.group.stc      <- bind_rows(col.poll.stc, col.plant)
col.group          <- bind_rows(col.poll, col.plant)

#set colors for group comparisons including overall
col.group2.stc     <- bind_rows(col.poll.stc, col.plapoll)
col.group2         <- bind_rows(col.poll, col.plapoll)

#set colors for PollDep comparisons
col.PollDep        <- col.grp[11:13,]

# interaction colors with standard names
col.int.stc        <- col.grp[7:9,]
col.int.stc$group  <- c("Hoverfly", "Bee", "Butterfly")

#set colors for Interaction comparisons
col.int            <- col.grp[7:9,]

#set colors for Interaction comparison with overall group
col.int2           <- col.grp[7:10,]

#set colors for Interaction group comparisons
col.int3           <- bind_rows(col.int, col.plant)

# colours for id.grps with scientific scheme
# TODO: Apply colour scheme throughout notebook
col.group.sci <- col.grp$colour[c(1:4, 6)]
names(col.group.sci) <- c("Coleoptera",
                          "Diptera",
                          "Hymenoptera",
                          "Lepidoptera",
                          "Plants")

# set max number of facets for plots with many facets
# batches of 49 each, in order to be optimally legible
batch_size <- 49


if (! test_run) {
  
  
  mod_vec <- c(
    "models/20210912_1844_model_data/doy_vs_temp_lat_long_elev_temp-species_20210912_1844/lmm_model_doy_temp_20210912_1844.rds",
    "models/20210912_1844_model_data/doy_vs_year_lat_long_elev_year-species_20210912_2010/lmm_model_doy_year_20210912_2010.rds"
    )
  
} else {
  
  mod_vec <- "temp_models/20211108_1728_model_data/doy_vs_temp_lat_long_elev_temp-species_20211108_1728/lmm_model_doy_temp_20211108_1728.rds"

}

if (!test_run) {
  dat_occ_file        <- "data/occurrences_full_pruned_clim_elev.csv"
} else {
  dat_occ_file        <- "data/occurrences_full_pruned_clim_elev_test.csv"
}

# read in data, reduce columns to only those relevant here
select_cols <- c('family', 'species', 'year', 'doy',
                 'decade', 'id.grp', 'long', 'lat', 'temp', 'elev')

dat.occ <- dat.occ <- fread(dat_occ_file,
                              select = select_cols,
                              showProgress = FALSE)  %>%
    
    # remove records w/o determined temp, shouldnt be necessary any more
    drop_na(temp, elev) %>%
    
    # if specified, center and / or scale data
    mutate(across(tidyselect:::where(is.numeric) & !doy, scale,
                  center = center_preds, scale = scale_preds))

for (mod_file in mod_vec) {
  
  lm_mod <- readRDS(mod_file)
  
  run_path <- str_extract(mod_file, "[^/]+/[^/]+/")
  
  form <- str_extract(toString(lm_mod@call), "(?<=lmer, ).[^,]+")
  
  # model setup -----------------------
  
  # get timestamp identifying this model
  time_stamp <- str_extract(mod_file, "[0-9]{8}_[0-9]{4}(?=.rds)")
  
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
  
  mod_path <- paste0(run_path,
                     dep_var, "_vs_",
                     paste(fix_vars, collapse = "_"), "_",
                     paste(str_c(rnd_var_df$group, rnd_var_df$rnd_var,
                                 sep = "-"),
                           collapse = "_"), "_",
                     time_stamp, "/")
  
  dir.check(mod_path)
  
  # Model running ---------------------
  
  log_msg("Starting model ", form, "...")
  
  # Diagnostic plots ------------------
  
  if (plot_diagnostics) {
    
    # generate plotting dir
    plot_path <- paste0(mod_path, "plots/")
    dir.check(plot_path)
    
    log_msg("Extracting plotting data...")
    
    # extraction of raw marginal residuals nicked from the redress package:
    # https://github.com/goodekat/redres/blob/714227ec6fb4821b6977743e38903dd83fb09e8d/R/resid_raw.R
    mod_resid <- lm_mod@resp$y - (lm_mod@pp$X %*% matrix(lm_mod@beta, ncol = 1))
    # mod_resid <- residuals(lm_mod)
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
        width = 25,
        height = 15,
        units = "cm",
        res = 300
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
    
    if (plot_diagnostics){
      
      ggsave(paste0(plot_path,
                    "lmm_resid_fit_",
                    str_replace(simple_form, "~", "_"), "_",
                    time_stamp,
                    ".png"),
             lm_res_fit_plot,
             width = 25,
             height = 15,
             units = "cm")
      
    }
    
    if (plot_diagnostics_facet) {
      
      ggsave(paste0(plot_path,
                    "lmm_resid_fit_facet_",
                    str_replace(simple_form, "~", "_"), "_",
                    time_stamp,
                    ".png"),
             
             lm_res_fit_plot +
               
               # add indication of high density of points
               geom_density2d(col = col.stc.line) +
               
               # add red regression curve for better visibility
               geom_smooth(col = "red") +
               
               facet_wrap( ~ cols ),
             
             width = 25,
             height = 15,
             units = "cm")
      
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
           
           width = 25,
           height = 15,
           units = "cm")
    
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
             x = fix_var,
             y = "Residuals") +
        scale_color_manual(name   = "Group",
                           values = col.group.sci) +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom")
      
      # if we don't plot normal diagnostics but this plot would not be saved 
      # under faceting, save it anyways
      if (plot_diagnostics || !(is.numeric(dat.occ[[fix_var]]))) {
        
        # save plot
        ggsave(paste0(plot_path,
                      "lmm_resid_fix_eff_", fix_var, "_",
                      str_replace(simple_form, "~", "_"), "_",
                      time_stamp,
                      ".png"),
               
               fix_var_plot,
               
               width = 25,
               height = 15,
               units = "cm")
        
      }
      
      if (plot_diagnostics_facet && is.numeric(dat.occ[[fix_var]])) {
        
        ggsave(paste0(plot_path,
                      "lmm_resid_fix_eff_", fix_var, "_facet_",
                      str_replace(simple_form, "~", "_"), "_",
                      time_stamp,
                      ".png"),
               
               fix_var_plot + 
                 
                 # add indication of high density of points
                 geom_density2d(col = col.stc.line) +
                 
                 # add red regression curve for better visibility
                 geom_smooth(col = "red") +
                 
                 facet_wrap( ~id.grp ),
               
               width = 25,
               height = 15,
               units = "cm")
        
      }
      
      rm(fix_var_plot)
    }
    
    if (has_ranef) {
      
      # plot resid against levels of rnd eff
      for (rnd_var in rnd_vars) {
        
        rnd_var_lvl <- unique(dat.occ[[rnd_var]])
        n_rnd_var   <- length(rnd_var_lvl)
        
        for (i in 1:(ceiling(n_rnd_var / batch_size))) {
          
          # get selection vector of current levels of the rnd var
          ind_vec <- (((i - 1) * batch_size) + 1):(i * batch_size)
          plot_rnd_val_lvl <- rnd_var_lvl[ind_vec]
          sel_vec <- dat.occ[[rnd_var]] %in% plot_rnd_val_lvl
          
          # make df using these lvls and their residuals
          mod_resid_plot <- mod_resid         [sel_vec]
          rnd_var_vec    <- dat.occ[[rnd_var]][sel_vec]
          plot_df <- data.frame(resid   = mod_resid_plot,
                                rnd_var = rnd_var_vec)
          
          ggsave(paste0(plot_path,
                        "lmm_resid_rnd_eff_", rnd_var, "_",
                        str_replace(simple_form, "~", "_"),
                        "_", time_stamp, 
                        "_", str_pad(i,
                                     ceiling(log10(n_rnd_var / batch_size)),
                                     "left",
                                     "0"),
                        ".png"),
                 
                 ggplot(data = plot_df,
                        aes(rnd_var, resid)) +
                   geom_boxplot() +
                   labs(title = rnd_var,
                        x = rnd_var) +
                   geom_hline(yintercept = 0) +
                   geom_text(data = data.frame(
                     rnd_var = sort(plot_rnd_val_lvl),
                     lab = paste0("n = ", table(rnd_var_vec)),
                     ypos = ypos(mod_resid_plot)), 
                     aes(rnd_var, ypos, label = lab)) +
                   facet_wrap(~ rnd_var, scale = "free_x") +
                   labs(title = rnd_var,
                        subtitle = form,
                        xlab = rnd_var,
                        ylab = "Residuals") +
                   scale_color_manual(name   = "Group",
                                      values = col.group.sci) +
                   theme_minimal() +
                   theme(panel.grid = element_blank(),
                         axis.text.x = element_blank()),
                 
                 width  = 25,
                 height = 35,
                 units  = "cm")
        }
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