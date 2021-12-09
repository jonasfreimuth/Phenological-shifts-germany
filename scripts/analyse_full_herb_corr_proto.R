

# define datasets used
anls_1 <- "data/only_lfu_bfl_nagu_inat_analysis_data/"
anls_2 <- "data/preserved_specimen_only/"
comp_name <- "onlyVegmapCitsci_onlyPresspec"

# either "inset" or "plant" !NOT "Plants"!
group <- "plant"

base_dir  <- "comp_analyses/"

d_name_1 <- str_split(comp_name, "_")[[1]][1]
d_name_2 <- str_split(comp_name, "_")[[1]][2]

dir_name <- paste0(base_dir, comp_name, "_", group, "/")
dir.check(dir_name)

writeLines(paste("Analysis of differences between rnd eff",
                 "of insects",
                 "\n between datasets in",
                 anls_1, "and", anls_2),
           paste0(dir_name, "info.txt"))

plot_comp <- TRUE

# load data and merge -----------------------------------------------------

tax_col_vec <- c("kingdom",
                 "phylum",
                 "id.grp",
                 "order",
                 "family",
                 "genus",
                 "species")

if (!exists("tax_df")) {
  tax_df <- fread(
    "data/full_analysis_data/occurrences_full_pruned_clim_elev.csv", 
    select = tax_col_vec) %>%
    distinct()
}

# read in first dataset
slopes_year <- fread(paste0(anls_1, "rnd_eff_year.csv"))
slopes_temp <- fread(paste0(anls_1, "rnd_eff_temp.csv"))
slopes_all <- left_join(slopes_temp, slopes_year, by = "species",
                        suffix = c("_temp", "_year")) %>%
  select(-starts_with(c("main_var", "group_"))) %>% 
  left_join(tax_df, by = "species") 

if (group == "plant") {
  slopes_all <- slopes_all %>% 
    filter(id.grp == "Plants")
} else {
  slopes_all <- slopes_all %>% 
    filter(id.grp != "Plants")
}

slopes_all_1 <- slopes_all

# read in second dataset
slopes_year <- fread(paste0(anls_2, "rnd_eff_year.csv"))
slopes_temp <- fread(paste0(anls_2, "rnd_eff_temp.csv"))
slopes_all <- left_join(slopes_temp, slopes_year, by = "species",
                        suffix = c("_temp", "_year")) %>%
  select(-starts_with(c("main_var", "group_"))) %>% 
  left_join(tax_df, by = "species")

if (group == "plant") {
  slopes_all <- slopes_all %>% 
    filter(id.grp == "Plants")
} else {
  slopes_all <- slopes_all %>% 
    filter(id.grp != "Plants")
}

slopes_all_2 <- slopes_all

# combine datasets
slopes_all_both <- inner_join(slopes_all_1, slopes_all_2,
                              by = tax_col_vec,
                              suffix = c("_1", "_2")) 

# get names for each par var comb and each dataset
nam_vec <- names(slopes_all_both)
col_vec <- nam_vec[grepl(x = nam_vec,
                         pattern = "(slope|intercept)_(year|temp)")]

# calculate CIs
for (col in col_vec) {
  se_nam <- str_replace(col, "_", "_std_err_")
  
  par_var_vec <- slopes_all_both[[col]]
  pava_se_vec <- slopes_all_both[[se_nam]] 
  
  ci_min_vec <- par_var_vec - 1.96 * pava_se_vec
  ci_max_vec <- par_var_vec + 1.96 * pava_se_vec
  
  ci_min_nam <- str_replace(col, "_", "_ci_min_")
  ci_max_nam <- str_replace(col, "_", "_ci_max_")
  
  slopes_all_both[[ci_min_nam]] <- ci_min_vec
  slopes_all_both[[ci_max_nam]] <- ci_max_vec
}

# check whether ds1 and ds2 differ in obs
for (var in c("temp", "year")) {
  for (par in c("slope", "intercept")) {
    comp_nm_vec <- nam_vec[grepl(x = nam_vec, paste0(par, "_", var))]
    
    ci_min_nam_vec <- str_replace(comp_nm_vec, "_", "_ci_min_")
    ci_max_nam_vec <- str_replace(comp_nm_vec, "_", "_ci_max_")
    
    ds_1_ci_min <- slopes_all_both[[ci_min_nam_vec[1]]]
    ds_2_ci_min <- slopes_all_both[[ci_min_nam_vec[2]]]
    
    ds_1_ci_max <- slopes_all_both[[ci_max_nam_vec[1]]]
    ds_2_ci_max <- slopes_all_both[[ci_max_nam_vec[2]]]
    
    ds_1_grt <- ds_1_ci_min > ds_2_ci_max
    ds_2_grt <- ds_1_ci_max < ds_2_ci_min
    
    diff_vec <- xor(ds_1_grt, ds_2_grt)
    
    comp_col_nm <- paste0(par, "_", var, "_diff")
    
    slopes_all_both[[comp_col_nm]] <- diff_vec
  }
}

ds_contrasts <- bind_rows(slopes_all_1 %>% mutate(ds = d_name_1),
                          slopes_all_2 %>% mutate(ds = d_name_2)) %>% 
  
  # again ensure we use only species in both data sets
  filter(species %in% unique(slopes_all_both$species))

ds_contrasts_sum <- ds_contrasts %>% 
  group_by(ds) %>% 
  summarise(across(matches("(slope|intercept)_(year|temp)"),
                   list(mean    = mean,
                        std_err = ~ sd(.x) / sqrt(n()),
                        ci_min  = ~ mean(.x) - 1.96 * (sd(.x) / sqrt(n())),
                        ci_max  = ~ mean(.x) + 1.96 * (sd(.x) / sqrt(n())),
                        max     = ~ max(.x))),
            n_spec = n())


# save species (unique *shouldnt* be necessary)
fwrite(slopes_all_both,
       str_glue(dir_name, "{group}_species.txt"))


# Plot and analyse differences --------------------------------------------

if (plot_comp) {
  
  plot_dir <- paste0(dir_name, "plots/")
  dir.check(plot_dir)
  
  for (var in c("temp", "year")) {
    for (par in c("slope", "intercept")) {
      
      # Correlation plot ----------------------------------
      
      plot_data <- slopes_all_both %>% 
        select(id.grp, family, species,
               matches(str_glue("{par}.+{var}"))) %>% 
        
        # sorry rather hacky; replaces variable bits of the names with 
        #  constant bits. Done sequentially as i couldnt figure out
        #  smth else. Note the "unquoted" par on the second go, thats necessary
        #  as there will be a literal "par" there on the 2nd go
        rename_with(.cols = matches(str_glue("{par}.+{var}")),
                    .fn = str_replace_all, pattern = par, "par") %>% 
        rename_with(.cols = matches(str_glue("par.+{var}")),
                    .fn = str_replace_all, pattern = var, "var")
      
      
      # compute percentage of non significantly different species
      perc_nodiff <- (1 - (sum(plot_data$par_var_diff) / nrow(plot_data))) %>% 
        multiply_by(100) %>% 
        round(1)
      
      
      cor_plot <- plot_data %>%
        ggplot(aes(par_var_1, par_var_2,
                   # col = family
        )) +
        geom_errorbar(aes(x = par_var_1,
                          ymin = par_ci_min_var_2,
                          ymax = par_ci_max_var_2,
                          col = par_var_diff),
                      width = 0,
                      alpha = alpha.ln) +
        geom_errorbarh(aes(y = par_var_2,
                           xmin = par_ci_min_var_1,
                           xmax = par_ci_max_var_1,
                           col = par_var_diff),
                       height = 0,
                       alpha = alpha.ln) +
        geom_point(aes(col = par_var_diff)) +
        geom_hline(yintercept = 0, col = col.stc.line) + 
        geom_vline(xintercept = 0, col = col.stc.line) +
        labs(title = str_glue("Correlation of {group} {var}-{par}s\n",
                              comp_name),
             subtitle = paste0("r = ",
                               round(cor(plot_data$par_var_1,
                                         plot_data$par_var_2),
                                     3),
                               ", p = ",
                               round(cor.test(plot_data$par_var_1,
                                              plot_data$par_var_2)$p.val,
                                     3),
                               ", Overlap: ",
                               perc_nodiff),
             x = str_glue("{group} {par} in {d_name_1}",
                          "[days / {var}]", sep = "\n"),
             y = str_glue("{group} {par} in {d_name_2}",
                          "[days / {var}]", sep = "\n"),
             color = "Difference\nsignificant") +
        theme_shifts()
      
      # cor_plot
      
      save_plot(str_glue(plot_dir, "{par}_{var}_cor_plot.png"), cor_plot)
      
      
      # Contrast plot -------------------------------------
      
      plot_contrasts <- ds_contrasts %>% 
        select(id.grp, family, species, ds,
               matches(str_glue("{par}.+{var}"))) %>% 
        
        # sorry rather hacky; replaces variable bits of the names with 
        #  constant bits. Done sequentially as i couldnt figure out
        #  smth else. Note the "unquoted" par on the second go, thats necessary
        #  as there will be a literal "par" there on the 2nd go
        rename_with(.cols = matches(str_glue("{par}.+{var}")),
                    .fn = str_replace_all, pattern = par, "par") %>% 
        rename_with(.cols = matches(str_glue("par.+{var}")),
                    .fn = str_replace_all, pattern = var, "var") %>% 
        
        # add information about species difference from dataset above
        mutate(par_var_diff = rep(plot_data$par_var_diff, 2))
      
      ds_lvls <- unique(plot_contrasts$ds)
      
      if (length(ds_lvls) == 2) {
        par_var_ds1 <- plot_contrasts$par_var[plot_contrasts$ds == ds_lvls[1]]
        par_var_ds2 <- plot_contrasts$par_var[plot_contrasts$ds == ds_lvls[2]]
      } else  {
        stop("Can only compare exactly two datasets")
      }
      
      diff_test <- t.test(par_var_ds1, par_var_ds2,
                          paired = TRUE)
      
      pval <- diff_test$p.value %>% round(3)
      
      plot_contrasts_sum <- ds_contrasts_sum %>% 
        select(ds, n_spec,
               matches(str_glue("{par}.+{var}"))) %>% 
        
        # sorry rather hacky; replaces variable bits of the names with 
        #  constant bits. Done sequentially as i couldnt figure out
        #  smth else. Note the "unquoted" par on the second go, thats necessary
        #  as there will be a literal "par" there on the 2nd go
        rename_with(.cols = matches(str_glue("{par}.+{var}")),
                    .fn = str_replace_all, pattern = par, "par") %>% 
        rename_with(.cols = matches(str_glue("par.+{var}")),
                    .fn = str_replace_all, pattern = var, "var")
      
      contr_plot <- ggplot() +
        geom_boxplot(aes(ds, par_var), plot_contrasts, 
                     alpha = 0.5,
                     col = "gray80") +
        geom_line(aes(ds, par_var, group = species),
                  plot_contrasts,
                  col = col.pt,
                  alpha = alpha.ln) +
        geom_pointrange(aes(ds, par_var,
                            ymax = par_var + 1.96 * par_std_err_var,
                            ymin = par_var - 1.96 * par_std_err_var,
                            col = par_var_diff
        ),
        plot_contrasts, 
        position = "jitter",
        # alpha = 0.5,
        # col = col.pt
        ) +
        geom_point(aes(ds, par_var_mean),
                   plot_contrasts_sum,
                   col = col.pt) +
        geom_errorbar(aes(ds, ymin = par_var_ci_min, ymax = par_var_ci_max),
                      plot_contrasts_sum,
                      col = col.pt) +
        geom_hline(yintercept = 0, col = col.stc.line) +
        geom_text(aes(ds, ypos(par_var_max),
                      label = n_spec),
                  plot_contrasts_sum,) +
        labs(title = str_glue("Comparison between {var}-{par}s of common ",
                              "{group}s\nin datasets {d_name_2} and {d_name_1}"),
             subtitle = str_glue("p = {pval} (Paired t-test)",
                                 ", Overlap: {perc_nodiff}",),
             x = "Datasets",
             y = str_glue("{var}-{par} [days / {var}]"),
             color = "Difference\nsignificant") +
        theme_shifts()
      
      save_plot(str_glue(plot_dir, "{par}_{var}_contr_plot.png"), contr_plot)
    }
  }
}
