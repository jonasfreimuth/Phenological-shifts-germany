

# define datasets used
anls_1 <- "data/lfu_bfl_analysis_data/"
anls_2 <- "data/nagu_inat_analysis_data/"

base_dir  <- "comp_analyses/"
comp_name <- "vegmap_citsci"

d_name_1 <- str_split(comp_name, "_")[[1]][1]
d_name_2 <- str_split(comp_name, "_")[[1]][2]

dir_name <- paste0(base_dir, comp_name, "/")
dir.check(dir_name)

writeLines(paste("Analysis of differences between rnd eff",
                 "of plants",
                 "\n between datasets in",
                 anls_1, "and", anls_2),
           paste0(dir_name, "info.txt"))

plot_comp <- TRUE

# load data and merge -----------------------------------------------------

tax_info <- fread("data/full_analysis_data/occurrences_full_pruned_clim_elev.csv", 
                  select = c("id.grp", "family", "species")) %>% 
  distinct(id.grp, family, species)

slopes_year <- fread(paste0(anls_1, "rnd_eff_year.csv"))
slopes_temp <- fread(paste0(anls_1, "rnd_eff_year.csv"))
slopes_all <- left_join(slopes_temp, slopes_year, by = "species",
                        suffix = c("_temp", "_year")) %>%
  select(-starts_with(c("main_var", "group_"))) %>% 
  left_join(tax_info, by = "species") %>% 
  filter(id.grp == "Plants")
slopes_all_1 <- slopes_all

slopes_year <- fread(paste0(anls_2, "rnd_eff_year.csv"))
slopes_temp <- fread(paste0(anls_2, "rnd_eff_year.csv"))
slopes_all <- left_join(slopes_temp, slopes_year, by = "species",
                        suffix = c("_temp", "_year")) %>%
  select(-starts_with(c("main_var", "group_"))) %>% 
  left_join(tax_info, by = "species") %>% 
  filter(id.grp == "Plants")
slopes_all_2 <- slopes_all

slopes_all_both <- inner_join(slopes_all_1, slopes_all_2,
                              by = c("id.grp", "family", "species"),
                              suffix = c("_1", "_2"))

ds_contrasts <- bind_rows(slopes_all_1 %>% mutate(ds = d_name_1),
                          slopes_all_2 %>% mutate(ds = d_name_2)) %>% 
  filter(species %in% unique(slopes_all_both$species))

ds_contrasts_sum <- ds_contrasts %>% 
  group_by(ds) %>% 
  summarise(across(matches("(slope_(year|temp)|intercept_(year|temp))"),
                   list(mean    = mean,
                        std_err = ~ sd(.x) / sqrt(n()),
                        ci_min  = ~ mean(.x) - 1.96 * (sd(.x) / sqrt(n())),
                        ci_max  = ~ mean(.x) + 1.96 * (sd(.x) / sqrt(n())),
                        max     = max)),
            n_spec = n())


# save plant species (unique *shouldnt* be necessary)
writeLines(unique(slopes_all_both$species),
           paste0(dir_name, "plant_species.txt"))


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
      
      
      cor_plot <- plot_data %>%
        ggplot(aes(par_var_1, par_var_2,
                   # col = family
                   )) +
        geom_point() +
        geom_hline(yintercept = 0, col = col.stc.line) + 
        geom_vline(xintercept = 0, col = col.stc.line) +
        labs(title = paste("Correlation of plant slopes",
                           comp_name,
                           sep = "\n"),
             subtitle = paste0("r = ",
                               round(cor(plot_data$par_var_1,
                                         plot_data$par_var_2),
                                     3),
                               ", p = ",
                               round(cor.test(plot_data$par_var_1,
                                              plot_data$par_var_2)$p.val,
                                     3)),
             x = str_glue("Plant {par} in {d_name_1}",
                       "[days / {var}]", sep = "\n"),
             y = str_glue("Plant {par} in {d_name_2}",
                       "[days / {var}]", sep = "\n")) +
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
                    .fn = str_replace_all, pattern = var, "var")
      
      diff_test <- t.test(as.formula(str_glue("par_var ~ ds")),
                          plot_contrasts)
      
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
        geom_pointrange(aes(ds, par_var,
                            ymax = par_var + 1.96 * par_std_err_var,
                            ymin = par_var - 1.96 * par_std_err_var,
                            col = species),
                        plot_contrasts, 
                        position = "jitter",
                        alpha = 0.3) +
        geom_point(aes(ds, par_var_mean),
                   plot_contrasts_sum) +
        geom_errorbar(aes(ds, ymin = par_var_ci_min, ymax = par_var_ci_max),
                      plot_contrasts_sum) +
        geom_text(aes(ds, ypos(par_var_max),
                      label = n_spec),
                  plot_contrasts_sum) +
        labs(title = str_glue("Comparison between {var}-{par}s of common ",
                              "plants\nin datasets {d_name_2} and {d_name_1}"),
             subtitle = str_glue("p = {pval}"),
             x = "Datasets",
             y = str_glue("{var}-{par} [days / {var}]")) +
        theme_shifts(legend.position = "none")
      
      save_plot(str_glue(plot_dir, "{par}_{var}_contr_plot.png"), contr_plot)
    }
  }
}
