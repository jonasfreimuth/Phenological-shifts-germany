
# Setup -------------------------------------------------------------------

library("ggplot2")
library("data.table")
library("dplyr")

# read in preset model formulas
form_vec <- readLines("static_data/model_formulas.txt")

lookup <- fread("data/id_grp_species_lookup.csv")

# plotting -----------------------------------------------------------

# temp
raneff_temp <- fread("data/glmm_rnd_eff_doy_temp_20210627_0306.csv") %>%
  left_join(lookup, by = "species")

temp_forest <- ggplot(raneff_temp,
                      aes(reorder(species, -temp), temp, col = id.grp)) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~ id.grp, scales = "free_y") +
  labs(title = "Random effects of temp on species",
       x = "Species",
       y = "Slope") +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

temp_forest

# TODO: make sure dir exists
ggsave("plots/glmm_rnd_eff_doy_temp_forest.png", temp_forest)


# time
raneff_year <- fread("data/glmm_rnd_eff_doy_year_20210627_0559.csv") %>%
  left_join(lookup, by = "species")

year_forest <- ggplot(raneff_year,
                      aes(reorder(species, -year), year, col = id.grp)) +
  geom_point() + 
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~ id.grp, scales = "free_y") +
  labs(title = "Random effects of year on species",
       x = "Species",
       y = "Slope") +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

year_forest

# TODO: make sure dir exists
ggsave("plots/glmm_rnd_eff_doy_year_forest.png", year_forest)
