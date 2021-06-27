# this script requires running both the old analysis as is on the 
# first_submission_analysis branch as well as the random effects from the 
# run_raw_data_models script


# Setup -------------------------------------------------------------------

library("dplyr")
library("tidyr")
library("here")
library("stringr")
library("ggplot2")

source("scripts/functions.R")


# for time ----------------------------------------------------------------


# read in corresponding data
mres_old <- read.csv("data/species_reaction_over_time.csv")
mres_new <- read.csv("data/glmm_rnd_eff_doy_year_20210627_0559.csv")

# isolate species present in both data sets
both_spec <- intersect(mres_old$species, mres_new$species)

# prune both datasets down to species in both
mres_old <- mres_old[(mres_old$species %in% both_spec), ] %>% 
  arrange(species)
mres_new <- mres_new[(mres_new$species %in% both_spec), ] %>% 
  arrange(species)

slope_diff <- mres_old$slope - mres_new$year
intercept_diff <- mres_old$intercept - mres_new$Intercept


mres_diff <- data.frame(id.grp = mres_old$id.grp,
                        species = mres_old$species,
                        slope_diff = slope_diff,
                        intercept_diff = intercept_diff) %>% 
  arrange(id.grp)

# save file 
write.csv(mres_diff, "data/model_results_difference_time.csv")


# Plot results ------------------------------------------------------------

dir.check(here("plots"))

year_slope_plt <- ggplot(mres_diff, aes(reorder(species, - slope_diff),
                                        slope_diff,
                                        group = id.grp, col = id.grp)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap( ~ id.grp, scales = "free_y") +
  labs(title = "Slope Old - New",
       subtitle = "Year",
       x = "Difference in slope",
       y = "Species") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank())
year_slope_plt

ggsave(year_slope_plt, filename = "plots/mres_year_slope_diff.png")

year_intercept_plt <- ggplot(mres_diff, aes(reorder(species, - intercept_diff),
                                            intercept_diff,
                                            group = id.grp, col = id.grp)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap( ~ id.grp, scales = "free_y") +
  labs(title = "Intercept Old - New",
       subtitle = "Year",
       x = "Difference in intercept",
       y = "Species") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank())
year_intercept_plt

ggsave(year_intercept_plt, filename = "plots/mres_year_intercept_diff.png")


# Same for temp -----------------------------------------------------------

# read in corresponding data
mres_old <- read.csv("data/species_reaction_with_temp.csv")
mres_new <- read.csv("data/glmm_rnd_eff_doy_temp_20210627_0306.csv")

# isolate species present in both data sets
both_spec <- intersect(mres_old$species, mres_new$species)

# prune both datasets down to species in both
mres_old <- mres_old[(mres_old$species %in% both_spec), ] %>% 
  arrange(species)
mres_new <- mres_new[(mres_new$species %in% both_spec), ] %>% 
  arrange(species)

slope_diff <- mres_old$slope - mres_new$temp

intercept_diff <- mres_old$intercept - mres_new$Intercept

mres_diff <- data.frame(id.grp = mres_old$id.grp,
                        species = mres_old$species,
                        slope_diff = slope_diff,
                        intercept_diff = intercept_diff) %>% 
  arrange(id.grp)

# save file 
write.csv(mres_diff, "data/model_results_difference_temp.csv")


# Plot results ------------------------------------------------------------

dir.check(here("plots"))

temp_slope_plt <- ggplot(mres_diff, aes(reorder(species, - slope_diff), slope_diff,
                                        group = id.grp, col = id.grp)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap( ~ id.grp, scales = "free_y") +
  labs(title = "Slope Old - New",
       subtitle = "temp",
       x = "Difference in slope",
       y = "Species") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank())

# temp_slope_plt

ggsave(temp_slope_plt, filename = "plots/mres_temp_slope_diff.png")


temp_intercept_plt <- ggplot(mres_diff, aes(reorder(species, - intercept_diff), intercept_diff,
                                            group = id.grp, col = id.grp)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap( ~ id.grp, scales = "free_y") +
  labs(title = "Intercept Old - New",
       subtitle = "temp",
       x = "Difference in intercept",
       y = "Species") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank())
# temp_intercept_plt

ggsave(temp_intercept_plt, filename = "plots/mres_temp_intercept_diff.png")
