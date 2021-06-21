
library("dplyr")
library("tidyr")
library("here")
library("stringr")
library("ggplot2")

source("scripts/functions.R")

# Same for temp -----------------------------------------------------------

# read in corresponding data
mres_old <- read.csv("data/species_reaction_with_temp.csv")
mres_new <- read.csv("data/mtemp_model_cent_rnd_eff_tmb_20210621_1333.csv")

# isolate species present in both data sets
both_spec <- intersect(mres_old$species, mres_new$X)

# prune both datasets down to species in both
mres_old <- mres_old[(mres_old$species %in% both_spec), ] %>% 
  arrange(species)
mres_new <- mres_new[(mres_new$X %in% both_spec), ] %>% 
  arrange(X)


# # adjust data structure of mres_new to roughly match mres_old
# mres_new <- mres_new %>% 
#   pivot_wider(id_cols = c("group_var", "group"),
#               names_from = effect,
#               values_from = c(value, se, lower_2.5, upper_97.5),
#               names_sort = TRUE
#   )

slope_diff <- mres_old$slope - mres_new$temp
intercept_diff <- mres_old$intercept - mres_new$X.Intercept


mres_diff <- data.frame(id.grp = mres_old$id.grp,
                        species = mres_old$species,
                        slope_diff = slope_diff,
                        intercept_diff = intercept_diff) %>% 
  arrange(id.grp)

# save file 
write.csv(mres_diff, "data/model_results_difference_temp_tmb.csv")


# Plot results ------------------------------------------------------------

dir.check(here("plots"))

temp_slope_plt <- ggplot(mres_diff, aes(reorder(species, - slope_diff), slope_diff,
                                        group = id.grp, col = id.grp)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap( ~ id.grp, scales = "free_y") +
  labs(title = "Slope Old - New",
       subtitle = "temp") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank())

# temp_slope_plt

ggsave(temp_slope_plt, filename = "plots/mres_temp_slope_diff_tmb.png")


temp_intercept_plt <- ggplot(mres_diff, aes(reorder(species, - intercept_diff), intercept_diff,
                                            group = id.grp, col = id.grp)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap( ~ id.grp, scales = "free_y") +
  labs(title = "Intercept Old - New",
       subtitle = "temp") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank())
# temp_intercept_plt

ggsave(temp_intercept_plt, filename = "plots/mres_temp_intercept_diff_tmb.png")
