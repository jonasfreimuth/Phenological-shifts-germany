# library("ggplot2")
# library("stringr")
# library("data.table")
# library("here")
# library("parallel")
# library("glmmTMB")
# library("DHARMa")
# library("dplyr")
# library("tidyr")
library("broom.mixed")

# ---- To be run from inside run_models.R ----

plotModelCoefs <- function(glm_mod, data) {
  mod_sum <- tidy(glm_mod, c("ran_vals"))
  
  ggplot() + geom_point(data = data, aes(temp, doy, group = species)) +
    geom_abline(data = sp_coefs, aes(
      slope = temp,
      intercept = `(Intercept)`,
      group = species
    ),
    col = "red") +
    facet_wrap( ~ species, scales = "free_y") +
    geom_smooth(data = data,
                aes(temp, doy),
                method = "lm",
                col = "blue") +
    theme_minimal() +
    theme(legend.position = "none")
}
