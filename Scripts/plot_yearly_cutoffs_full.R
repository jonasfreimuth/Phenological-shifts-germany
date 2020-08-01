library(here)
library(beepr)
library(data.table)
library(tidyverse)

#load data
dat.occ <- fread(
  here("Data", "occurrences_full_pruned.csv"))


for (cutoff.year in seq(1960, 2010, by = 5)) {
  
  png(here("Plots/additional", paste("group_trends_full_", cutoff.year, ".png")), width = 1500, height = 1000)
  
  print(
    
    dat.occ %>%
      # sample_n(10000) %>%
      filter(year >= cutoff.year) %>%
      ggplot() +
      geom_point(aes(x = year, y = doy,
                     col = id.grp), 
                 size = 2,
                 alpha = .2) +
      geom_smooth(aes(x = year, y = doy, col = id.grp, group = id.grp), 
                  method = lm, se = F, size = 1.5, linetype = 1,
                  alpha = .7) +
      ylim(c(0, 365)) +
      xlim(c(1960, 2019)) +
    labs(x = "Year",
         y = "DOY") +
      theme(
        axis.title = element_text(size = 40),
        axis.text = element_text(size = 40),
        axis.ticks = element_line(colour = "gray31", size = 1.2),
        axis.ticks.length.x.bottom = unit(8, "bigpts"),
        axis.line = element_line(colour = "gray31", size = 1.2),
        legend.position = "bottom",
        legend.text = element_text(size = 40),
        legend.title = element_text(size = 40),
        panel.background = element_blank(),
        panel.spacing = unit(32, "bigpts"),
        strip.background = element_blank(),
        strip.text = element_text(size = 40),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()
        )
  )
  
  dev.off()
  
}


