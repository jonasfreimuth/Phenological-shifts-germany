
# Setup -------------------------------------------------------------------

# make df for plotting by adding pvals of time and temp regressions
dat.occ.plot <- left_join(dat.occ, bind_cols(select(stat.spec.time,
                                                    species,
                                                    slope.time = slope, pval.time = fdr.pval),
                                             select(stat.spec.temp,
                                                    slope.temp = slope, pval.temp = fdr.pval)
),
by="species") %>%
  mutate(species = fct_reorder(as.factor(species), slope.time),
         pval.time = cut(pval.time, breaks = c(0, 0.05, 1), labels = c("p < 0.05", "p > 0.05")),
         pval.temp = cut(pval.temp, breaks = c(0, 0.05, 1), labels = c("p < 0.05", "p > 0.05"))) %>%
  arrange(slope.time)

# set global parameters for plotting
size.pt.main <- 3
size.pt.sec <- 3
size.line <- 1.7
pch.min <- 4
pch.max <- 24
lty.sec <- 3
lty.sec.print <- 2
method <- "lm"
SE <- FALSE

# Functions ---------------------------------------------------------------

# just to be save, run functions script
source(here('scripts', 'functions.R'))



# Plot species over time --------------------------------------------------

huge_plot(path = here("Plots", "huge_all_species.png"),
          x = year,
          y = mean.doy,
          col = id.grp,
          title = "All species' time slopes by Group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Year",
          ylab = "DOY"
)

huge_plot(here("Plots", "huge_plant_species.png"),
          x = year,
          y = mean.doy,
          col = order,
          title = "Plants' time slopes by order",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Year",
          ylab = "DOY",
          data = filter(dat.occ.plot, kingdom == "Plantae")
          )


huge_plot(here("Plots", "huge_insect_species.png"),
          width = 9000, height = 6000,
          x = year,
          y = mean.doy,
          col = order,
          title = "Insects' time slopes by Group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Year",
          ylab = "DOY",
          data = filter(dat.occ.plot, kingdom == "Animalia")
)

# also save print versions

dir.check(here("plots/print"))

huge_plot_print(path = here("Plots/print", "huge_all_species.png"),
          x = year,
          y = mean.doy,
          col = id.grp,
          title = "All species' time slopes by Group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Year",
          ylab = "DOY"
)

huge_plot_print(here("Plots/print", "huge_plant_species.png"),
          x = year,
          y = mean.doy,
          col = order,
          title = "Plants' time slopes by order",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Year",
          ylab = "DOY",
          data = filter(dat.occ.plot, kingdom == "Plantae")
)


huge_plot_print(here("Plots/print", "huge_insect_species.png"),
          width = 9000, height = 6000,
          x = year,
          y = mean.doy,
          col = order,
          title = "Insects' time slopes by Group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Year",
          ylab = "DOY",
          data = filter(dat.occ.plot, kingdom == "Animalia")
)

# Plot species with temperature -------------------------------------------

huge_plot(path = here("Plots", "huge_all_species_temp.png"),
          x = temp,
          y = mean.doy,
          col = id.grp,
          title = "All species' reaction to temperature by group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Yearly mean temperature [\u00B0C]",
          ylab = "DOY"
)



huge_plot(path = here("Plots", "huge_plant_species_temp.png"),
          x = temp,
          y = mean.doy,
          col = order,
          title = "Plants' reaction to temperature by order",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Yearly mean temperature [\u00B0C]",
          ylab = "DOY",
          data = filter(dat.occ.plot, kingdom == "Plantae")
)

huge_plot(path = here("Plots", "huge_insect_species_temp.png"),
          width = 9000, height = 6000,
          x = temp,
          y = mean.doy,
          col = order,
          title = "Insects' reaction to temperature by Group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Yearly mean temperature [\u00B0C]",
          ylab = "DOY",
          data = filter(dat.occ.plot, kingdom == "Animalia")
)

# print versions
huge_plot_print(path = here("Plots/print", "huge_all_species_temp.png"),
          x = temp,
          y = mean.doy,
          col = id.grp,
          title = "All species' reaction to temperature by group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Yearly mean temperature [\u00B0C]",
          ylab = "DOY"
)



huge_plot_print(path = here("Plots/print", "huge_plant_species_temp.png"),
          x = temp,
          y = mean.doy,
          col = order,
          title = "Plants' reaction to temperature by order",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Yearly mean temperature [\u00B0C]",
          ylab = "DOY",
          data = filter(dat.occ.plot, kingdom == "Plantae")
)

huge_plot_print(path = here("Plots/print", "huge_insect_species_temp.png"),
          width = 9000, height = 6000,
          x = temp,
          y = mean.doy,
          col = order,
          title = "Insects' reaction to temperature by Group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Yearly mean temperature [\u00B0C]",
          ylab = "DOY",
          data = filter(dat.occ.plot, kingdom == "Animalia")
)
