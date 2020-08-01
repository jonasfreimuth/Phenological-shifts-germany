
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

# define function to plot and save
huge_plot <- function(path, width = 18000, height = 12000, 
                      x = year,
                      y = mean.doy,
                      ...,
                      ymin = min.doy,
                      ymax = max.doy,
                      data = dat.occ.plot,
                      title,
                      subtitle,
                      xlab,
                      ylab,
                      xlim = NULL,
                      ylim = c(0, 365),
                      save = TRUE) {
  
  
  x <- enquo(x)
  y <- enquo(y)
  ymin <- enquo(ymin)
  ymax <- enquo(ymax)
  dots <- enquos(...)
  
  if (is.null(xlim)) {
    
    xvec <- select(data, x := !! x)$x
    
    xlim <- c(min(xvec), max(xvec))
    
  }
  
  
  if (save == TRUE) png(path, width = width, height = height)
  
  print(
    ggplot(data,
           aes(!!x, !!y, !!!dots,
               # size = n.rec, 
               lty = pval.time)) +
      labs(title = title,
           subtitle = subtitle,
           x = xlab,
           y = ylab) +
      geom_point(size = size.pt.main) +
      geom_smooth(method = method,
                  size = size.line) +
      geom_point(aes(!!x, !!ymin, !!! dots),
                 size = size.pt.sec, pch = pch.min) +
      geom_smooth(aes(!!x, !!ymin, !!! dots), se = SE,
                  method = method,
                  size = size.line,
                  lty = lty.sec) +
      geom_point(aes(!!x, !!ymax, !!! dots),
                 size = size.pt.sec, pch = pch.max) +
      geom_smooth(aes(!!x, !!ymax, !!! dots), se = SE,
                  method = method,
                  size = size.line,
                  lty = lty.sec) +
      facet_wrap( ~ species) +
      coord_cartesian(xlim = xlim,
                      ylim = ylim) +
      scale_color_discrete(name = "Group") +
      scale_linetype_discrete(guide = F) +
      theme(plot.title = element_text(size = 200),
            plot.subtitle = element_text(size = 150),
            axis.title = element_text(size = 150),
            axis.text = element_text(size = 40),
            axis.ticks = element_line(colour = "gray31", size = 1.2),
            axis.ticks.length.y.left = unit(8, "bigpts"),
            axis.ticks.length.x.bottom = unit(8, "bigpts"),
            strip.background = element_blank(),
            strip.text = element_text(size = 40),
            axis.line = element_line(colour = "gray31", size = 1.2),
            legend.position = "bottom",
            legend.text = element_text(size = 150),
            legend.title = element_text(size = 150),
            panel.background = element_blank(),
            panel.grid.major = element_blank() )
  )
  
  if (save == TRUE) dev.off()
  
}

# define function to plot and save
huge_plot_print <- function(path, width = 18000, height = 12000, 
                            x = year,
                            y = mean.doy,
                            ...,
                            ymin = min.doy,
                            ymax = max.doy,
                            data = dat.occ.plot,
                            title,
                            subtitle,
                            xlab,
                            ylab,
                            xlim = NULL,
                            ylim = c(0, 365),
                            save = TRUE) {
  
  
  x <- enquo(x)
  y <- enquo(y)
  ymin <- enquo(ymin)
  ymax <- enquo(ymax)
  dots <- enquos(...)
  
  if (is.null(xlim)) {
    
    xvec <- select(data, x := !! x)$x
    
    xlim <- c(min(xvec), max(xvec))
    
  }
  
  
  if (save == TRUE) png(path, width = width, height = height)
  
  print(
    ggplot(data,
           aes(!!x, !!y, !!!dots,
               # size = n.rec, 
               lty = pval.time)) +
      labs(title = title,
           subtitle = subtitle,
           x = xlab,
           y = ylab) +
      geom_smooth(method = method,
                  size = size.line) +
      geom_smooth(aes(!!x, !!ymin, !!! dots), se = SE,
                  method = method,
                  size = size.line,
                  lty = lty.sec.print) +
      geom_smooth(aes(!!x, !!ymax, !!! dots), se = SE,
                  method = method,
                  size = size.line,
                  lty = lty.sec.print) +
      facet_wrap( ~ species) +
      coord_cartesian(xlim = xlim,
                      ylim = ylim) +
      scale_color_discrete(name = "Group") +
      scale_linetype_discrete(guide = F) +
      theme(plot.title = element_text(size = 200),
            plot.subtitle = element_text(size = 150),
            axis.title = element_text(size = 150),
            axis.text = element_text(size = 40),
            axis.ticks = element_line(colour = "gray31", size = 1.2),
            axis.ticks.length.y.left = unit(8, "bigpts"),
            axis.ticks.length.x.bottom = unit(8, "bigpts"),
            strip.background = element_blank(),
            strip.text = element_text(size = 40),
            axis.line = element_line(colour = "gray31", size = 1.2),
            legend.position = "bottom",
            legend.text = element_text(size = 150),
            legend.title = element_text(size = 150),
            panel.background = element_blank(),
            panel.grid.major = element_blank() )
  )
  
  if (save == TRUE) dev.off()
  
}


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
          x = y_mean_temp,
          y = mean.doy,
          col = id.grp,
          title = "All species' reaction to temperature by group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Yearly mean temperature [\u00B0C]",
          ylab = "DOY"
)



huge_plot(path = here("Plots", "huge_plant_species_temp.png"),
          x = y_mean_temp,
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
          x = y_mean_temp,
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
          x = y_mean_temp,
          y = mean.doy,
          col = id.grp,
          title = "All species' reaction to temperature by group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Yearly mean temperature [\u00B0C]",
          ylab = "DOY"
)



huge_plot_print(path = here("Plots/print", "huge_plant_species_temp.png"),
          x = y_mean_temp,
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
          x = y_mean_temp,
          y = mean.doy,
          col = order,
          title = "Insects' reaction to temperature by Group",
          subtitle = "Dashed lines denote non significant slopes, triangles are first and last occurrence day",
          xlab = "Yearly mean temperature [\u00B0C]",
          ylab = "DOY",
          data = filter(dat.occ.plot, kingdom == "Animalia")
)
