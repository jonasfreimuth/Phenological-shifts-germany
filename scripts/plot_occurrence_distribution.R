
library("data.table")
library("ggplot2")
library("tidyr")
library("dplyr")
library("tidyselect")
library("raster")

source("scripts/functions.R")


dir.check("plots/additional")


# define colours
# DO NOT CHANGE ORDER, only append
col.grp <-
  data.frame(
    group = c(
      "Beetles",
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
      "Insect independent"
    ),
    colour = c(
      "#9815db",
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
      "#008a00"
    )
  )

# Load and prune data -----------------------------------------------------

# pruning should go like in get_occurrence_data.R but is done extra here to preserve
# institutions which get taken out there

#load data
dat.occ <- fread(
  "data/f_occurrences_full_pruned_elev.csv") %>% 
  
  drop_na(temp, elev)

# get german shapefile for plotting
germany <- raster::getData("GADM", country = "DEU", level = 1)


# Generate plots ----------------------------------------------------------


for (ins in unique(dat.occ$institutionCode)) {
  
  # plot spatial distribution and save plot
  png(paste0("plots/additional",
             "occurrences_spatial_distribution_",
             gsub("[[:punct:]]", "_", ins), ".png"),
      width = 3000, height = 3000)
  
  print(
    ggplot() +
      geom_polygon(data = germany,
                   aes(x = long, y = lat, group = group)) +
      
      # only plot points from the current institution
      geom_point(data = filter(dat.occ, institutionCode == ins),
                 aes(x=decimalLongitude, y = decimalLatitude, col = id.grp),
                 alpha = .3,
                 size = 3) + 
      labs(x = "Longitude", y = "Latitude",
           title = ins) +
      coord_fixed(ratio = 1.3) +
      scale_color_manual(name = "Group",
                         values = col.grp) +
      theme(
        plot.title = element_text(size = 40),
        plot.subtitle = element_text(size = 35),
        axis.title = element_text(size = 40),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 40),
        legend.text = element_text(size = 35),
        panel.background = element_blank(),
        panel.spacing = unit(32, "bigpts"),
        strip.background = element_blank(),
        strip.text = element_text(size = 30),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()
      )
  )
  
  dev.off()
  
}


# plot spatial distribution of all in one
png(paste0("plots/additional/",
           "occurrences_spatial_distribution_all.png"),
    width = 3000, height = 3000)

print(
  ggplot() +
    geom_polygon(data = germany,
                 aes(x = long, y = lat, group = group)) + 
    # only plot points with coordinates (Duh)
    geom_point(data = filter(dat.occ),
               aes(x=decimalLongitude, y = decimalLatitude, col = id.grp),
               alpha = .3,
               size = 3) + 
    labs(x = "Longitude", y = "Latitude") +
    coord_fixed(ratio = 1.3) +
    scale_color_manual(name = "Group",
                       values = col.grp) +
    facet_wrap(~institutionCode) +
    theme(
      plot.title = element_text(size = 40),
      plot.subtitle = element_text(size = 35),
      axis.title = element_text(size = 40),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 40),
      legend.text = element_text(size = 35),
      panel.background = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 30),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank()
    )
)

dev.off()


# Plot decadal occurrence distributions -----------------------------------

for (dec in sort(unique(dat.occ$decade))) {
  
  # make df with all records up to that decade
  dat.occ.dec <- dat.occ %>% 
    filter(decade <= dec) %>% 
    # exclude records without coordinates
    filter(!is.na(decimalLatitude) &
             !is.na(decimalLongitude))
  
  # plot that onto germany
  png(paste0("plots/additional",
            "occurrences_doy_distribution_decadal_", dec, ".png"),
      width = 1500, height = 1000)
  
  print(
    ggplot() +
      geom_polygon(data = germany,
                   aes(x = long, y = lat, group = group)) + 
      geom_point(data = dat.occ.dec,
                 aes(x = decimalLongitude, y = decimalLatitude,
                     col = institutionCode),
                 alpha = .3,
                 size = 3) + 
      coord_fixed(ratio = 1.3) +
      labs(x = "Longitude", y = "Latitude",
           title = paste("Cumulative occurrences up to decade of", dec)) +
      theme(
        plot.title = element_text(size = 40),
        plot.subtitle = element_text(size = 35),
        axis.title = element_text(size = 40),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 40),
        legend.text = element_text(size = 35),
        panel.background = element_blank(),
        panel.spacing = unit(32, "bigpts"),
        strip.background = element_blank(),
        strip.text = element_text(size = 30),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()
      )
  )
  
  
  dev.off()
  
}


# Plot DOY distributions --------------------------------------------------

for (ins in unique(dat.occ$institutionCode)) {
  
  # plot spatial distribution and save plot
  png(paste0("plots/additional",
            "occurrences_doy_distribution_",
            gsub("[[:punct:]]", "_", ins), ".png"),
      width = 1500, height = 1000)
  
  print(
    ggplot() +
      # only plot points from the current institution
      geom_bar(data = filter(dat.occ, institutionCode == ins),
               aes(x = doy)) + 
      labs(x = "Day of the year", y = "# of records",
           title = ins
      ) +
      xlim(min(dat.occ$doy), max(dat.occ$doy)) +
      # facet_wrap(~institutionCode) + 
      theme(
        plot.title = element_text(size = 40),
        plot.subtitle = element_text(size = 35),
        axis.title = element_text(size = 40),
        axis.text = element_text(size = 30),
        axis.ticks = element_line(colour = "gray31", size = 1.2),
        axis.ticks.length.x.bottom = unit(8, "bigpts"),
        axis.line = element_line(colour = "gray31", size = 1.2),
        legend.position = "bottom",
        legend.title = element_text(size = 40),
        legend.text = element_text(size = 35),
        panel.background = element_blank(),
        panel.spacing = unit(32, "bigpts"),
        strip.background = element_blank(),
        strip.text = element_text(size = 30)
      )
  )
  
  dev.off()
  
  # make dataframe with counts for group per year
  dat.n.rec <- count(filter(dat.occ, institutionCode == ins),
                     id.grp, year)
  
  # plot record number distribution and save
  png(paste0("plots/additional/",
             "occurrences_nrec_distribution_",
             gsub("[[:punct:]]", "_", ins), ".png"),
  width = 1500, height = 1000)
  
  print(
    ggplot() +
      # only plot points from the current institution
      geom_col(data = dat.n.rec,
               aes(x = year, y = n,
                   fill = id.grp, col = id.grp),
               position = "stack") + 
      labs(x = "Year", y = "# of records",
           title = ins
      ) +
      xlim(min(1960), max(dat.occ$year)) +
      # facet_wrap(~institutionCode) + 
      scale_color_manual(name = "Group",
                         aesthetics = c("colour", "fill"),
                         values = col.grp) +
      theme(
        plot.title = element_text(size = 40),
        plot.subtitle = element_text(size = 35),
        axis.title = element_text(size = 40),
        axis.text = element_text(size = 30),
        axis.ticks = element_line(colour = "gray31", size = 1.2),
        axis.ticks.length.x.bottom = unit(8, "bigpts"),
        axis.line = element_line(colour = "gray31", size = 1.2),
        legend.position = "bottom",
        legend.title = element_text(size = 40),
        legend.text = element_text(size = 35),
        panel.background = element_blank(),
        panel.spacing = unit(32, "bigpts"),
        strip.background = element_blank(),
        strip.text = element_text(size = 30)
      )
  )
  
  dev.off()
  
}

# plot doy distribution and save plot
png(paste0("plots/additional/",
           "occurrences_doy_distribution_overall.png"),
    width = 1500, height = 1000)

print(
  ggplot() +
    # only plot points from the current institution
    geom_bar(data = dat.occ,
             aes(x = doy)) + 
    labs(x = "Day of the year", y = "# of records",
         title = "All Institutions"
    ) +
    xlim(min(dat.occ$doy), max(dat.occ$doy)) +
    # facet_wrap(~institutionCode) + 
    theme(
      plot.title = element_text(size = 40),
      plot.subtitle = element_text(size = 35),
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 30),
      axis.ticks = element_line(colour = "gray31", size = 1.2),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = "gray31", size = 1.2),
      legend.position = "bottom",
      legend.title = element_text(size = 40),
      legend.text = element_text(size = 35),
      panel.background = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 30)
    )
)

dev.off()


# Plot species doy distributions ------------------------------------------

# somewhat WIP but Butterflies were the main interest anyway

dir.check("plots/additional/species_doy_dist")

# Butterfly species
# I tried lapply instead of the common for loop here 
# in the hopes that it might be faster but i haven't 
# actually checked whether actually it is. Also 
# invisible is necessary to not show a list of "null device" from dev.off()
invisible(
  lapply(unique(filter(dat.occ, order == "Lepidoptera")$species),
         function(x) {
           
           png(paste0("plots/additional/species_doy_dist/",
                      str_glue("doy_dist_Lep_{str_replace(x, ' ', '_')}.png")),
               height = 1000, width = 1500)
           
           print(
             ggplot(filter(dat.occ, species == x),
                    aes(doy)) + 
               geom_bar() +
               xlim(c(min(dat.occ$doy), max(dat.occ$doy))) +
               theme(
                 plot.title = element_text(size = 40),
                 plot.subtitle = element_text(size = 35),
                 axis.title = element_text(size = 40),
                 axis.text = element_text(size = 30),
                 axis.ticks = element_line(colour = "gray31", size = 1.2),
                 axis.ticks.length.x.bottom = unit(8, "bigpts"),
                 axis.line = element_line(colour = "gray31", size = 1.2),
                 legend.position = "bottom",
                 legend.title = element_text(size = 40),
                 legend.text = element_text(size = 35),
                 panel.background = element_blank(),
                 panel.spacing = unit(32, "bigpts"),
                 strip.background = element_blank(),
                 strip.text = element_text(size = 30) )
           )
           dev.off()
         }
  )
)

dir.check("plots/additional/species_inst")

min_doy <- min(dat.occ$doy)
max_doy <- max(dat.occ$doy)

min_temp <- min(dat.occ$temp)
max_temp <- max(dat.occ$temp)

#  plot species temp distributions and color institutions
for (spec in unique(dat.occ$species)) {
  
  ggsave(paste0("plots/additional/species_inst/",
                gsub("[[:punct:]]", "_", spec), ".png"),
         
         dat.occ %>% 
           filter(species == spec) %>% 
           ggplot(aes(temp, doy, col = institutionCode)) + 
           geom_point()+
           geom_smooth(col = "red", method = "lm") +
           ylim(c(min_doy, max_doy)) +
           xlim(c(min_temp, max_temp)) +
           labs(title = spec,
                subtitle = unique(id.grp)) +
           theme_minimal(),
         
         width = 20, height = 12
  )
  
}

beep()



