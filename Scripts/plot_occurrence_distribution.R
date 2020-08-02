# Setup; load libraries ---------------------------------------------------

library(here)
library(beepr)
library(data.table)
library(tidyverse)


# define function for checking the existence of a directory
# and if it doesn't create it along with its parents 
dir.check <- function(path) {
  
  # check if dir exists
  xist <- dir.exists(path)
  
  # get parent directory (there might be a better way, right now it takes )
  parent <- str_sub(string = path, end = str_locate(path, "/(?=\\w+$)")[1,2] - 1)
  
  # if yes stop and do nothing
  if(xist) {
    
    
    
  } else if (dir.exists(parent)) { # check if parent exists
    
    # if yes create the directory in the parent
    dir.create(path)
    
  } else { 
    
    # if the parent also doesn't exist, repeat the process
    # with the parent
    dir.check(parent)
    
    # now try it again with the actual path
    dir.check(path)
    
  }
  
}

dir.check(here("plots/additional"))


# define color scheme
col.group = c("Coleoptera" = "chartreuse",
              "Diptera" = "yellow",
              "Hymenoptera" = "orange",
              "Lepidoptera" = "red",
              "Plants" = "darkgreen")

# Load and prune data -----------------------------------------------------

# pruning should go like in get_occurrence_data.R but is done extra here to preserve
# institutions which get taken out there

#load data
dat.occ <- fread(
  here("Data", "occurrences_full_refined.csv")) %>% 
  
  #remove records without determined species
  filter(species != "") %>%
  
  #exclude the current year from further analysis
  filter(year != as.integer(substr(
    Sys.Date(), start = 1, stop = 4
  ))) %>%
  
  #exclude first and last days
  filter(doy != 1, doy != 366, doy != 365)

# get german shapefile for plotting
germany <- raster::getData("GADM", country = "DEU", level = 1)


# Generate plots ----------------------------------------------------------


for (ins in unique(filter(dat.occ,
                          !is.na(decimalLatitude) &
                          !is.na(decimalLongitude))$institutionCode)) {
  
  # plot spatial distribution and save plot
  png(here("Plots/additional",
           paste("occurrences_spatial_distribution_",
                 gsub("[[:punct:]]", "_", ins), ".png", sep = "")),
      width = 3000, height = 3000)
  
  print(
    ggplot() +
      geom_polygon(data = germany,
                   aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.3) +
      # only plot points from the current institution
      # only plot points with coordinates (Duh)
      geom_point(data = filter(dat.occ, institutionCode == ins &
                                 !is.na(decimalLatitude) &
                                 !is.na(decimalLongitude)),
                 aes(x=decimalLongitude, y = decimalLatitude, col = id.grp),
                 alpha = .3,
                 size = 3) + 
      labs(x = "Longitude", y = "Latitude",
           title = ins) +
      # facet_wrap(~institutionCode) + 
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
png(here("Plots/additional",
         paste("occurrences_spatial_distribution_all.png", sep = "")),
    width = 3000, height = 3000)

print(
  ggplot() +
    geom_polygon(data = germany,
                 aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    # only plot points with coordinates (Duh)
    geom_point(data = filter(dat.occ,
                             !is.na(decimalLatitude) &
                               !is.na(decimalLongitude)),
               aes(x=decimalLongitude, y = decimalLatitude, col = id.grp),
               alpha = .3,
               size = 3) + 
    labs(x = "Longitude", y = "Latitude") +
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


# Plot DOY distributions --------------------------------------------------

for (ins in unique(dat.occ$institutionCode)) {
  
  # plot spatial distribution and save plot
  png(here("Plots/additional",
           paste("occurrences_doy_distribution_",
                 gsub("[[:punct:]]", "_", ins), ".png", sep = "")),
      width = 1500, height = 1000)
  
  print(
    ggplot() +
      # only plot points from the current institution
      # only plot points with coordinates (Duh)
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
  png(here("Plots/additional",
           paste("occurrences_nrec_distribution_",
                 gsub("[[:punct:]]", "_", ins), ".png", sep = "")),
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
                         values = col.group) +
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
png(here("Plots/additional",
         paste("occurrences_doy_distribution_overall.png", sep = "")),
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

# insects
lapply(unique(filter(dat.occ, order == "Lepidoptera")$species),
       function(x) {
         
         png(here("Plots/additional", str_glue("doy_dist_Lep_{str_replace(x, ' ', '_')}.png")),
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

beep()



