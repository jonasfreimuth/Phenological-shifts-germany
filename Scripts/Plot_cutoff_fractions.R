

#function for calculating a position above the range of a vector x
ypos <- function(x, frac = 0.3) {
  
  return(max(x) + abs(max(x)) * frac)
  
} 

#function for generating letter labels to indicate differences between
#factor levels as determined by a statistical test
pairdiff <- function(data, formula, test = "Tukey", Letters = LETTERS, threshold = 0.05) {
  
  if (test == "Tukey") {
    
    #find letters for non differeing factor levels
    pairs <- multcompLetters(
      #extract named row of pvalues from Tukey test
      TukeyHSD(aov(data = data, formula = formula))[[paste(formula[3])]][,4],
      Letters = Letters)
    
    return(
      #return only Letters and factor levels
      data.frame(letter = pairs$Letters, level = names(pairs$Letters))
    )
    
  } else if (test == "t.test") { 
    
    #perform test
    test <- t.test(formula, data)
    
    #get results into form accepted by multcomLetters
    spoofvec <- test[["p.value"]]
    names(spoofvec) <- paste(str_extract(names(test[["estimate"]]), "[:alpha:]+$"), collapse = "-")
    
    #find letters for non differeing factor levels
    pairs <- multcompLetters(spoofvec, Letters = Letters)
    
    return(
      #return only Letters and factor levels
      data.frame(letter = pairs$Letters, level = names(pairs$Letters))
    )
    
    
  }
  
  
}

#load data
dat.occ <- fread(here("Data", "occurrences_species_yearly_mean_doy_pruned.csv")) 

for (i in seq(from = .1,
              to = 1,
              by = .1
)) {
  
  # make sure there are records for at least half the years
  thr.year <- round(i * (max(dat.occ$year) - min(dat.occ$year)))
  
  #exclude species that span less than x years
  dat.occ.i <- dat.occ %>%
    group_by(species) %>%
    filter(length(year) >= thr.year) %>%
    ungroup()
  
  dat.occ.plant <- filter(dat.occ.i, kingdom == "Plantae")
  dat.occ.poll <- filter(dat.occ.i, kingdom == "Animalia")
  
  stat.spec.time.plants <- slopes(dat.occ.plant, mean.doy ~ year,
                                  tax_level = species)
  stat.spec.time.polls <- slopes(dat.occ.poll, mean.doy ~ year,
                                  tax_level = species)
  
  stat.spec.time <- bind_rows(stat.spec.time.polls, stat.spec.time.plants)
  
  
  #calculate N, mean and sd of slopes for time
  stat.spec.time.meta <- stat.spec.time %>%
    group_by(kingdom) %>%
    summarise(n.slope = length(slope),
              mean.slope = mean(slope),
              sd.slope = sd(slope),
              ci.min = ci.min(slope),
              ci.max = ci.max(slope),
              var = "Year")
  
  #calculate percentage of significantly and generally advancing species
  advfrac <- stat.spec.time %>%
    group_by(kingdom) %>%
    summarise(n.tot = n()) %>%
    left_join(., 
              stat.spec.time %>%
                filter(pval <= 0.05, slope < 0) %>%
                group_by(kingdom) %>%
                summarise(n.adv = n()),
              by = c("kingdom")) %>%
    left_join(., 
              stat.spec.time %>%
                filter(slope < 0) %>%
                group_by(kingdom) %>%
                summarise(n.adv.gen = n()),
              by = c("kingdom")) %>%
    mutate(advfrac = n.adv/n.tot,
           advfrac.gen = n.adv.gen/n.tot)
  
  #add to data frame
  stat.spec.time.meta$advfrac <- advfrac$advfrac
  stat.spec.time.meta$advfrac.gen <- advfrac$advfrac.gen
  
  
  #calculate N, mean and sd of slopes for time
  stat.group.time.meta <- stat.spec.time %>%
    group_by(id.grp) %>%
    summarise(n.slope = length(slope),
              mean.slope = mean(slope),
              sd.slope = sd(slope),
              ci.min = ci.min(slope),
              ci.max = ci.max(slope),
              var = "Year")
  
  #calculate percentage of significantly and generally advancing species
  advfrac <- stat.spec.time %>%
    group_by(id.grp) %>%
    summarise(n.tot = n()) %>%
    left_join(., 
              stat.spec.time %>%
                filter(pval <= 0.05, slope < 0) %>%
                group_by(id.grp) %>%
                summarise(n.adv = n()),
              by = c("id.grp")) %>%
    left_join(., 
              stat.spec.time %>%
                filter(slope < 0) %>%
                group_by(id.grp) %>%
                summarise(n.adv.gen = n()),
              by = c("id.grp")) %>%
    mutate(advfrac = n.adv/n.tot,
           advfrac.gen = n.adv.gen/n.tot)
  
  #add to data frame
  stat.group.time.meta$advfrac <- advfrac$advfrac
  stat.group.time.meta$advfrac.gen <- advfrac$advfrac.gen
  
  
  all.time.meta <- bind_rows(stat.group.time.meta, rename(stat.spec.time.meta[1,], id.grp = kingdom)) %>%
    mutate(id.grp = as.factor(str_replace_all(id.grp, "Animalia", col.group2$group[5]))) %>%
    mutate(id.grp = fct_relevel(id.grp, col.group2$group))
  
  
  #same for raw data
  stat.all.time <- bind_rows(stat.spec.time, mutate(filter(stat.spec.time, kingdom == "Animalia"),
                                                    id.grp = col.group2$group[5])) %>%
    mutate(id.grp = fct_relevel(id.grp, col.group2$group))
  
  
  png(here("Temp", paste0("group_mean_slopes_time_", i, ".png")), width = 1500, height = 1000)
  
  print(
    #plot for time
    ggplot() +
      geom_hline(yintercept = 0, size = 1.5, lty = 2, col = "gray31") +
      geom_jitter(data = stat.all.time,
                  aes(x = id.grp, y = slope),
                  size = 5,
                  alpha = 0.3,
                  width = 0.3,
                  col = "gray40"
      ) +
      geom_point(data = all.time.meta,
                 aes(x = id.grp, y = mean.slope,
                     col = id.grp, pch = id.grp),
                 size = 8) +
      geom_errorbar(data = all.time.meta,
                    aes(x = id.grp,
                        ymin = ci.min, ymax = ci.max,
                        col = id.grp),
                    size = 2
      ) +
      geom_text(data = all.time.meta,
                aes(
                  x = id.grp,
                  y = ypos(stat.spec.time$slope),
                  label = paste0("N =  ", n.slope)
                ),
                size = 13,
                col = "gray31"
      ) +
      # #add labels for differing factor levels
      # geom_text(
      #   data = pairdiff(filter(stat.all.time, id.grp != col.group2$group[5]), slope ~ id.grp),
      #   aes(
      #     x = level,
      #     y = ypos(stat.spec.time$slope, .1),
      #     label = letter
      #   ),
      #   size = 13,
      #   col = "gray31"
      # ) +
      geom_vline(xintercept = 4.5, size = 1.3, lty = 3, col = "gray31") +
      geom_vline(xintercept = 5.5, size = 1.3, lty = 3, col = "gray31") +
      labs(x = "Group", y = "Mean slope [days/year] (\u00B1 95% CI)") +
      # scale_shape_manual(values = c(19, 19, 19, 19, 18, 19)) +
      # scale_color_manual(name = "Group",
      #                    labels = col.group2$group,
      #                    values = col.group2$colour) +
      theme(
        axis.title = element_text(size = 40),
        axis.text = element_text(size = 40),
        axis.text.x = element_text(
          size = 35,
          angle = 25,
          hjust = 1
        ),
        axis.ticks = element_line(colour = "gray31", size = 1.2),
        axis.ticks.length.y.left = unit(8, "bigpts"),
        axis.ticks.length.x.bottom = unit(8, "bigpts"),
        axis.line = element_line(colour = "gray31", size = 1.2),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(32, "bigpts"),
        strip.background = element_blank(),
        strip.text = element_text(size = 40),
      )
  )
  
  dev.off()
  
}

beep()