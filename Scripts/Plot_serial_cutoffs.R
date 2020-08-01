
#load libraries
library("here")
library("scales")
library("beepr")
library("rgdal")
library("lme4")
library("lmerTest")
library("broom")
library("rlang")
library("multcompView")
library("data.table")
library("tidyverse")

#define function for species plotting
specplot <-  function(data, spec) {
  print(
    ggplot(filter(data, species == spec),
           aes(year, mean.doy)) +
      geom_pointrange(aes(ymin = ci.min.doy, ymax = ci.max.doy)) +
      geom_smooth(method = "lm") +
      theme_minimal()
  )
}

#function for calculating a position above the range of a vector x
ypos <- function(x, frac = 0.3) {
  
  return(max(x) + abs(max(x)) * frac)
  
} 
#define function for calculating cis
ci.min <- function(data, level = 0.975) {
  
  return(mean(data) - sd(data)/sqrt(length(data)) * qt(level, length(data) - 1))
  
}

ci.max <- function(data, level = 0.975) {
  
  return(mean(data) + sd(data)/sqrt(length(data)) * qt(level, length(data) - 1))
  
}

calc.ci <- function(data, level = 0.975) {
  
  ci.min <- ci.min(data, level)
  ci.max <- ci.max(data, level)
  
  cis <- as.data.frame(list(ci.min, ci.max), col.names = c("ci.min", "ci.max"))
  
  return(cis)
  
}


#function for getting individual slopes
slopes <- function(data, formula = mean.doy ~ year,
                   tax_level = species,
                   ..., thr = 1) {
  
  #enquote var names
  tax_level <- enquo(tax_level)
  add_vars <- enquos(...)
  
  #keep taxon info as a string
  taxon <- paste(quo_get_expr(tax_level))
  
  #get column for tax level
  data <- mutate(data, tax_level := !!tax_level)
  
  
  cols <- c("kingdom", "order", "family", "genus",
            "species", "id.grp", "slope", "intercept", "pval", 
            "rsquared", "df", "fstat", "ci.min", "ci.max",
            paste(str_sub(as.character(get_expr(add_vars)), 2), sep = ", "))
  
  
  #create data frame to store statistic values
  stat.out <- setNames(data.frame(matrix(ncol = length(cols),
                                         nrow = length(unique(data$tax_level))),
                                  stringsAsFactors = FALSE),
                       cols)
  
  #take one row out to store values from each loop
  stat.i <- stat.out[1,]
  
  #start loop that runs a lm for each instance of the level and saves statistic values
  for (i in sort(unique(data$tax_level))) {
    
    #get current position in both data frames
    j <- match(i, sort(unique(data$tax_level)))
    
    #create subset for that level
    dat.i <- filter(data, tax_level == i)
    
    #if not enough records for taxlevel are present, skip
    if (nrow(dat.i) < thr) {
      
      next
      
    }
    
    #perform linear model on that subset and save summary
    lm.i <- lm(formula, dat.i)
    sum.lm.i <- summary(lm.i)
    
    #calculate confidence interval of slope
    ci.i <- confint(lm.i, as.character(formula)[3], level = 0.95)
    
    #add summary data
    stat.i <- dat.i %>%
      group_by(kingdom, order, family,
               genus, species, id.grp,
               !!! add_vars) %>%
      summarise(start.year = min(year),
                end.year = max(year),
                span = (max(year) - min(year)) + 1,
                n.year = length(year) )
    
    
    #add statistics data
    stat.i <- stat.i %>%
      mutate(slope = sum.lm.i$coefficients[2,1],
             intercept = sum.lm.i$coefficients[1,1],
             pval = pf(sum.lm.i$fstatistic[1], sum.lm.i$fstatistic[2],
                       sum.lm.i$fstatistic[3], lower.tail=FALSE),
             rsquared = sum.lm.i$r.squared,
             df = sum.lm.i$fstatistic[3],
             fstat = sum.lm.i$fstatistic[1],
             ci.min = ci.i[1,1],
             ci.max = ci.i[1,2]
             
      )
    
    #bind loop data to overall data
    stat.out <- bind_rows(stat.out, stat.i)
    
  }
  
  #remove nas
  stat.out <- drop_na(stat.out, slope)
  
  #correct pvalues for multiple testing
  stat.out$fdr.pval <- p.adjust(stat.out$pval, method="fdr")
  
  #cut uneeded tax levels
  if(taxon == "genus") {
    
    stat.out <- select(stat.out, everything(), -species)
    
  } else if (taxon == "family") {
    
    stat.out <- select(stat.out, everything(), - species, -genus)
    
  } else if (taxon == "order") {
    
    stat.out <- select(stat.out, everything(), -species, -genus, -family)
    
  } else if (taxon == "kingdom") {
    
    stat.out <- select(stat.out, everything(), -species, -genus, -family, -order)
    
  }
  
  
  return(stat.out)
  
}



#load data
dat.occ <- fread(here("Data", "occurrences_full_pruned.csv")) 

#initialise data frame for storing mins and maxs
meta <- data.frame(id.grp = NA,
                   minSlope = NA,
                   maxSlope = NA,
                   meanSlope = NA,
                   nSlope = NA,
                   frac = NA,
                   cutoff = sort(rep(1:15, 9*6))
)

#"for each mean cutoff from 1 to 15"
for (c in seq(from = 01,
              # to = as.integer(readline(prompt = "Highest cutoff:"))
              to = 15
)) {
  
  
  #make mean doy dataset at cutoff c
  dat.occ.c <- dat.occ %>%
    group_by(kingdom, phylum, order, family, genus, id.grp) %>%
    group_by(species, add = TRUE) %>%
    group_by(year, add = TRUE) %>%
    # filter  out years with too few records
    filter(n() >= c) %>%
    summarise(
      mean.doy = mean(doy),
      sd.doy = sd(doy),
      min.doy = min(doy),
      max.doy = max(doy),
      median.doy = median(doy),
      quant25 = quantile(doy, probs = 0.25),
      quant75 = quantile(doy, probs = 0.75),
      n.rec = length(doy)
    ) %>% ungroup()
  
  
  #"look at how different fractions play out"
  for (fract in seq(from = .1,
                    to = .9,
                    by = .1
  )) {
    
    # make sure there are records for at least fract of the years
    thr.year <- round(fract * (max(dat.occ$year) - min(dat.occ$year)))
    
    #exclude species that span less than x years
    dat.occ.fract <- dat.occ.c %>%
      group_by(species) %>%
      filter(length(year) >= thr.year) %>%
      ungroup()
    
    #plot order slopes
    order.slopes <- dat.occ.fract %>%
      ggplot(
        aes(year, mean.doy, col = id.grp)
      ) +
      labs(title = paste("n of records per mean:", c),
           subtitle = paste("Fraction of period covered:", fract)) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme_minimal() +
      # geom_text() +
      # facet_wrap( ~ species) +
      xlim(c(min(dat.occ$year), max(dat.occ$year))) +
      ylim(c(min(dat.occ$doy),max(dat.occ$doy))) +
      theme(axis.title = element_text(size = 40),
            axis.text = element_text(size = 30),
            axis.ticks = element_line(colour = "gray31", size = 1.2),
            axis.ticks.length.y.left = unit(8, "bigpts"),
            axis.ticks.length.x.bottom = unit(8, "bigpts"),
            axis.line = element_line(colour = "gray31", size = 1.2),
            plot.title = element_text(size = 40),
            plot.subtitle = element_text(size = 40),
            legend.position = "bottom",
            legend.text = element_text(size = 40),
            legend.title = element_text(size = 40),
            panel.background = element_blank(),
            panel.grid.major = element_blank() )
    
    
    
    dat.occ.plant <- filter(dat.occ.fract, kingdom == "Plantae")
    dat.occ.poll <- filter(dat.occ.fract, kingdom == "Animalia")
    
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
      mutate(id.grp = as.factor(str_replace_all(id.grp, "Animalia", "Insects overall"))) %>%
      mutate(id.grp = fct_relevel(id.grp,
                                  c("Coleoptera", "Diptera", "Hymenoptera", "Lepidoptera", "Insects overall", "Plants")))
    
    
    #same for raw data
    stat.all.time <- bind_rows(stat.spec.time, mutate(filter(stat.spec.time, kingdom == "Animalia"),
                                                      id.grp = "Insects overall")) %>%
      mutate(id.grp = fct_relevel(id.grp,
                                  c("Coleoptera", "Diptera", "Hymenoptera", "Lepidoptera", "Insects overall", "Plants")))
    
    
    #plot group slopes
    group.slopes <- ggplot() +
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
      # geom_errorbar(data = all.time.meta,
      #               aes(x = id.grp,
      #                   ymin = ci.min, ymax = ci.max,
      #                   col = id.grp),
      #               size = 2
      # ) +
      geom_text(data = all.time.meta,
                aes(
                  x = id.grp,
                  y = ypos(stat.spec.time$slope),
                  label = paste0("N =  ", n.slope)
                ),
                size = 13,
                col = "gray31"
      ) +
      geom_vline(xintercept = 4.5, size = 1.3, lty = 3, col = "gray31") +
      geom_vline(xintercept = 5.5, size = 1.3, lty = 3, col = "gray31") +
      labs(x = "Group", y = "Mean slope [days/year] (\u00B1 95% CI)") +
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
    
    
    png(here("Temp", paste0("cotoff_", c, "_fraction_", fract, ".png")), width = 1500, height = 1500)

    print(
      gridExtra::grid.arrange(order.slopes, group.slopes)
    )

    dev.off()
    
    #write stuff into meta df
    meta_i <- stat.all.time %>%
      filter(id.grp != "Insects overall") %>%
      mutate(id.grp = as.character(id.grp)) %>%
      group_by(id.grp) %>%
      summarise(minSlope = min(slope),
                maxSlope = max(slope),
                meanSlope = mean(slope),
                nSlope = length(slope),
                frac = fract,
                cutoff = c) 
    
    #put meta_i at the bottom most postition in meta with the length of meta_i
    meta[nrow(filter(meta, !is.na(id.grp)))+1:nrow(meta_i),] <- meta_i
    
    # meta[meta$frac == fract & meta$cutoff == c,] <- as.data.frame(meta_i)
    
  }
  
  
  
  # metrics <- dat.occ.c %>%
  #   group_by(id.grp) %>%
  #   summarise(n.spec = uniqueN(species)) %>%
  #   ggplot(
  #     aes(id.grp, n.spec) 
  #   ) + 
  #   geom_col() +
  #   theme(axis.title = element_text(size = 40),
  #         axis.text = element_text(size = 30),
  #         axis.ticks = element_line(colour = "gray31", size = 1.2),
  #         axis.ticks.length.y.left = unit(8, "bigpts"),
  #         axis.ticks.length.x.bottom = unit(8, "bigpts"),
  #         axis.line = element_line(colour = "gray31", size = 1.2),
  #         legend.position = "bottom",
  #         legend.text = element_text(size = 40),
  #         legend.title = element_text(size = 40),
  #         panel.background = element_blank(),
  #         panel.grid.major = element_blank() )
  
  
}

#remove NAs from meta
meta <- drop_na(meta)

#define function for plotting
metric.plot <- function(data = meta, y = meanSlope, title = "Mean slope") {
  
  y <- enquo(y)
  
  ggplot(data,
         aes(frac, !! y, col = id.grp)) +
    geom_point(size = 2) +
    geom_line(size = 2) +
    facet_wrap(~ cutoff) +
    labs(title = paste(title, "by min. # of records per yearly mean DOY"),
         x = "Fraction of period covered at minimum") +
    theme(plot.title = element_text(size = 40),
          plot.margin = unit(c(80, 80, 80, 80), "bigpts"),
          axis.title = element_text(size = 40),
          axis.text = element_text(size = 30),
          axis.ticks = element_line(colour = "gray31", size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = "gray31", size = 1.2),
          strip.text = element_text(colour = "gray31", size = 20),
          strip.background = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_blank() )
}

means <- metric.plot()

mins <- metric.plot(y = minSlope, title = "Min slope")

maxs <- metric.plot(y = maxSlope, title = "Max slope")

nplot <- metric.plot(y = nSlope, title = "n Species")
  
  
  
png(here("Temp", "development_cutoffs.png"), width = 3000, height = 2000)

print(
  
  gridExtra::grid.arrange(means, nplot, mins, maxs)
  
)

dev.off()

beep()
