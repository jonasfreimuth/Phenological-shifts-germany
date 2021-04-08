# functions to be loaded from main script


# Main script functions ---------------------------------------------------

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

# define function for checking the existence of a directory
# and if it doesn't create it along with its parents 
dir.check <- function(path) {
  
  # check if dir exists
  xist <- dir.exists(path)
  
  # get parent directory (there might be a better way, right now it takes )
  parent <- str_sub(string = path,
                    end = str_locate(path, "/(?=\\w+$)")[1,2] - 1)
  
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


# function for calculating a position above the range of a vector x
# muliple vectors can be supplied, again, the max result of all vectors
# is returned
ypos <- function(x, ..., frac = 0.3) {
  
  x <- c(x, ...)
  
  pos <- max(x, na.rm = TRUE) + abs(max(x, na.rm = TRUE)) * frac
  
  return(pos)
  
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

# define function for getting the x intercept of a linear equation
xintercpt <- function(yintercept, slope) {
  
  # calculate x intercept (y = 0)
  xintercept <- -(yintercept/slope)
  
  return(xintercept)
  
}


# function for generating a data frame of linear model estimates and pvals for all levels
# of a given data frame and a given variable
lm.sum <- function(data = dat.occ, col = id.grp, formula = mean.doy ~ year) {
  
  # make quosure
  col <- enquo(col)
  
  # rename data frame cols to fit the internal logic
  data <- data %>%
    mutate(col := !! col)
  
  # get unique levels of variable
  levels <- unique(as.character(data$col))
  
  # for every level, run a linear model and save the relevant values
  lm.list <- lapply(levels, function(x){
    
    data <- filter(data, !! col == x)
    
    sum.mod <- summary(lm(formula, data))
    
    out <- data.frame(col = x,
                      intercept = coef(sum.mod)[1],
                      slope = coef(sum.mod)[2],
                      pval = as.data.frame(sum.mod[["coefficients"]])$'Pr(>|t|)'[2])
    
    # rename col column to the name from data
    names(out)[1] <- toString(quo_get_expr(col))
    
    return(out)
    
  }
  
  ) 
  
  # generate data frame from lapply list and adjust pvalues with 
  # fdr
  out <- bind_rows(lm.list) %>%
    mutate(pval = p.adjust(pval, method = "fdr"))
  
  return(out)
  
}

#function for getting individual slopes
slopes <- function(data, formula = mean.doy ~ year,
                   tax_level = species,
                   ..., thr = 1,
                   time_var = year) {
  
  #enquote var names
  tax_level <- enquo(tax_level)
  time_var <- enquo(time_var)
  add_vars <- enquos(...)
  
  #keep taxon info as a string
  taxon <- paste(quo_get_expr(tax_level))
  
  #get column for tax level
  data <- mutate(data, tax_level := !! tax_level, time := !! time_var)
  
  
  cols <- c("kingdom", "order", "family", "genus",
            "species", "id.grp", "slope", "intercept", "pval", 
            "rsquared", "df", "fstat", "ci.min", "ci.max",
            paste(str_sub(as.character(get_expr(add_vars)), 2), sep = ", "))
  
  #select summary vars according to tax level
  # there's porbably a better way to do this but i can't be asked right now
  if(taxon == "species") {
    
    #make a list of col names
    sum_vars <- syms(
      list(
        "kingdom",
        "phylum",
        "order",
        "id.grp",
        "family",
        "genus",
        "species"
      )
    )
    
    
  } else if (taxon == "genus") {
    
    #make a list of col names
    sum_vars <- syms(
      list(
        "kingdom",
        "phylum",
        "order",
        "id.grp",
        "family",
        "genus"
      )
    )
    
    #add the additional vars to the list
    sum_vars <-  c(sum_vars, lapply(add_vars, quo_get_expr))
    
  } else if (taxon == "family") {
    
    #make a list of col names
    sum_vars <- syms(
      list(
        "kingdom",
        "phylum",
        "order",
        "id.grp",
        "family"
      )
    )
    
  } else if (taxon == "order") {
    
    #make a list of col names
    sum_vars <- syms(
      list(
        "kingdom",
        "phylum",
        "order",
        "id.grp"
      )
    )
    
  } else if (taxon == "phylum") {
    
    #make a list of col names
    sum_vars <- syms(
      list(
        "kingdom",
        "phylum"
      )
    )
    
  } else if (taxon == "kingdom") {
    
    #make a list of col names
    sum_vars <- syms(
      list(
        "kingdom"
      )
    )
    
  }
  
  # add the additional vars to the list
  sum_vars <-  c(sum_vars, lapply(add_vars, quo_get_expr))
  
  stat.out <- lapply(unique(data$tax_level), function(x) {
    
    data <- filter(data, tax_level == x)
    
    #if not enough records for taxlevel are present, skip
    if (nrow(data) < thr) {
      
      return(NULL)
      
    }
    
    mod <- lm(formula, data)
    sum.mod <- summary(mod)
    
    # calculate confidence interval of slope
    ci.mod <- confint(mod, as.character(formula)[3], level = 0.95)
    
    #add summary data
    out <- data %>%
      group_by(!!! sum_vars) %>%
      summarise(start = min(time),
                end = max(time),
                span = (max(time) - min(time)) + 1,
                n = length(time),
                mean.doy = mean(mean.doy)) %>%
      ungroup()
    
    #add statistics data
    out <- out %>%
      mutate(slope = sum.mod$coefficients[2,1],
             intercept = sum.mod$coefficients[1,1],
             pval = pf(sum.mod$fstatistic[1], sum.mod$fstatistic[2],
                       sum.mod$fstatistic[3], lower.tail=FALSE),
             rsquared = sum.mod$r.squared,
             df = sum.mod$fstatistic[3],
             fstat = sum.mod$fstatistic[1],
             ci.min = ci.mod[1,1],
             ci.max = ci.mod[1,2]
             
      )
    
    return(out)
    
    
  })
  
  # convert list to data frame
  stat.out <- bind_rows(stat.out) %>%
    drop_na(slope)
  
  #correct pvalues for multiple testing
  stat.out$fdr.pval <- p.adjust(stat.out$pval, method="fdr")
  
  return(stat.out)
  
}

#function for calculating meta stats for slope differences between traits
trait <- function(data = dat.occ.plant, var, trait_name, min_spec = 2) {
  
  var <- enquo(var)
  trait_name <- enquo(trait_name)
  
  #turn var_names into col names
  data <- mutate(data, trait := !! trait_name, var := !! var)
  
  #calculate slopes 
  stat <- slopes(data, mean.doy ~ var, tax_level = species, trait)
  
  #generate vector of every single occurrence of a trait
  traits <- as.character(stat$trait) %>%
    str_split(", ", simplify = TRUE )%>%
    str_c(sep = ", ") %>%
    #exclude all empty fields
    str_subset("^$", negate = TRUE)
  
  #generate list of all possible traits
  traitlist <- unique(traits)
  
  #initiate list and counting var
  trait.list <- list()
  i <- 1
  
  #generate dataset with all plants present in one trait
  for (t in traitlist) {
    
    stat.i <- filter(stat, str_detect(trait, t))
    
    #skip this trait level if there are not enough species
    if (nrow(stat.i) < min_spec) {next}
    
    stat.i$trait <- t
    
    trait.list[[i]] <- stat.i
    
    #increment counting var
    i <- i +1
  }
  
  #bind all data frames together
  stat.traits <- bind_rows(trait.list)
  
  
  ## calculate meta stats -------------------------------------------------
  
  #calculate N, mean and sd of slopes 
  stat.traits.meta <- stat.traits %>%
    group_by(trait) %>%
    summarise(n.slope = length(slope),
              mean.slope = mean(slope),
              sd.slope = sd(slope),
              se.slope = sd(slope)/sqrt(length(slope)),
              ci.min = ci.min(slope),
              ci.max = ci.max(slope),
              adv.frac = sum(slope < 0)/length(slope),
              var = toString(quo_get_expr(var)),
    )
  
  ## ANOVA   ----------------------------------------------------
  
  mod.trait <- aov(data = stat.traits, slope ~ trait)
  
  
  ## Renaming and output ----------------------------------------
  
  # abbreviate trait and make a symbol
  abb_trait <- sym(abbreviate(tolower(quo_get_expr(trait_name)), 7, strict = TRUE))
  
  # use the symbol to make a quosure
  abb_trait <- enquo(abb_trait)
  
  # rename trait columns
  stat.traits.meta <- rename(stat.traits.meta, !! abb_trait := trait)
  stat.traits <- rename(stat.traits, !! abb_trait := trait)
  
  #return both data frames as a list
  return(list(stat = stat.traits, meta = stat.traits.meta, model = mod.trait))
  
  
}

# function for analyzing differences between trait levels
analyse.trait <- function(trait = Habitat, data = dat.occ.plant,
                          verbose = TRUE) {
  
  trait <- enquo(trait)
  
  #calculate stats and meta stats for Habitat trait, time and temperature
  stat.trait.time <- trait(var = year, trait_name = !! trait, data = data) 
  
  stat.trait.temp <- trait(var = y_mean_temp, trait_name = !! trait, data = data)
  
  #abbreviate trait name from here
  trait <- sym(abbreviate(tolower(quo_get_expr(trait)), 7, strict = TRUE))
  
  trait <- enquo(trait)
  
  print(
    #plot results for time
    ggplot() +
      geom_jitter(data = stat.trait.time$stat,
                  aes(x = !! trait, y = slope),
                  alpha = 0.5, col = "grey", width = 0.2) +
      geom_errorbar(data = stat.trait.time$meta,
                    aes(x = !! trait, ymin = ci.min, ymax = ci.max),
                    col = "#2F5496", size = 1) +
      geom_point(data = stat.trait.time$meta,
                 aes(x = !! trait, y = mean.slope),
                 col = "#2F5496", size = 2) +
      geom_text(data = stat.trait.time$meta, 
                aes(x = !! trait, y = 5, label = paste0("n =  \n", n.slope)),
                col = "gray32") +
      labs(x = str_glue("{as.character(quo_get_expr(trait))}"),
           y = "Mean Slope [days/a] (\u00B1 95% CI)",
           title = str_glue("Mean slope differences between {as.character(quo_get_expr(trait))}s over time")) +
      theme_minimal()
  )
  
  print(
    #plot results for temp
    ggplot() +
      geom_jitter(data = stat.trait.temp$stat,
                  aes(x = !! trait, y = slope),
                  alpha = 0.5, col = "grey", width = 0.2) +
      geom_errorbar(data = stat.trait.temp$meta,
                    aes(x = !! trait, ymin = ci.min, ymax = ci.max),
                    col = "#2F5496", size = 1) +
      geom_point(data = stat.trait.temp$meta,
                 aes(x = !! trait, y = mean.slope),
                 col = "#2F5496", size = 2) +
      geom_text(data = stat.trait.temp$meta, 
                aes(x = !! trait, y = 44, label = paste0("n =  \n", n.slope)),
                col = "gray32") +
      labs(x = str_glue("{as.character(quo_get_expr(trait))}"),
           y = "Mean Slope [days/\u00B0C] (\u00B1 95% CI)",
           title = str_glue(
             "Mean slope differences between {as.character(quo_get_expr(trait))}s with temp")
      ) +
      theme_minimal())
  
  
  cat("\nAnova mean time slope diffferences:\n\n")
  
  # summarise ANOVA
  print(summary(stat.trait.time$model))
  
  # make data frame of signifcatnly different pairs and display them if there are any
  stat.trait.time.posthoc <- TukeyHSD(stat.trait.time$model)
  stat.trait.time.posthoc <- as.data.frame(stat.trait.time.posthoc[["trait"]]) %>%
    mutate(pairs = unlist(attr(stat.trait.time.posthoc[["trait"]], "dimnames")[1])) %>%
    filter(`p adj` <= 0.05)
  
  if (nrow(stat.trait.time.posthoc) > 0) {
    
    cat("\n\n Differences from Tukey HSD Test:\n\n")
    
    print.data.frame(stat.trait.time.posthoc)
    
  } else {
    
    cat("\n\n No significant pairwise differences.")
    
  }
  # same for temperature
  cat("\n------------------------------------------------------------\n")
  
  cat("\nAnova mean temp slope differences:\n\n")
  
  print(summary(stat.trait.temp$model))
  
  stat.trait.temp.posthoc <- TukeyHSD(stat.trait.temp$model)
  stat.trait.temp.posthoc <- as.data.frame(stat.trait.temp.posthoc[["trait"]]) %>%
    mutate(pairs = unlist(attr(stat.trait.temp.posthoc[["trait"]], "dimnames")[1])) %>%
    filter(`p adj` <= 0.05)
  
  if (nrow(stat.trait.temp.posthoc) > 0) {
    
    cat("\n\n Differences from Tukey HSD Test:\n\n")
    
    print.data.frame(stat.trait.temp.posthoc)
    
  } else {
    
    cat("\n\n No significant pairwise differences.")
    
  }
  
  # get trait abbreviation
  abb_trait <- abbreviate(tolower(as.character(quo_get_expr(trait))), 7, strict = TRUE)
  
  # assign both stat lists to objects in the global environment
  assign(str_glue("stat.{abb_trait}.time"), stat.trait.time, pos = 1)
  assign(str_glue("stat.{abb_trait}.temp"), stat.trait.temp, pos = 1)
  
  # save trait data to disk
  write.csv(x = stat.trait.time$stat,
            file = here('data', str_glue("trait_{abb_trait}_time.csv")))
  write.csv(x = stat.trait.temp$stat,
            file = here('data', str_glue("trait_{abb_trait}_temp.csv")))
  
}

# function for generating plots of duration shift plots
dur_plot <- function(x = decade,
                     y = duration,
                     ...,
                     data = dat.occ.dur.dec,
                     title = NULL, xlab = NULL, ylab = NULL) {
  
  x <- enquo(x)
  y <- enquo(y)
  
  dots <- enquos(...)
  
  plot <- ggplot(data) +
    geom_point(aes(!!x, !!y, !!!dots),
               alpha = 0.3,
               position = position_dodge(width = 2)) +
    geom_smooth(aes(!!x, !!y, !!!dots),
                method = "lm",
                # se = FALSE
    ) +
    # stat_summary(aes(!!x, !!y, !!!dots),
    #              size = 1.5, pch = 18,
    #              fun = mean, geom = "point"
    # ) + 
    coord_cartesian(ylim = c(0, 365)) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(plot)
  
}

# function for generating plots of duration shift slopes
dur_slope_plot <- function(x = id.grp,
                           y = slope,
                           ymeta = mean.slope,
                           ymin = ci.min,
                           ymax = ci.max,
                           ...,
                           data = stat.spec.dur,
                           metadata = stat.spec.dur.meta,
                           title = NULL, xlab = NULL, ylab = NULL,
                           ylim = NULL) {
  
  x <- enquo(x)
  y <- enquo(y)
  ymeta <- enquo(ymeta)
  ymin <- enquo(ymin)
  ymax <- enquo(ymax)
  
  dots <- enquos(...)
  
  if (is.null(ylim)) {
    
    # generate vector of all relevant columns
    yvec <- c(pivot_longer(data, cols = starts_with("slope"))$value,
              pivot_longer(metadata, cols = starts_with("ci"))$value) 
    
    #calculate maximum ydimensions
    ylim <- c(min(yvec, na.rm = TRUE) * 1.4,
              max(yvec, na.rm = TRUE) * 1.4)
    
    
  }
  
  # ypos.df <- select(data, vec := !! y)
  
  plot <- ggplot(data = metadata) +
    geom_hline(yintercept = 0, lty = 2, col = "gray31") +
    geom_jitter(data = data,
                aes(x = !! x, y = !! y,
                    alpha = n), 
                size = 1.5, width = 0.2, col = "gray31") +
    geom_point(aes(x = !! x, y = !! ymeta), col = "#2F5496",
               size = 2) +
    geom_errorbar(aes(x = !! x, 
                      ymin = !! ymin, ymax = !! ymax),
                  size = 1, col = "#2F5496") +
    geom_text(aes(x = !! x, y = ylim[2] * 0.9, label = paste0("N =  ", n.slope)), col = "gray31") +
    labs(x = xlab,
         y = ylab,
         title = title) +
    coord_cartesian(ylim = ylim) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(plot)
  
}


# function for generating plots of duration shift plots
dur_plot_2k <- function(x = decade,
                        y = duration,
                        ...,
                        data = dat.occ.dur.dec,
                        title = NULL, xlab = NULL, ylab = NULL) {
  
  x <- enquo(x)
  y <- enquo(y)
  
  dots <- enquos(...)
  
  plot <- ggplot(data) +
    # geom_point(aes(!!x, !!y, !!!dots),
    #            alpha = 0.3) +
    geom_smooth(aes(!!x, !!y, !!!dots),
                # method = "lm",
                # se = FALSE
    ) +
    # stat_summary(aes(!!x, !!y, !!!dots),
    #              size = 1.5, pch = 18,
    #              fun = mean, geom = "point"
    # ) + 
    coord_cartesian(ylim = c(0, 365)) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(plot)
  
}

# function for generating species interaction plots on yearly data
int_plot_year <- function(data = dat.i, x,
                     filename = paste0(x, '_',
                                    paste(unique(data$species),
                                          collapse = '_'),
                                    ".png"),
                     dir = paste0("plots/interactions/", x),
                     width = 20, height = 12.5
) {
  # print(dir)
  dir.check(dir)
  
  plot_int <- ggplot(data,
                     aes_string(x = x,
                                y = "mean.doy",
                                col = "kingdom")) +
    geom_point(aes(size = n.rec)) +
    geom_smooth(method = "lm", size = 1.5) +
    labs(title = toString(unique(data$species))) +
    coord_cartesian(xlim = c(year.start, year.stop),
                    ylim = c(0, 365)) +
    theme(axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.text.x = element_text(angle = 30, hjust = 1),
          axis.ticks = element_line(colour = "gray25", size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = "gray25", size = 1.2),
          plot.title = element_text(size = 40, hjust = .5),
          legend.title = element_text(size = 40),
          legend.text = element_text(size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_blank())
  
  ggsave(filename = paste0(dir, '/', filename),  width = width, height = height,
         plot = plot_int)
}

# function for generating species interaction plots on decadal data
# warning, does different things from yearly plot
int_plot_dec <- function(data = dat.i, x,
                     filename = paste0(x, '_',
                                    paste(unique(data$species),
                                          collapse = '_'),
                                    ".png"),
                     dir = paste0("plots/interactions/", x, "/"),
                     width = 20, height = 12.5
) {
  
  dir.check(dir)
  
  plot_int <- ggplot(data,
                     aes_string(x = x,
                                y = "mean.doy",
                                col = "kingdom")) +
    geom_point(aes(size = n.rec)) +
    geom_smooth(method = "lm", size = 1.5) +
    labs(title = toString(unique(data$species))) +
    coord_cartesian(xlim = c(year.start, year.stop),
                    ylim = c(0, 365)) +
    theme(axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.text.x = element_text(angle = 30, hjust = 1),
          axis.ticks = element_line(colour = "gray25", size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = "gray25", size = 1.2),
          plot.title = element_text(size = 40, hjust = .5),
          legend.title = element_text(size = 40),
          legend.text = element_text(size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_blank())
  
  ggsave(filename = paste0(dir, filename),  width = width, height = height,
         plot = plot_int)
}


# function for summarizing data frames containing grouped data
sum_df <- function(df, group_vars) {
  
  num_list <- list(mean = ~mean(.x),
                   median = ~median(.x),
                   min = ~min(.x),
                   max = ~max(.x),
                   n.unique = ~n_distinct(.x))
  
  char_list <- list(n = ~n(),
                    n.unique = ~n_distinct(.x))
  
  df_sum <- df %>% 
    ungroup() %>% 
    mutate(across(where(is.factor), as.character)) %>% 
    group_by({{group_vars}}) %>% 
    summarise(tibble(
      across(where(is.character), char_list,
             .names = '{.col}.{.fn}'
             ),
      across(where(is.numeric), num_list,
             .names = '{.col}.{.fn}'
             )
             )
      ) %>% 
    ungroup()
  
  return(df_sum)
}

# function for printing a log message starting with date and time
log_msg <- function(...) {
  print(paste(toString(format(Sys.time())), ...))
}


# Plot script functions ---------------------------------------------------


#function for finding variance inflation factors
corvif <- function(data) {
  
  #coerce to data frame
  data <- as.data.frame(data, formula)
  
  #vif part
  form <- formula(paste("mean.doy ~ ", paste(strsplit(
    names(data), " "), collapse = " + ")))
  data   <- data.frame(mean.doy = 1 + rnorm(nrow(data)) , data)
  lm_mod  <- lm(form, data)
  
  cat("\n\nVariance inflation factors\n\n")
  print(car::vif(lm_mod))
}

# function for generating letter labels to indicate differences between
# factor levels as determined by a statistical test. When group_order 
# (character vector of level names) is provided, pairings will be assigned
# Letters according to their precedence in group_order
pairdiff <- function(data, formula, test = "Tukey",
                     Letters = LETTERS, threshold = 0.05,
                     level_order = NULL) {
  
  if (test == "Tukey") {
    
    # do test
    tk_test <- TukeyHSD(aov(data = data, formula = formula))
    
    # extract names and p-val vector
    pvals <- tk_test[[paste(formula[3])]][,4]
    names(pvals) <- rownames(tk_test[[paste(formula[3])]])
    
    
    #find letters for non differing factor levels
    pairs <- multcompLetters(pvals,
      Letters = Letters,
      threshold = threshold)
    
    # save only Letters and factor levels
    levels_df <- data.frame(letter = pairs$Letters, level = names(pairs$Letters))
    
    
  } else if (test == "t.test") { 
    
    #perform test
    test <- t.test(formula, data)
    
    
    # extract names and pvals from test
    pvals <- test[["p.value"]]
    names(pvals) <-paste(str_extract(names(test[["estimate"]]),
                                        "[:alpha:]+$"), 
                            collapse = "-")
    
    #find letters for non differeing factor levels
    pairs <- multcompLetters(pvals, Letters = Letters,
                             threshold = threshold)
    # save letters and levels
    levels_df <- data.frame(letter = pairs$Letters, level = names(pairs$Letters))
    
    
  } else {
    
    stop("Unrecognised test!")
    
  }
  
  # check if a sorting vector is provided, if not return what we have so far
  if (is.null(level_order)) {
    
    return(levels_df)
    
  }
  
  # attempt to filter out level_order values not in data
  level_order <- level_order[level_order %in% unique(data[[toString(formula[3])]])]
  
  # check if the next step is possible 
  if (nrow(levels_df) != length(level_order)) {
    stop("Data levels have different length from level_order vector provided!")
  } 
  
  # initialise vector for later
  ordered_letters <- c()
  
  # redistribute letters to reflect order in plot (i.e. A gets assigned to first element)
  # this is pretty inefficient right now, but i just want it to work and *in theory* this 
  # should never need to deal with more than 25 letters, also the precedence replacement mechanic
  # will prbably break with more than 52 levels if digits were used instead of letters 
  # to denote pairwise differences
  
  # first extract letters according to levels of level_order
  for (level in level_order) {
    
    # extract current row
    level_row <- levels_df[levels_df$level == level, ]
    
    # extract letters (split into individual characters)
    level_letters <- strsplit(level_row$letter, split = NULL)[[1]]
    
    # add the letters to overall letters list if they are not there already
    for (letter in level_letters) {
      
      if ( !(letter %in% ordered_letters)) {
        
        ordered_letters <- append(ordered_letters, letter)
        
      }
      
    }
    
  }
  
  # now turn letters into numbers corresponding to their precedence
  # this is done in a separrate step from the next to ensure nothing gets overwritten
  # (if for some reason numbers were used instead of letters, this uses letters for precedence instead.
  #  if theres a mixture theres a good chance this will break but i cant be asked to think
  #  for that case) 
  
  # detect whether any numbers were used
  digits_used <- str_detect(paste(Letters, collapse = ""), "[:digit:]")
  
  # replace letters with their precedence
  for (i in seq_along(ordered_letters)) {
    
    letter <- ordered_letters[i]
    
    # determine replacement
    if (!digits_used) {
      replacement <-  toString(i)
    } else {
      replacement <- c(LETTERS, letters)[i]
    }
    
    levels_df$letter <- str_replace_all(levels_df$letter, letter, paste(replacement, ",", sep = ""))
    
  }
  
  # lastly replace precedence with letters again
  for (i in seq_along(ordered_letters)) {
    
    letter <- ordered_letters[i]
    
    # determine replacement
    if (!digits_used) {
      replacement <-  Letters[i]
    } else {
      replacement <-toString(i)
    }
    
    levels_df$letter <- str_replace_all(levels_df$letter, paste(i, ",", sep = ""), replacement)
    
  } 
  
  
  return(levels_df)
  
  
}


# function for generating trait plots
traitplot <- function(abb_trait) {
  
  # make sure directory exits
  dir.check(here("plots/traits"))
  
  # find the time  and temp stat list
  stat.trait.time <- eval(as.name(str_glue("stat.{abb_trait}.time"))) 
  stat.trait.temp <- eval(as.name(str_glue("stat.{abb_trait}.temp")))
  
  # make model formula for pairwise differences 
  # same formula works for time and temp
  pd.form <- as.formula(str_glue("slope ~ {abb_trait}"))
  
  # enquote the trait abbrev
  abb_trait <- sym(abb_trait)
  
  abb_trait <- enquo(abb_trait)
  
  # rename columns
  stat.trait.time$stat <- stat.trait.time$stat %>%
    mutate(trait := !! abb_trait)
  stat.trait.time$meta <- stat.trait.time$meta %>%
    mutate(trait := !! abb_trait)
  stat.trait.temp$stat <- stat.trait.temp$stat %>%
    mutate(trait := !! abb_trait)
  stat.trait.temp$meta <- stat.trait.temp$meta %>%
    mutate(trait := !! abb_trait)
  
  # generate time and temp plots and save them for further use
  
  #plot results for time
  plt.time <-  ggplot() +
    geom_jitter(data = stat.trait.time$stat,
                aes(x = !! abb_trait, y = slope),
                size = 5,
                alpha = 0.2, col = "gray40", width = 0.2) +
    geom_hline(yintercept = 0, lty = 3, col = col.line) + 
    geom_errorbar(data = stat.trait.time$meta,
                  aes(x = !! abb_trait, ymin = ci.min, ymax = ci.max),
                  col = "gray10", size = 2,
                  width = 0.6) +
    geom_point(data = stat.trait.time$meta,
               aes(x = !! abb_trait, y = mean.slope),
               col = "gray10", size = 8) +
    # geom_text(data = stat.trait.time$meta,
    #           aes(x = !! abb_trait,
    #               y = ypos(stat.trait.time$stat$slope, frac = 0.5),
    #               label = paste0("n = \n", n.slope)),
    #           size = 13, col = "gray31") +
    # #add labels for differing factor levels
    # geom_text(
    #   data = pairdiff(stat.trait.time$stat, pd.form,
    #                   level_order = sort(
    #                     unique(stat.trait.time$stat[[quo_get_expr(abb_trait)]]))
    #                   ),
    #   aes(
    #     x = level,
    #     y = ypos(stat.trait.time$stat$slope, frac = 0.1),
    #     label = letter
    #   ),
    #   size = 13,
    #   col = "gray31"
    # ) +
    labs(x = as.character(quo_get_expr(abb_trait)),
         y = "Temporal change [days/decade]") +
    scale_y_continuous(labels = mult_10_format()) +
    theme(axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.position = "none",
          panel.background = element_blank(),
          panel.spacing = unit(32, "bigpts"),
          strip.background = element_blank(),
          strip.text = element_text(size = 40), 
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank()) 
  
  
  #plot results for temp
  plt.temp <- ggplot() +
    geom_jitter(data = stat.trait.temp$stat,
                aes(x = !! abb_trait, y = slope),
                size = 5,
                alpha = 0.2, col = "gray40", width = 0.2) +
    geom_hline(yintercept = 0, lty = 3, col = col.line) + 
    geom_errorbar(data = stat.trait.temp$meta,
                  aes(x = !! abb_trait, ymin = ci.min, ymax = ci.max),
                  col = "gray10", size = 2,
                  width = 0.6) +
    geom_point(data = stat.trait.temp$meta,
               aes(x = !! abb_trait, y = mean.slope),
               col = "gray10", size = 8) +
    # geom_text(data = stat.trait.temp$meta,
    #           aes(x = !! abb_trait,
    #               y = ypos(stat.trait.temp$stat$slope,
    #                        frac = 0.5),
    #               label = paste0("n = \n", n.slope)),
    #           size = 13, col = "gray31") +
    # #add labels for differing factor levels
    # geom_text(
    #   data = pairdiff(stat.trait.temp$stat, pd.form,
    #                   level_order = sort(
    #                     unique(stat.trait.time$stat[[quo_get_expr(abb_trait)]]))
    #   ),
    #   aes(
    #     x = level,
    #     y = ypos(stat.trait.temp$stat$slope, frac = 0.1),
    #     label = letter
    #   ),
    #   size = 13,
    #   col = "gray31"
    # ) +
    labs(x = as.character(quo_get_expr(abb_trait)),
         y = "Climate sensitivity [days/\u00B0C]") +
    theme(axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.position = "none",
          panel.background = element_blank(),
          panel.spacing = unit(32, "bigpts"),
          strip.background = element_blank(),
          strip.text = element_text(size = 40), 
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank()) 
  
  
  # save both plots into the global environment 
  assign(str_glue("plots_{as.character(quo_get_expr(abb_trait))}"),
         list(plt.time, plt.temp), pos = 1)
  
  # print plots
  
  png(here("plots/traits",
           str_glue("Mean_CI_{as.character(quo_get_expr(abb_trait))}_time.png")), width = 1600, height = 1000)
  
  print(plt.time)
  
  dev.off()
  
  
  png(here("plots/traits",
           str_glue("Mean_CI_{as.character(quo_get_expr(abb_trait))}_temp.png")), width = 1600, height = 1000)
  
  print(plt.temp)
  
  dev.off()
  
}

# define a format that transforms labels to 10x
mult_10_format <- function() {
  function(x) format(10 * x, digits = 2) 
}

# define a format that adds an "s" to a number
# this is used for decades labeling
decade_format <- function() {
  function(x) paste(format(x), "s", sep = "")
}

# define format to convert decimal to percent
decimal_to_percent_format <- function() {
  function(x) format(100 * x)
}


# define function for making midado plots
dur_mikado_plot <- function(data, filename = NULL, width = 1500, height = 1000,
                            min = min.doy, mean = mean.doy, max = max.doy,
                            x = year, facet = ~ id.grp,
                            raw.alpha = 0.3,
                            title = NULL, xlab = NULL, ylab = NULL) {
  
  min <- enquo(min)
  max <- enquo(max)
  mean <- enquo(mean)
  x <- enquo(x)
  
  png(filename, width = width, height = height)
  
  print(
    ggplot(data) +
      # group regressions
      # geom_line(aes(year, mean.doy, group = species),
      #           stat = "smooth", method = "lm", se = FALSE,
      #           col = "gray31", alpha = raw.alpha, size = 1.5) +
      # geom_line(aes(!! x, !! min, group = species),
      #           stat = "smooth", method = "lm", se = FALSE,
      #           col = "cyan", alpha = raw.alpha, size = 1.5) +
      # geom_line(aes(!! x, !! max, group = species),
      #           stat = "smooth", method = "lm", se = FALSE,
      #           col = "orange", alpha = raw.alpha, size = 1.5) +
      # overall regressions
    geom_line(aes(!! x, !! mean, group = id.grp),
              stat = "smooth", method = "lm", se = TRUE,
              col = "black", size = 2) +
      geom_line(aes(!! x, !! min, group = id.grp),
                stat = "smooth", method = "lm", se = TRUE,
                col = "blue", size = 2) +
      geom_line(aes(!! x, !! max, group = id.grp),
                stat = "smooth", method = "lm", se = TRUE,
                col = "red", size = 2) +
      facet_wrap(facet, nrow = 1) +
      labs(x = xlab,
           y = ylab) +
      theme(text = element_text(size = 40),
            axis.text.x = element_text(angle = 50, hjust = 1),
            axis.ticks = element_line(colour = col.ax, size = 1.2),
            axis.ticks.length.y.left = unit(8, "bigpts"),
            axis.ticks.length.x.bottom = unit(8, "bigpts"),
            axis.line = element_line(colour = col.ax, size = 1.2),
            strip.background = element_blank(),
            plot.margin = unit(c(128, 64, 0, 64), "bigpts"),
            plot.title = element_text(size = 40, hjust = 0.5),
            plot.subtitle = element_text(size = 30, hjust = 0.5),
            legend.position = "right",
            legend.text = element_text(size = 35),
            legend.title = element_text(size = 40),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_blank())
  )
  
  dev.off()
  
}


# function for generating plots of duration shift slopes
dur_slope_plot_save <- function(x = id.grp,
                                y = slope,
                                ymeta = mean.slope,
                                ymin = ci.min,
                                ymax = ci.max,
                                ...,
                                data = stat.all.dur,
                                metadata = stat.all.dur.meta,
                                xlab = NULL, ylab = NULL,
                                ylim = NULL,
                                y_ax_explanation = NULL # currently unused
                                ) {
  
  x <- enquo(x)
  y <- enquo(y)
  ymeta <- enquo(ymeta)
  ymin <- enquo(ymin)
  ymax <- enquo(ymax)
  
  dots <- enquos(...)
  
  if (is.null(ylim)) {
    
    # generate vector of all relevant columns
    yvec <- c(pivot_longer(data, cols = starts_with("slope"))$value,
              pivot_longer(metadata, cols = starts_with("ci"))$value) 
    
    #calculate maximum ydimensions
    ylim <- c(min(yvec, na.rm = TRUE) * 1.4,
              max(yvec, na.rm = TRUE) * 1.4)
    
    
  }
  
  # ypos.df <- select(data, vec := !! y)
  
  plot <- ggplot(data = metadata) +
    geom_hline(yintercept = 0, lty = 2, col = col.line) +
    geom_jitter(data = data,
                aes(x = !! x, y = !! y), 
                size = 5, width = 0.2, col = col.pt,
                alpha = 0.3) +
    geom_point(aes(x = !! x, y = !! ymeta, col = !! x),
               size = 8) +
    geom_errorbar(aes(x = !! x, 
                      ymin = !! ymin, ymax = !! ymax,
                      col = !! x),
                  size = 2) +
    geom_text(aes(x = !! x, y = ylim[2] * 0.9, label = paste0("N =  ", n.slope)),
              col = col.note, size = 15) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(filter(data,
                             #exclude Hymenoptera, Diptera and the Insects overall group
                             !(id.grp %in% c(excl.group.year,
                                             col.grp$group[5]))),
                      formula(str_glue("{quo_get_expr(y)} ~ {quo_get_expr(x)}"))
                      ),
      aes(
        x = level,
        y = ypos(c(stat.all.dur$slope,
                   stat.all.dur.meta$ci.max),
                 frac = 0.1),
        label = letter
      ),
      size = 13,
      col = col.note
    ) +
    labs(x = xlab,
         y = ylab) +
    coord_cartesian(ylim = ylim,
                    # xlim = c(1, nrow(metadata) + 1) # extends plot to the right
                    ) +
    scale_y_continuous(labels = mult_10_format(),
                       
    ) +
    scale_color_manual(name = "Group",
                       labels = col.group.stc$group,
                       values = col.group.stc$colour) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.text.x = element_text(size = 35,
                                 # angle = 25,
                                 # hjust = 1
                                 ),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.position = "none",
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 40),
      plot.margin = unit(c(128, 20, 0, 0), "bigpts")
    )
  
}



# function for generating plots of shift slopes
# this is now annoyingly hacked apart for the graphical abstract
slope_plot_save <- function(x = id.grp,
                            y = slope,
                            ymeta = mean.slope,
                            ymin = ci.min,
                            ymax = ci.max,
                            ...,
                            data = stat.all.time,
                            metadata = all.time.meta,
                            xlab = NULL, ylab = NULL,
                            ylim = NULL, # currently does nothing (graphical abstract)
                            title = NULL,
                            data_points = TRUE,
                            scale_y_10 = TRUE) {
  
  x <- enquo(x)
  y <- enquo(y)
  ymeta <- enquo(ymeta)
  ymin <- enquo(ymin)
  ymax <- enquo(ymax)
  
  dots <- enquos(...)
  
  if (is.null(ylim)) {
    
    # generate vector of all relevant columns
    yvec <- c(pivot_longer(data, cols = starts_with("slope"))$value,
              pivot_longer(metadata, cols = starts_with("ci"))$value) 
    
    #calculate maximum ydimensions
    ylim <- c(min(yvec, na.rm = TRUE) * 1.45,
              max(yvec, na.rm = TRUE) * 1.45)
    
    
  }
  
  # ypos.df <- select(data, vec := !! y)
  
  # we buld the plot in sections to control whether some things should get added
  # first the basic plot
  plot <- ggplot(data = metadata) +
    geom_hline(yintercept = 0, lty = lty.sec, col = col.line)
  
  # if necessary, add raw data
  if (data_points == TRUE) {
    
    plot <- plot  + 
      geom_jitter(data = data,
                  aes(x = !! x, y = !! y), 
                  size = 5, width = 0.2, col = col.pt,
                  alpha = 0.3) 
    
  }
  
  # continue with the rest of the plot
  plot <- plot +
    geom_point(aes(x = !! x, y = !! ymeta, col = !! x),
               size = 10) +
    geom_errorbar(aes(x = !! x, 
                      ymin = !! ymin, ymax = !! ymax,
                      col = !! x),
                  size = 2) +
    geom_text(aes(x = !! x, y = ypos(metadata$ci.max,
                                     if (data_points == TRUE) {data$slope},
                                     3, # make sure its at least at three (thirty after scaling)
                                     frac = 0.6),
                  label = paste0("N =  ", n.slope)),
              col = col.note, size = annot_text) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(data = filter(data,
                                    #exclude Hymneoptera, Diptera and the Insects overall group
                                    !(id.grp %in% c(excl.group.year,
                                                    col.grp$group[5]))),
                      formula(str_glue("{quo_get_expr(y)} ~ {quo_get_expr(x)}")),
                      level_order = levels(data[[toString(quo_get_expr(x))]])),
      aes(
        x = level,
        y = ypos(metadata$ci.max,
                 if (data_points == TRUE) {data$slope},
                 3, # see above
                 frac = 0.3),
        label = letter
      ),
      size = annot_text,
      col = col.note
    ) +
    labs(x = xlab,
         y = ylab,
         title = title)
  
  # if we're dealing with time data, scale the y-axis by ten
  # (to reflect days/decade, data is in days/year)
  if (scale_y_10 == TRUE) {
    plot <- plot + scale_y_continuous(labels = mult_10_format())
  }
  
  
  # if no data points are plotted, ignore ylimits
  # currently always ignored
  if (FALSE) {
    # if (data_points == TRUE) {
    plot <- plot + coord_cartesian(ylim = ylim)
  }
  
  # continue with the rest of the plot
  plot <- plot +
    scale_color_manual(name = "Group",
                       labels = col.group$group,
                       values = col.group$colour) +
    theme(
      axis.title = element_text(size = 50),
      axis.text = element_text(size = 45),
      axis.text.x = element_text(size = 45,
                                 # angle = 25,
                                 # hjust = 1
      ),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.position = "none",
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 40),
      plot.title = element_text(size = 60, hjust = 0.5, vjust = 18)) +
    # special theme for graphical abstract
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid = element_blank(),
      legend.background = element_rect(fill = "transparent"), 
      legend.box.background = element_rect(fill = "transparent") 
    )
  
  # change margin size depending on whether its a raw data plot
  # currently knocked out
  if (FALSE) {
    # if (data_points == TRUE) {
    plot <- plot + 
      theme(plot.margin = unit(c(128, 32, 0, 32), "bigpts"))
  }  else { 
    plot <- plot + 
      theme(plot.margin = unit(c(256, 32, 0, 32), "bigpts"))
  }
  
}


# bioflor trait functions -------------------------------------------------


#function for getting traits by their regex string
get_traits <- function(text, traits, patterns) {
  
  #initiate data frame
  traits_out <- setNames(data.frame(
    matrix(ncol = length(traits),
           nrow = 1)),
    traits)
  
  #start for loop to extract each trait value
  for (i in seq(1, length(traits), 1)) {
    
    traits_out[1, i] <- paste(unlist(stringr::str_extract_all(text, patterns[i])), collapse = ", ")
    
  }
  
  return(traits_out)
  
}


# Huge plot functions -----------------------------------------------------


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


# Result saving functions -------------------------------------------------

# define function to write name and value into the next row of the data frame
# also optionally transform the value and save it to extra column or alternatively provide
# formatted Value directly (one excludes the other)
append.df <- function(name, value, multiplier = NULL, formattedValue = NULL,  target = res.figures) {
  
  # throw error if both multiplier and formattedValue are given
  # this shouldnt happen because i want to write both into the same
  # column
  if (!is.null(multiplier) & !is.null(formattedValue)) {
    
    stop("Provided both multiplier and formattedValue.")
    
  } else if (is.null(multiplier) & is.null(formattedValue)){
    
    formattedValue <- NA
    
  } else if (!is.null(multiplier)) {
    
    formattedValue <- value * multiplier
    
  } 
  
  target[nrow(target) + 1, ] <- data.frame(name = name,
                                           value = value,
                                           formattedValue = formattedValue)
  
  return(target)
  
}
