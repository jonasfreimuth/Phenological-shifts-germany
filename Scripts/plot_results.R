#----------------------------------------------

# FOR SAVING PLOTS FROM ANALYSIS NOTEBOOK ONLY

#----------------------------------------------

# set options
options(stringsAsFactors = FALSE)

# if it doesn't exist yet, create the plots directory
dir.check(here("plots"))

# set groups to be excludes
excl.group.year <- c("Diptera", "Hymenoptera")

# set named vector for recoding group names to common names
recode.vec <- c(
  "Coleoptera" = "Beetles",
  "Diptera" = "Flies",
  "Hymenoptera" = "Bees",
  "Lepidoptera" = "Butterflies"
)

# make alternative vector for interactions
recode.vec.int <- recode.vec[2:4]
recode.vec.int[1:3] <- c("Hoverfly - Plant", "Bee - Plant", "Butterfly - Plant")


# translate to trivial names
excl.trivial.year <- recode_factor(excl.group.year, !!! recode.vec)

# Set graphics parameters -------------------------------------------------

# set colour for raw data points
col.pt <- "gray50"

# set colour for static lines
col.line <- "gray31"

# set colour for axis elements
col.ax <- "gray31"

# set col for annotations
col.note <- "gray31"

# annotation text size
annot_text <- 20

# line types
lty.sec <- 3

# line size
hline_size_large <- 2
line_size_large <- 2.5

# text size
text_size_large <- 50
legend_text_size_large <- 40

# axis parameters
axis_size_large <- 2
axis_ticks_size_large <- 2
axis_ticks_length_large <- 16


# set colour tables -------------------------------------------------------

# define colours
# DO NOT CHANGE ORDER, only append
col.grp <- data.frame(group = c("Beetles", "Flies", "Bees", "Butterflies",
                                "Insects overall", "Plants",
                                "Hoverfly - Plant", "Bee - Plant",
                                "Butterfly - Plant",
                                "Overall",
                                "Insect dependent", "Intermediate", "Insect independent"),
                      colour = c("#9815db", "#f41d0f", "#ffa500", "#4744ff",
                                 "gold", "#3aae10",
                                 "#f41d0f", "#ffa500", "#4744ff",
                                 "deepskyblue",
                                 "red", "#ffa500", "#3aae10"
                      ))

# alternative: named vector
col.grp.vec <- col.grp$colour
names(col.grp.vec) <- col.grp$group


#set colours for Plant-Pollinator comparisons
col.plapoll <- col.grp[5:6,]

# static polls group (no exclusion)
col.poll.stc <- col.grp[1:4,]

#only for polls
col.poll <- col.grp[1:4,] %>% 
  filter(!(group %in% excl.trivial.year))

#only for plants
col.plant <- col.grp[6,]

#set colours for group comparisons (add plant group at the end)
col.group.stc <- bind_rows(col.poll.stc, col.plant)
col.group <- bind_rows(col.poll, col.plant)

#set colours for group comparisons including overall
col.group2.stc <- bind_rows(col.poll.stc, col.plapoll)
col.group2 <- bind_rows(col.poll, col.plapoll)


#set colours for PollDep comparisons
col.PollDep <- col.grp[11:13,]

# interaction colours with standard names
col.int.stc <- col.grp[7:9,]
col.int.stc$group <- c("Hoverfly", "Bee", "Butterfly")

#set colours for Interaction comparisions
col.int <- col.grp[7:9,]

#set colours for Interaction comparison with overall group
col.int2 <- col.grp[7:10,]

#set colours for Interaction group comparisions
col.int3 <- bind_rows(col.int, col.plant)

#set colours for decade comparisons
col.dec <- data.frame(group = c("1990s and before", "all decades", "1990s on"),
                      colour = c("#2F5496", "#972A4B", "#FF0000")
)

# Functions ---------------------------------------------------------------

# function for generating letter labels to indicate differences between
# factor levels as determined by a statistical test. When group_order 
# (character vector of level names) is provided, pairings will be assigned
# Letters according to their precedence in group_order
pairdiff <- function(data, formula, test = "Tukey", Letters = LETTERS, threshold = 0.05,
                     level_order = NULL) {
  
  if (test == "Tukey") {
    
    #find letters for non differing factor levels
    pairs <- multcompLetters(
      #extract named row of pvalues from Tukey test
      TukeyHSD(aov(data = data, formula = formula))[[paste(formula[3])]][,4],
      Letters = Letters,
      threshold = threshold)
    
    # save only Letters and factor levels
    levels_df <- data.frame(letter = pairs$Letters, level = names(pairs$Letters))
    
    
  } else if (test == "t.test") { 
    
    #perform test
    test <- t.test(formula, data)
    
    #get results into form accepted by multcompLetters
    spoofvec <- test[["p.value"]]
    names(spoofvec) <- paste(str_extract(names(test[["estimate"]]), "[:alpha:]+$"), collapse = "-")
    
    #find letters for non differeing factor levels
    pairs <- multcompLetters(spoofvec, Letters = Letters,
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
  dir.check(here("Plots/traits"))
  
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
    #           aes(x = !! abb_trait, y = ypos(stat.trait.time$stat$slope, frac = 0.5),
    #               label = paste0("n = \n", n.slope)),
    #           size = 13, col = "gray31") +
    # #add labels for differing factor levels
    # geom_text(
    #   data = pairdiff(stat.trait.time$stat, pd.form,
    #                   level_order = sort(unique(stat.trait.time$stat[[quo_get_expr(abb_trait)]]))),
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
    #           aes(x = !! abb_trait, y = ypos(stat.trait.temp$stat$slope, frac = 0.5),
    #               label = paste0("n = \n", n.slope)),
    #           size = 13, col = "gray31") +
    # #add labels for differing factor levels
    # geom_text(
    #   data = pairdiff(stat.trait.temp$stat, pd.form,
    #                   level_order = sort(unique(stat.trait.time$stat[[quo_get_expr(abb_trait)]]))),
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
  assign(str_glue("plots_{as.character(quo_get_expr(abb_trait))}"), list(plt.time, plt.temp), pos = 1)
  
  # print plots
  
  png(here("Plots/traits",
           str_glue("Mean_CI_{as.character(quo_get_expr(abb_trait))}_time.png")), width = 1600, height = 1000)
  
  print(plt.time)
  
  dev.off()
  
  
  png(here("Plots/traits",
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

# It's getting hot in Germany ---------------------------------------------

ggsave(
  ggplot(dat.temp,
         aes(year, y_mean_temp, col = y_mean_temp)) +
    geom_point(size = 5) +
    geom_line(size = 2) +
    geom_smooth(aes(year, y_mean_temp), method = "lm", col = "blue", size = 2, se = FALSE) +
    labs(
      title = "Yearly mean temperature in Germany",
      x = "Year", y = "Mean temperature [\u00B0C]") +
    scale_color_gradient(low = "#2F5496", high = "#FF0000") +
    geom_text(aes(x = 1990, y = 10), label = paste0("r =  ", round(cor.temp[["estimate"]], 2)),
              col = "gray38", size = 20) +
    theme(axis.title = element_text(size = 50),
          axis.text = element_text(size = 50),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.position = "none",
          panel.background = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(size = 60, hjust = 0.5),
          plot.margin = unit(c(32, 32, 32, 32), "bigpts")) +
    # special theme for graphical abstract
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent"), 
      legend.box.background = element_rect(fill = "transparent") 
    ),
  filename = here("Plots", "Germany temperature over time.png"), width = 22, height = 14,
  bg = "transparent"
)


# Record numbers ----------------------------------------------------------

ggsave(
  ggplot(dat.n  %>% 
           mutate(id.grp = recode_factor(id.grp, !!! recode.vec, .ordered = FALSE)),
         aes(year, n.rec, col = id.grp)) +
    geom_col(size = 5, position = "dodge", width = 0.8, orientation = "x") + 
    # geom_smooth(size = 1.5,
    #             col = col.line) + 
    facet_wrap(~id.grp, ncol = 2,
               scales = "free_y") +
    labs(x = "Year",
         y = "Log10(n) records") +
    scale_y_log10() +
    scale_color_manual(name = "Group",
                       aesthetics = c("color", "fill"),
                       labels = col.group.stc$group, 
                       values = col.group.stc$colour) +
    theme(axis.title = element_text(size = text_size_large),
          axis.text = element_text(size = text_size_large),
          axis.text.x =  element_text(hjust = 1, angle = text_size_large),
          axis.ticks = element_line(colour = col.ax, size = axis_ticks_size_large),
          axis.ticks.length = unit(axis_ticks_length_large, "bigpts"),
          axis.line = element_line(colour = col.ax, size = axis_size_large),
          legend.position = "none",
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(64, "bigpts"), 
          strip.background = element_blank(),
          strip.text = element_text(size = text_size_large, color = col.note),
          plot.margin = unit(c(0, 64, 0, 0), "bigpts")
          ),
  filename = here("Plots", "group_record_numbers_time.png"),
  width = 20, height = 20
)



# Plot 1: group trends over time ------------------------------------------
# 
# png(here("Plots", "group_trends_time.png"), width = 1600, height = 1000)
# 
# print(
#   #plot trends of collection day in relation to year by group
#   ggplot(data = dat.occ, aes(x = year, y = mean.doy, group = as.factor(kingdom), col = kingdom)) +
#     geom_point(size = 3, alpha = 0.7,
#                position = position_dodge(width = 0.5)) +
#     geom_smooth(aes(col = kingdom),
#                 method = lm, se = T, size = 2.5) +
#     labs(x = "Year",
#          y = "Yearly mean DOY") +
#     scale_color_manual(name = "Group",
#                        labels = col.plapoll$group, 
#                        values = col.plapoll$colour) +
#     # scale_shape_manual(name = "Group", labels = c("Pollinators", "Plants"), 
#     #                    values = c(1, 19)) +
#     theme(axis.title = element_text(size = 40),
#           axis.text = element_text(size = 30),
#           axis.ticks = element_line(colour = col.ax, size = 1.2),
#           axis.ticks.length.y.left = unit(8, "bigpts"),
#           axis.ticks.length.x.bottom = unit(8, "bigpts"),
#           axis.line = element_line(colour = col.ax, size = 1.2),
#           legend.position = "bottom",
#           legend.text = element_text(size = 40),
#           legend.title = element_text(size = 40),
#           panel.background = element_blank(),
#           panel.grid.major = element_blank()) )
# 
# 
# dev.off()
# 
# png(here("Plots", "group_trends_temp.png"), width = 1600, height = 1000)
# 
# print(
#   #plot trends of collection day in relation to temp by group
#   ggplot(data = dat.occ, aes(x = y_mean_temp, y = mean.doy, group = as.factor(kingdom), col = kingdom)) +
#     geom_point(size = 3, alpha = 0.5) +
#     geom_smooth(aes(col = kingdom),
#                 method = lm, se = T, size = 2.5) +
#     labs(x = "Yearly mean Temperature [\u00B0C]",
#          y = "Yearly mean DOY") +
#     scale_color_manual(name = "Group",
#                        labels = col.plapoll$label, 
#                        values = col.plapoll$colour) +
#     # scale_shape_manual(name = "Group", labels = c("Pollinators", "Plants"), 
#     #                    values = c(1, 19)) +
#     theme(axis.title = element_text(size = 40),
#           axis.text = element_text(size = 30),
#           axis.ticks = element_line(colour = col.ax, size = 1.2),
#           axis.ticks.length.y.left = unit(8, "bigpts"),
#           axis.ticks.length.x.bottom = unit(8, "bigpts"),
#           axis.line = element_line(colour = col.ax, size = 1.2),
#           legend.position = "bottom",
#           legend.text = element_text(size = 40),
#           legend.title = element_text(size = 40),
#           panel.background = element_blank(),
#           panel.grid.major = element_blank(),
#     ) )
# 
# dev.off()

#same as above, but with PollOrders
ggsave(
  ggplot() +
    geom_point(data = dat.occ,
               aes(x = year, y = mean.doy,
                   col = id.grp),
               size = 2, alpha = 0.3,
               position = position_dodge(width = 0.3)) +
    # plot the regression lines faintly in gray at first so they separate from the dots
    geom_abline(aes(intercept = intercept, slope = slope),
                data = lm.res.plapoll.time,
                size = line_size_large * 1.5,
                col = col.line, alpha = 0.5) +
    geom_abline(aes(intercept = intercept, slope = slope, lty = breaks, col = id.grp),
                lm.res.plapoll.time,
                size = line_size_large) +
    # geom_smooth(data = dat.occ, 
    #             aes(x = year, y = mean.doy,
    #                 col = id.grp), 
    #             method = lm, size = 2, linetype = 1,
    #             alpha = 0.7) +
    labs(x = NULL,
         y = "Peak flowering/activity [DOY]") +
    scale_color_manual(name = "Group", 
                       values = col.group.stc$colour,
                       labels = col.group.stc$group) +
    scale_linetype(name = "Significance") +
    theme(axis.title = element_text(size = text_size_large),
          axis.text = element_text(size = text_size_large),
          axis.ticks = element_line(colour = col.ax, size = axis_ticks_size_large),
          axis.ticks.length = unit(axis_ticks_length_large, "bigpts"),
          axis.line = element_line(colour = col.ax, size = axis_size_large),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(size = legend_text_size_large),
          legend.title = element_text(size = legend_text_size_large),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(64, "bigpts"), 
          strip.background = element_blank(),
          strip.text = element_text(size = text_size_large, color = col.note),
          plot.margin = unit(c(0, 64, 0, 0), "bigpts")
    ),
  filename = here("Plots", "group_trends_time_orders.png"), width = 20, height = 12.5
)

ggsave(
  ggplot() +
    geom_point(data = dat.occ,
               aes(x = y_mean_temp, y = mean.doy,
                   col = id.grp), 
               size = 3, alpha = 0.5,
               position = position_dodge(width = 0.05)) +
    # plot the regression lines faintly in gray at first so they separate from the dots
    geom_abline(aes(intercept = intercept, slope = slope),
                lm.res.plapoll.temp,
                size = line_size_large * 1.5,
                col = col.line, alpha = 0.5) +
    geom_abline(aes(intercept = intercept, slope = slope, lty = breaks, col = id.grp),
                lm.res.plapoll.temp,
                size = line_size_large) +
    # geom_smooth(data = dat.occ, 
    #             aes(x = y_mean_temp, y = mean.doy, 
    #                 col = id.grp), 
    #             method = lm, se = T, size = 2.5, linetype = 1,
    #             alpha = 0.7) +
    labs(x = "Annual mean temperature [°C]",
         y = "Peak flowering/activity [DOY]") +
    scale_color_manual(name = "Group", 
                       values = col.group.stc$colour,
                       labels = col.group.stc$group) +
    scale_linetype(name = "Significance") +
    theme(axis.title = element_text(size = text_size_large),
          axis.text = element_text(size = text_size_large),
          axis.ticks = element_line(colour = col.ax, size = axis_ticks_size_large),
          axis.ticks.length = unit(axis_ticks_length_large, "bigpts"),
          axis.line = element_line(colour = col.ax, size = axis_size_large),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(size = legend_text_size_large),
          legend.title = element_text(size = legend_text_size_large),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(64, "bigpts"), 
          strip.background = element_blank(),
          strip.text = element_text(size = text_size_large, color = col.note),
          plot.margin = unit(c(0, 64, 0, 0), "bigpts")
    ),
  filename = here("Plots", "group_trends_temp_orders.png"), width = 20, height = 12.5
  
)

# # same again but controling for orders
# 
# png(here("Plots", "group_trends_time_orders_cont.png"), width = 1600, height = 1000)
# 
# print(
#   ggplot(data = dat.occ, 
#          aes(x = year, y = mean.doy, col = kingdom)) +
#     geom_point(size = 3, alpha = 0.4) +
#     geom_abline(intercept = coef.time[1,1],
#                 slope = coef.time[2,1], size = 2.5,
#                 col = col.plapoll$colour[1]) +
#     geom_abline(intercept = (coef.time[1,1] + coef.time[3,1]),
#                 slope = (coef.time[2,1] + coef.time[4,1]), size = 2.5,
#                 col = col.plapoll$colour[2]) +
#     labs(x = "Year", 
#          y = "Yearly mean DOY") +
#     scale_color_manual(name = "Group",
#                        labels = col.plapoll$group, 
#                        values = col.plapoll$colour) +
#     theme(axis.title = element_text(size = 40),
#           axis.text = element_text(size = 35),
#           axis.ticks = element_line(colour = col.ax, size = 1.2),
#           axis.ticks.length.y.left = unit(8, "bigpts"),
#           axis.ticks.length.x.bottom = unit(8, "bigpts"),
#           axis.line = element_line(colour = col.ax, size = 1.2),
#           legend.position = "bottom",
#           legend.text = element_text(size = 40),
#           legend.title = element_text(size = 40),
#           panel.background = element_blank(),
#           panel.grid.major = element_blank() )
#   
# )
# 
# dev.off()
# 
# 
# png(here("Plots", "group_trends_temp_orders_cont.png"), width = 1600, height = 1000)
# 
# print(
#   ggplot(data = dat.occ, 
#          aes(x = y_mean_temp, y = mean.doy, col = kingdom)) +
#     geom_point(size = 3, alpha = 0.5) +
#     geom_abline(intercept = coef.temp[1,1],
#                 slope = coef.temp[2,1], size = 2.5,
#                 col = col.plapoll$colour[1]) +
#     geom_abline(intercept = (coef.temp[1,1] + coef.temp[3,1]),
#                 slope = (coef.temp[2,1] + coef.temp[4,1]), size = 2.5,
#                 col = col.plapoll$colour[2]) +
#     labs(x = "Year", 
#          y = "Yearly mean DOY") +
#     scale_color_manual(name = "Group",
#                        labels = col.plapoll$group, 
#                        values = col.plapoll$colour) +
#     theme(axis.title = element_text(size = 40),
#           axis.text = element_text(size = 35),
#           axis.ticks = element_line(colour = col.ax, size = 1.2),
#           axis.ticks.length.y.left = unit(8, "bigpts"),
#           axis.ticks.length.x.bottom = unit(8, "bigpts"),
#           axis.line = element_line(colour = col.ax, size = 1.2),
#           legend.position = "bottom",
#           legend.text = element_text(size = 40),
#           legend.title = element_text(size = 40),
#           panel.background = element_blank(),
#           panel.grid.major = element_blank() )
#   
# )
# 
# dev.off()

# Plant order trends over time --------------------------------------------


png(here("Plots", "Plant order trends over time.png"), width = 2500, height = 1750)

print(
  #plot trends of collection day in relation to year
  ggplot() +
    facet_wrap( ~ order, drop = FALSE) +
    geom_point(data = dat.occ.plant.plot.ord,
               aes(x = year, y = mean.doy),
               size = 0.5, alpha = 0.7, col = "gray31") +
    stat_smooth(data = filter(dat.occ.plant.plot.ord, pval.time <= 0.05),
                aes(x = year, y = mean.doy),
                geom ="line", method = lm,
                col = "gray10", alpha = 0.6, size = 1) +
    labs(x = "Year", y = "DOY") +
    theme(legend.position = "none",
          axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.text.x.bottom = element_text(size = 25, angle = 30),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(64, "bigpts"),
          strip.background = element_blank(),
          strip.text = element_text(size = 40),
          plot.margin = unit(c(0, 64, 0, 0), "bigpts")) )

dev.off()


png(here("Plots", "Plant order trends over temp.png"), width = 2500, height = 1750)

print(
  
  #plot trends of collection day in relation to mean temperature
  ggplot() +
    facet_wrap( ~ order, drop = FALSE) +
    geom_point(data = dat.occ.plant.plot.ord,
               aes(x = y_mean_temp, y = mean.doy),
               size = 0.5, alpha = 0.7, col = "gray31") +
    stat_smooth(data = filter(dat.occ.plant.plot.ord, pval.temp <= 0.05),
                aes(x = y_mean_temp, y = mean.doy),
                geom ="line", method = lm,
                col = "gray10", alpha = 0.6, size = 1) +
    labs(x = "Yearly mean Temperature [°C]", y = "DOY") +
    theme(legend.position = "none") +
    theme(axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(32, "bigpts"),
          strip.background = element_blank(),
          strip.text = element_text(size = 40),
    ))

dev.off()


# Pollinator order trends -------------------------------------------------


png(here("Plots", "Pollinator order trends over time.png"), width = 1600, height = 1000)

print(
  #plot trends of collection day in relation to year
  ggplot() +
    facet_wrap( ~ order, drop = FALSE) +
    geom_point(data = dat.occ.poll.plot.ord,
               aes(x = year, y = mean.doy),
               size = 2, alpha = 0.7, col = "gray31") +
    stat_smooth(data = filter(dat.occ.poll.plot.ord, pval.time <= 0.05),
                aes(x = year, y = mean.doy),
                geom ="line", method = lm,
                col = "gray10", size = 2) +
    # geom_text(data = filter(dat.occ.poll.plot.ord, pval.time <= 0.05),
    #           aes(x = 1985, y = 400, label = paste0("R\u00B2 =  ", round(rs.time, 3))),
    #           size = 7, col = "gray31") +
    theme(legend.position = "none") +
    labs(x = "Year", y = "DOY") +
    theme(axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(32, "bigpts"),
          strip.background = element_blank(),
          strip.text = element_text(size = 40),
    ) )

dev.off()


png(here("Plots", "Pollinator order trends over temp.png"), width = 1600, height = 1000)

print(
  #plot trends of collection day in relation to mean temperature
  ggplot() +
    facet_wrap( ~ order, drop = FALSE) +
    geom_point(data = dat.occ.poll.plot.ord,
               aes(x = y_mean_temp, y = mean.doy),
               size = 2, alpha = 0.7, col = "gray31") +
    stat_smooth(data = filter(dat.occ.poll.plot.ord, pval.temp <= 0.05),
                aes(x = y_mean_temp, y = mean.doy),
                geom ="line", method = lm,
                col = "gray10", size = 2) +
    # geom_text(data = filter(dat.occ.poll.plot.ord, pval.temp <= 0.05),
    #           aes(x = 8.5, y = 400, label = paste0("R\u00B2 =  ", round(rs.temp, 3))),
    #           size = 7, col = "gray31") +
    theme(legend.position = "none")+
    labs(x = "Yearly mean Temperature [\u00B0C]", y = "DOY") +
    theme(axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(32, "bigpts"),
          strip.background = element_blank(),
          strip.text = element_text(size = 40)) )

dev.off()


# Individual species slopes -----------------------------------------------

if (any(c("full", "species") %in% opts)) {
  
  # if it doesn't exist yet, create the species subdirectory
  dir.check(here("plots/species"))
  
  #plots by species
  for (s in sort(unique(dat.occ$species))) {
    
    dat.occ.s <- filter(dat.occ, species == s)
    
    png(here("Plots/species", paste("doy_year_", s, ".png")), width = 1700, height = 1000)
    
    print(
      ggplot(dat.occ.s,
             aes(year, mean.doy, col = y_mean_temp
                 # size = log10(n.rec)
             )) +
        geom_point(size = 5) +
        # geom_errorbar(aes(ymin = mean.doy + sd.doy,
        #                   ymax = mean.doy - sd.doy), size = 1.2) +
        geom_smooth(method = "lm", size = 1.5) +
        geom_text(aes(year, mean.doy, label = n.rec), size = 8,
                  nudge_y = 10) +
        scale_color_gradientn(colors = palette(100), name = "Yearly\nmean\nTemperature",
                              limits = c(floor(min(dat.occ$y_mean_temp)), ceiling(max(dat.occ$y_mean_temp)))) +
        # scale_size_continuous(name = "log10\n# of Records",
        #                       limits = c(min(log10(dat.occ$n.rec)), max(log10(dat.occ$n.rec)))) +
        labs(title = s,
             subtitle = paste(dat.occ.s$kingdom, dat.occ.s$family, sep = ", "),
             x = "Year",
             y = "Yearly mean DOY\n(with n/year)") +
        coord_cartesian(xlim = c(min(dat.occ$year), max(dat.occ$year)),
                        ylim = c(min(dat.occ$mean.doy), max(dat.occ$mean.doy))) +
        theme(text = element_text(size = 40),
              axis.text.x = element_text(angle = 30, hjust = 1),
              axis.ticks = element_line(colour = col.ax, size = 1.2),
              axis.ticks.length.y.left = unit(8, "bigpts"),
              axis.ticks.length.x.bottom = unit(8, "bigpts"),
              axis.line = element_line(colour = col.ax, size = 1.2),
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
  
}


# First last mean mikado plot ---------------------------------------------

# define function for making midado plots
dur_mikado_plot <- function(data, filename = NULL, width = 1500, height = 1000,
                            min = min.doy, mean = mean.doy, max = max.doy, x = year, facet = ~ id.grp,
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

# plot for year
dur_mikado_plot(here("Plots", "species_mikado_year.png"), width = 1500, height = 1000,
                # rename id.grp to trivial name and exclude groups with to few species
                data = dat.occ %>% 
                  mutate(id.grp = recode_factor(id.grp, !!! recode.vec)) %>% 
                  filter(!(id.grp %in% excl.trivial.year)),
                xlab = "Year", ylab = "Day of the Year")

# plot for decade
dur_mikado_plot(here("Plots", "species_mikado_decade.png"), width = 1500, height = 1000,
                # rename id.grp to trivial name and exclude groups with to few species
                data = dat.occ.dur.dec %>% 
                  mutate(id.grp = recode_factor(id.grp, !!! recode.vec)) %>% 
                  filter(!(id.grp %in% excl.trivial.year)),
                x = decade,
                xlab = "Decade", ylab = "Day of the Year")


# Duration plots ----------------------------------------------------------

# bind group and pollinator overall meta data, replace the kingdom with the identifier for
# pollinators overall and also converting to factor, and rename levels to common names for the group
stat.all.dur.meta <- bind_rows(stat.spec.dur.meta,
                               # stat.spec.dur.poll.meta
) %>%
  # mutate(id.grp = as.factor(str_replace_all(id.grp, "Animalia", col.grp$group[5]))) %>%
  # recode levels to common names (recode only takes name/value pairs, so !!! expansion necessary)
  mutate(id.grp = recode_factor(id.grp, !!! recode.vec))

#same for raw data
stat.all.dur <- bind_rows(stat.spec.dur,
                          # mutate(filter(stat.spec.dur, kingdom == "Animalia"),
                          #                         id.grp = col.grp$group[5])
) %>%
  mutate(id.grp = recode_factor(id.grp, !!! recode.vec))

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
                                y_ax_explanation = NULL) {
  
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
                             #exclude Hymneoptera, Diptera and the Insects overall group
                             !(id.grp %in% c(excl.group.year, col.grp$group[5]))),
                      formula(str_glue("{quo_get_expr(y)} ~ {quo_get_expr(x)}"))),
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
                    xlim = c(1, nrow(metadata) + 1)) +
    scale_y_continuous(labels = mult_10_format(),
                      
    ) +
    scale_color_manual(name = "Group",
                       labels = col.group.stc$group,
                       values = col.group.stc$colour) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.text.x = element_text(size = 35,
                                 angle = 25,
                                 hjust = 1),
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
      plot.margin = unit(c(128, 128, 0, 0), "bigpts")
    )
  
}

png(here("Plots", "group_shifts_occurrence_duration.png"), width = 1600, height = 1000)

print(
  dur_slope_plot_save(xlab = "Group",
                      ylab = paste("Mean slope of dedacal activity duration",
                                   "[days/decade] (\u00B1 95% CI) [days/decade] (\u00B1 95% CI)",
                                   sep = "\n"))
)

dev.off()



png(here("Plots", "group_shifts_first_occurrence.png"), width = 1600, height = 1000)

print(
  dur_slope_plot_save(y = slope.first,
                      ymeta = mean.slope.first,
                      ymin = ci.min.first,
                      ymax = ci.max.first,
                      xlab = "Group", ylab = "Mean slope [days/decade] (\u00B1 95% CI)")
)

dev.off()


png(here("Plots", "group_shifts_last_occurrence.png"), width = 1600, height = 1000)

print(
  dur_slope_plot_save(y = slope.last,
                      ymeta = mean.slope.last,
                      ymin = ci.min.last,
                      ymax = ci.max.last,
                      xlab = "Group", ylab = "Mean slope [days/decade] (\u00B1 95% CI)")
)

dev.off()


ggsave(
  filename = here("Plots", "group_shifts_diff_occurrence.png"),
  plot = dur_slope_plot_save(
    y = slope.diff,
    ymeta = mean.slope.diff,
    ymin = ci.min.diff,
    ymax = ci.max.diff,
    xlab = "Group",
    ylab = paste(
      "Asymmetry of shifts first vs last DOY",
      "[days/decade] (\u00B1 95% CI)",
      sep = "\n"
    ),
    ylim = c(-5.5, 4.5),
    y_ax_explanation = "<-- First stronger | Last stronger -->"
  ),
  width = 20,
  height = 12
)



# Dirty Forest Plot Plants ------------------------------------------------

png(here("Plots", "forest_plot_plants_time.png"), width = 1000, height = 1700)


print(
  #dirty forest for time
  ggplot(data = stat.spec.time.plants) +
    geom_hline(yintercept = 0, lty = 3, col = col.line) +
    geom_pointrange(
      aes(
        x = reorder(species,-slope),
        y = slope,
        ymin = ci.min,
        ymax = ci.max
      ),
      size = 0.5,
      alpha = 0.5,
      col = col.plapoll$colour[2],
      pch = 18
    ) +
    coord_flip(ylim = c(min(stat.spec.time$ci.min), max(stat.spec.time$ci.max))) +
    labs(
      x = ("Species"),
      y = ("Temporal trends [days/decade]")
    ) +
    scale_y_continuous(labels = mult_10_format()) +
    facet_wrap(~ id.grp, scales = "free_y", ncol = 1) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.text.y = element_blank(),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.ticks.y = element_blank(),
      axis.line = element_line(colour = col.ax, size = 1.2),
      panel.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 30, color = "gray31")
    )
)

dev.off()


png(here("Plots", "forest_plot_plants_temp.png"), width = 1000, height = 1700)

print(
  #dirty forest for temp
  ggplot(data = stat.spec.temp.plants) +
    geom_hline(yintercept = 0, lty = 3, col = col.line) +
    geom_pointrange(
      aes(
        x = reorder(species,-slope),
        y = slope,
        ymin = ci.min,
        ymax = ci.max
      ),
      size = 0.5,
      alpha = 0.5,
      col = col.plapoll$colour[2],
      pch = 18
    ) +
    coord_flip(ylim = c(min(stat.spec.temp$ci.min), max(stat.spec.temp$ci.max))) +
    labs(
      x = ("Species"),
      y = ("Climate sensitivity [days/\u00B0C]")
    ) +
    facet_wrap(~ id.grp, scales = "free_y", ncol = 1) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.text.y = element_blank(),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.ticks.y = element_blank(),
      axis.line = element_line(colour = col.ax, size = 1.2),
      panel.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 30, color = "gray31")
    )
)


dev.off()


# Dirty Forest Plot Pollinators -------------------------------------------


png(here("Plots", "forest_plot_pollinators_time.png"), width = 1000, height = 1700)

print(
  #dirty forest for time
  ggplot(data = stat.spec.time.polls) +
    geom_pointrange(
      aes(
        x = reorder(species, -slope),
        y = slope,
        ymin = ci.min,
        ymax = ci.max,
        col = id.grp
      ),
      # alpha = 0.5,
      pch = 18,
      size = 2
    ) +
    scale_color_manual(
      name = "Group",
      labels = col.poll.stc$group,
      values = col.poll.stc$colour
    ) +
    labs(
      x = ("Species"),
      y = ("Temporal trends [days/decade]")
    ) +
    scale_y_continuous(labels = mult_10_format()) +
    coord_flip(ylim = c(min(stat.spec.time$ci.min), max(stat.spec.time$ci.max))) +
    facet_wrap(~ id.grp, scales = "free_y", ncol = 1) +
    geom_hline(yintercept = 0, lty = 3, col = col.line) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.text.y = element_blank(),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.ticks.y = element_blank(),
      axis.line = element_line(colour = col.ax, size = 1.2),
      panel.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 30, color = "gray31"),
      legend.position = "none"
    )
)

dev.off()


png(here("Plots", "forest_plot_pollinators_temp.png"), width = 1000, height = 1700)

print(
  #dirty forest for temp
  ggplot(data = stat.spec.temp.polls) +
    geom_pointrange(
      aes(
        x = reorder(species,-slope),
        y = slope,
        ymin = ci.min,
        ymax = ci.max,
        col = id.grp
      ),
      # alpha = 0.5,
      pch = 18,
      size = 2
    ) +
    scale_color_manual(
      name = "Group",
      labels = col.poll.stc$group,
      values = col.poll.stc$colour
    ) +
    labs(
      x = ("Species"),
      y = ("Climate sensitivity [days/\u00B0C]")
    ) +
    coord_flip(ylim = c(min(stat.spec.temp$ci.min), max(stat.spec.temp$ci.max))) +
    facet_wrap(~ id.grp, scales = "free_y", ncol = 1) +
    geom_hline(yintercept = 0, lty = 3, col = col.line) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.text.y = element_blank(),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.ticks.y = element_blank(),
      axis.line = element_line(colour = col.ax, size = 1.2),
      panel.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 30, color = "gray31"),
      legend.position = "none"
    )
)

dev.off()



# Boxplot Slope distribution ----------------------------------------------


png(here("Plots", "Boxplot Slope distribution time.png"), width = 1600, height = 1000)

print(
  #plot regression slopes for both kingdoms
  ggplot(stat.spec.time, aes(x = kingdom, y = slope)) +
    geom_boxplot(outlier.colour = "gray10",
                 outlier.size = 3,
                 fill = "gray10") +
    labs(x = "Group", y = "Slope [Days/year]") +
    scale_x_discrete(labels = c("Pollinators", "Plants")) +
    theme(axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(32, "bigpts"),
          strip.background = element_blank(),
          strip.text = element_text(size = 40),
    ) )

dev.off()

png(here("Plots", "Boxplot Slope distribution temp.png"), width = 1600, height = 1000)

print(
  #plot regression slopes for both kingdoms
  ggplot(stat.spec.temp, aes(x = kingdom, y = slope)) +
    geom_boxplot(outlier.colour = "gray10",
                 outlier.size = 3,
                 fill = "gray10") +
    labs(x = "Group", y = "Slope [Days/\u00B0C]") + 
    scale_x_discrete(labels = c("Pollinators", "Plants")) +
    theme(axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(32, "bigpts"),
          strip.background = element_blank(),
          strip.text = element_text(size = 40),
    ) )

dev.off()


# Barplot group slope numbers ---------------------------------------------


png(here("Plots", "Barplot group slope numbers.png"), width = 1600, height = 1000)

print(
  ggplot(stat.spec.temp.meta, aes(x = kingdom, y = n.slope)) +
    geom_bar(stat = "identity", fill = "gray10") +
    xlab("Group") + 
    ylab("# of Slopes") +
    scale_x_discrete(labels = c("Pollinators", "Plants")) +
    # coord_cartesian(ylim = c(0, 1000)) +
    theme(axis.title = element_text(size = 40),
          axis.text = element_text(size = 40),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.text = element_text(size = 40),
          legend.title = element_text(size = 40),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(32, "bigpts"),
          strip.background = element_blank(),
          strip.text = element_text(size = 40),
    ) )

dev.off()


#  Mean CI plot of TTests -------------------------------------------------


png(here("Plots", "Mean_CI_time.png"), width = 1000, height = 1000)

print(
  #for time
  ggplot(data = stat.spec.time.meta) +
    geom_hline(yintercept = 0, size = 1.5, lty = 3, col = col.line) +
    geom_jitter(
      data = stat.spec.time,
      aes(x = kingdom, y = slope),
      size = 5,
      alpha = 0.3,
      width = 0.3,
      col = "gray40"
    ) +
    geom_errorbar(
      aes(x = kingdom, ymin = ci.min, ymax = ci.max,
          col = kingdom),
      size = 2
    ) +
    geom_point(aes(x = kingdom, y = mean.slope,
                   col = kingdom),
               size = 8) +
    geom_text(
      aes(
        x = kingdom,
        y = ypos(stat.spec.time$slope),
        label = paste0("n = ", n.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(stat.spec.time, slope ~ kingdom, test = "t.test"),
      aes(
        x = level,
        y = ypos(stat.spec.time$slope, frac = 0.1),
        label = letter
      ),
      size = 13,
      col = "gray31"
    ) +
    labs(x = "Group", y = "Mean Slope [days/decade] (\u00B1 95% CI)") +
    scale_y_continuous(labels = mult_10_format()) +
    scale_color_manual(name = "Group",
                       labels = col.plapoll$group, 
                       values = col.plapoll$colour) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.text = element_text(size = 40),
      legend.title = element_text(size = 40),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 40),
      legend.position = "none"
    )
)

dev.off() 


png(here("Plots", "Mean_CI_temp.png"), width = 1000, height = 1000)

print(
  #for temp
  ggplot(data = stat.spec.temp.meta) +
    geom_hline(yintercept = 0, size = 1.5, lty = 3, col = col.line) +
    geom_jitter(
      data = stat.spec.temp,
      aes(x = kingdom, y = slope),
      size = 5,
      alpha = 0.3,
      width = 0.3,
      col = "gray40"
    ) +
    geom_errorbar(
      aes(x = kingdom, ymin = ci.min, ymax = ci.max,
          col = kingdom),
      size = 2
    ) +
    geom_point(aes(x = kingdom, y = mean.slope,
                   col = kingdom),
               size = 8) +
    geom_text(
      aes(
        x = kingdom,
        y = ypos(stat.spec.temp$slope),
        label = paste0("n = ", n.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(stat.spec.temp, slope ~ kingdom, test = "t.test"),
      aes(
        x = level,
        y = ypos(stat.spec.temp$slope, frac = 0.1),
        label = letter
      ),
      size = 13,
      col = "gray31"
    ) +
    labs(x = "Group", y = "Mean Slope [days/\u00B0C] (\u00B1 95% CI)") +
    scale_color_manual(name = "Group",
                       labels = col.plapoll$group, 
                       values = col.plapoll$colour) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.text = element_text(size = 40),
      legend.title = element_text(size = 40),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      legend.position = "none",
      strip.text = element_text(size = 40)
    )
)

dev.off()


#without raw data ++++++++++++++++++++++++++++++++++++++++


png(here("Plots", "Mean_CI_time_nopts.png"), width = 1000, height = 1000)

print(
  #for time
  ggplot(data = stat.spec.time.meta) +
    geom_errorbar(
      aes(x = kingdom, ymin = ci.min, ymax = ci.max,
          col = kingdom),
      size = 2
    ) +
    geom_point(aes(x = kingdom, y = mean.slope,
                   col = kingdom),
               size = 8) +
    geom_text(
      aes(
        x = kingdom,
        y = ypos(stat.spec.time.meta$ci.max),
        label = paste0("n = ", n.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(stat.spec.temp, slope ~ kingdom, test = "t.test"),
      aes(
        x = level,
        y = ypos(stat.spec.time.meta$ci.max, frac = 0.1),
        label = letter
      ),
      size = 13,
      col = "gray31"
    ) +
    labs(x = "Group", y = "Mean Slope [days/decade] (\u00B1 95% CI)") +
    scale_y_continuous(labels = mult_10_format()) +
    scale_color_manual(name = "Group",
                       labels = col.plapoll$group, 
                       values = col.plapoll$colour) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
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
    )
)

dev.off()


png(here("Plots", "Mean_CI_temp_nopts.png"), width = 1000, height = 1000)

print(
  #for temp
  ggplot(data = stat.spec.temp.meta) +
    geom_errorbar(
      aes(x = kingdom, ymin = ci.min, ymax = ci.max,
          col = kingdom),
      size = 2
    ) +
    geom_point(aes(x = kingdom, y = mean.slope,
                   col = kingdom),
               size = 8) +
    geom_text(
      aes(
        x = kingdom,
        y = ypos(stat.spec.temp.meta$ci.max),
        label = paste0("n = ", n.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(stat.spec.temp, slope ~ kingdom, test = "t.test"),
      aes(
        x = level,
        y = ypos(stat.spec.temp.meta$ci.max, frac = 0.1),
        label = letter
      ),
      size = 13,
      col = "gray31"
    ) +
    labs(x = "Group", y = "Mean Slope [days/\u00B0C] (\u00B1 95% CI)") +
    scale_color_manual(
      name = "Group",
      labels = col.plapoll$group,
      values = col.plapoll$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
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
    )
)

dev.off()


# Plot group mean slopes --------------------------------------------------

# bind group and pollinator overall meta data, replace the kingdom with the identifier for
# pollinators overall and also converting to factor, and rename levels to trivial names for the group
all.time.meta <- bind_rows(stat.group.time.meta,
                           # rename(stat.spec.time.meta[1,], id.grp = kingdom)
) %>%
  # mutate(id.grp = as.factor(str_replace_all(id.grp, "Animalia", col.grp$group[5]))) %>%
  # recode levels to common names (recode only takes name/value pairs, so !!! expansion necessary)
  mutate(id.grp = recode_factor(id.grp, !!! recode.vec)) %>%
  filter(!(id.grp %in% excl.trivial.year))


all.temp.meta <- bind_rows(stat.group.temp.meta,
                           # rename(stat.spec.temp.meta[1,], id.grp = kingdom)
) %>%
  # mutate(id.grp = as.factor(str_replace_all(id.grp, "Animalia", col.grp$group[5]))) %>%
  mutate(id.grp = recode_factor(id.grp, !!! recode.vec)) %>%
  filter(!(id.grp %in% excl.trivial.year))

#same for raw data
stat.all.time <- bind_rows(stat.spec.time,
                           # mutate(filter(stat.spec.time, kingdom == "Animalia"),
                           #                        id.grp = col.grp$group[5])
) %>%
  mutate(id.grp = recode_factor(id.grp, !!! recode.vec)) %>%
  filter(!(id.grp %in% excl.trivial.year))

stat.all.temp <- bind_rows(stat.spec.temp,
                           # mutate(filter(stat.spec.temp, kingdom == "Animalia"),
                           #                        id.grp = col.grp$group[5])
) %>%
  mutate(id.grp = recode_factor(id.grp, !!! recode.vec)) %>%
  filter(!(id.grp %in% excl.trivial.year))

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
                             !(id.grp %in% c(excl.group.year, col.grp$group[5]))),
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

ggsave(
  slope_plot_save(xlab = "Group", ylab = "Mean shift [days/decade] (\u00B1 95% CI)",
                  title = "Phenological shifts over time",
                  data_points = TRUE, scale_y_10 = TRUE),
  filename = here("Plots", "group_mean_slopes_time.png"), width = 20, height = 15,
  bg = "transparent"
)


ggsave(
  slope_plot_save(data = stat.all.temp, metadata = all.temp.meta,
                  xlab = "Group", ylab = "Mean shift [days/\u00B0C] (\u00B1 95% CI)",
                  data_points = TRUE, scale_y_10 = FALSE),
  filename = here("Plots", "group_mean_slopes_temp.png"), width = 20, height = 15,
  bg = "transparent"
)

#without raw data +++++++++++++++++++++++++++++

ggsave(
  slope_plot_save(xlab = "Group", ylab = "Mean shift [days/decade] (\u00B1 95% CI)",
                  title = "Phenological shifts over time",
                  data_points = FALSE, scale_y_10 = TRUE),
  filename = here("Plots", "group_mean_slopes_time_nopts.png"), width = 20, height = 15,
  bg = "transparent"
)

ggsave(
  slope_plot_save(data = stat.all.temp, metadata = all.temp.meta,
                  xlab = "Group", ylab = "Mean shift [days/\u00B0C] (\u00B1 95% CI)",
                  data_points = FALSE, scale_y_10 = FALSE),
  filename = here("Plots", "group_mean_slopes_temp_nopts.png"), width = 20, height = 15,
  bg = "transparent"
)


# Correlation of Time and temp slopes -------------------------------------

# rename factor levels for plotting 
stat.spec.both.plot <- stat.spec.both %>% 
  filter(!(id.grp %in% excl.group.year)) %>%
  mutate(id.grp = recode_factor(id.grp, !!! recode.vec))

# same for meta data
spec.both.meta.plot <- stat.spec.both.meta %>% 
  filter(!(id.grp %in% excl.group.year)) %>%
  mutate(id.grp = recode_factor(id.grp, !!! recode.vec))

# same for correlation results
cor.tests.plot <- cor.tests %>% 
  filter(!(id.grp %in% excl.group.year)) %>%
  mutate(id.grp = recode_factor(id.grp, !!! recode.vec))

ggsave(
  
  ggplot() +
    # zero axis (data arguments necessary, else it's acting up when trying to apply facets)
    geom_segment(aes(x = 0, xend = 0, y = -Inf, yend = ypos(stat.spec.both.plot$temp.slope,
                                                            stat.spec.both.plot$temp.ci.max,
                                                            frac = 0)),
                 data = stat.spec.both.plot,
                 size = 2,
                 col = "gray31", lty = 3) +
    geom_hline(yintercept = 0, lty = 3, col = col.line, size = 2,
               data = stat.spec.both.plot) +
    # raw data
    geom_errorbar(aes(x = time.slope,
                      ymin = temp.ci.min,
                      ymax = temp.ci.max),
                  data = stat.spec.both.plot,
                  size = 2,
                  alpha = 0.5,
                  col = col.pt) +
    geom_errorbarh(aes(y = temp.slope,
                       xmin = time.ci.min,
                       xmax = time.ci.max),
                   data = stat.spec.both.plot,
                   size = 2,
                   alpha = 0.5,
                   col = col.pt
    ) +
    geom_point(aes(time.slope, temp.slope, group = id.grp),
               data = stat.spec.both.plot,
               size = 6,
               alpha = 0.5,
               col = col.pt) +
    # lines for means down to axis for time
    geom_segment(aes(x = mean.slope.Year, xend = mean.slope.Year, y = -Inf, yend = mean.slope.Temperature,
                     col = id.grp),
                 data = spec.both.meta.plot,
                 size = 2,
                 alpha = 0.5) +
    # lines for means down to axis for temp
    geom_segment(aes(x = -Inf, xend = mean.slope.Year, y = mean.slope.Temperature, yend = mean.slope.Temperature,
                     col = id.grp),
                 data = spec.both.meta.plot,
                 size = 2,
                 alpha = 0.5) +
    # group means
    geom_errorbar(aes(x = mean.slope.Year,
                      ymin = ci.min.Temperature,
                      ymax = ci.max.Temperature, col = id.grp),
                  data = spec.both.meta.plot,
                  size = 3) +
    geom_errorbarh(aes(y = mean.slope.Temperature,
                       xmin = ci.min.Year,
                       xmax = ci.max.Year, col = id.grp),
                   data = spec.both.meta.plot,
                   size = 3) +
    geom_point(aes(mean.slope.Year, mean.slope.Temperature, col = id.grp),
               data = spec.both.meta.plot,
               size = 16,
               pch = 18
    ) +
    # # correltation coefficients and significance indication
    # geom_text(data = cor.tests.plot,
    #           aes(x = mean(c(min(stat.spec.both.plot$time.ci.min), max(stat.spec.both.plot$time.ci.max))),
    #               y = ypos(stat.spec.both.plot$temp.ci.max),
    #               label = str_glue("n = {n.slope.Year}, r =  {round(cor, 2)}\n{pval.sig}")),
    #           col = "gray38",
    #           size = 14) +
    facet_wrap( ~ id.grp) +
    labs(x = "Temporal trends [days / decade]",
         y = "Climate sensitivity [days / \u00B0C]") +
    scale_x_continuous(labels = mult_10_format()) +
    scale_color_manual(name = "Group",
                       values = col.grp.vec) +
    ylim(min(stat.spec.both.plot$temp.ci.min),
         max(stat.spec.both.plot$temp.ci.max) * 1.5) +
    theme(axis.title = element_text(size = 50),
          axis.text = element_text(size = 40),
          axis.ticks = element_line(colour = col.ax, size = 1.2),
          axis.ticks.length.y.left = unit(8, "bigpts"),
          axis.ticks.length.x.bottom = unit(8, "bigpts"),
          axis.line = element_line(colour = col.ax, size = 1.2),
          legend.position = "none",
          legend.text = element_text(size = 50),
          legend.title = element_text(size = 50),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing.x = unit(64, "bigpts"),
          strip.background = element_blank(),
          strip.text = element_text(size = 50),
          plot.margin = unit(c(200, 0, 0, 0), "bigpts")
    ), filename = here("Plots", "slopes_correlation.png"), width = 20, height = 12.5
)

# PollDeps reaction  -------------------------------------------------------

# make sure factor levels are in order
dat.occ.polldep$PollDep <- fct_relevel(dat.occ.polldep$PollDep,
                                       "Yes",
                                       "Intermediate",
                                       "No")

# get linear model results for plotting
lm.res.polldep <- lm.sum(dat.occ.polldep, PollDep) %>%
  mutate(breaks = cut(pval, breaks = c(0, 0.001, 1), labels = c("p < 0.001", "p > 0.001"),
                      right = FALSE),
         PollDep = fct_relevel(PollDep, c("Yes",
                                          "Intermediate",
                                          "No")))

ggsave(
  ggplot(data = dat.occ.polldep, aes(
    x = year,
    y = mean.doy,
    group = PollDep,
    col = PollDep
  )) +
    geom_point(size = 3, alpha = 0.15,
               position = position_dodge(width = 0.5)) +
    geom_abline(aes(intercept = intercept, slope = slope, lty = breaks, col = PollDep),
                lm.res.polldep,
                size = 2
    ) +
    # geom_smooth(method = lm, se = T, size = 2.5) +
    labs(x = NULL,
         y = "Peak flowering [DOY]") +
    scale_color_manual(
      name = "Group",
      labels = col.PollDep$group,
      values = col.PollDep$colour
    ) +
    scale_linetype(name = "Significance") +
    theme(axis.title = element_text(size = text_size_large),
          axis.text = element_text(size = text_size_large),
          axis.ticks = element_line(colour = col.ax, size = axis_ticks_size_large),
          axis.ticks.length = unit(axis_ticks_length_large, "bigpts"),
          axis.line = element_line(colour = col.ax, size = axis_size_large),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(size = legend_text_size_large),
          legend.title = element_text(size = legend_text_size_large),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.spacing = unit(64, "bigpts"), 
          strip.background = element_blank(),
          strip.text = element_text(size = text_size_large, color = col.note),
          plot.margin = unit(c(0, 64, 0, 0), "bigpts")
    ),
  filename = here("Plots", "PollDep overall trends over time.png"), width = 20, height = 12.5,
  bg = "transparent"
)


png(here("Plots", "PollDep overall trends over temp.png"), width = 1600, height = 1000)

print(
  #plot trends of collection day in relation to year by group
  ggplot(data = dat.occ.polldep, aes(
    x = y_mean_temp,
    y = mean.doy,
    group = PollDep,
    col = PollDep
  )) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = lm, se = T, size = 2.5) +
    labs(x = "Yearly mean temperature",
         y = "DOY") +
    scale_color_manual(
      name = "Group",
      labels = col.PollDep$group,
      values = col.PollDep$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.position = "bottom",
      legend.text = element_text(size = 40),
      legend.title = element_text(size = 40),
      panel.background = element_blank(),
      panel.grid.major = element_blank()
    )
)

dev.off()

# 
# png(here("Plots", "PollDep species trends over time.png"), width = 1600, height = 1000)
# 
# print(
#   #plot trends of collection day in relation to year
#   ggplot(data = dat.occ.polldep) +
#     facet_wrap( ~ species, drop = FALSE) +
#     geom_point(aes(x = year, y = mean.doy, col = PollDep),
#                size = 2) +
#     scale_color_grey(name = "Pollinator\ndependence", start = 0.8, end = 0.1) +
#     stat_smooth(data = filter(dat.occ.polldep, pval.time <= 0.05),
#                 aes(x = year, y = mean.doy, col = PollDep),
#                 geom ="line", method = lm,
#                 size = 2) +
#     geom_text(aes(x = 1985, y = 360, label = paste0("R\u00B2 =  ", round(rs.time, 3))),
#               size = 5, col = "gray31") +
#     labs(x = "Year", y = "DOY") +
#     theme(axis.title = element_text(size = 40),
#           axis.text = element_text(size = 40),
#           axis.text.x.bottom = element_text(size = 20, angle = 30),
#           axis.ticks = element_line(colour = col.ax, size = 1.2),
#           axis.ticks.length.y.left = unit(8, "bigpts"),
#           axis.ticks.length.x.bottom = unit(8, "bigpts"),
#           axis.line = element_line(colour = col.ax, size = 1.2),
#           legend.text = element_text(size = 40),
#           legend.position = "none",
#           legend.title = element_text(size = 40),
#           panel.background = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.spacing = unit(64, "bigpts"),
#           strip.background = element_blank(),
#           strip.text = element_text(size = 20),
#           plot.margin = unit(c(0, 64, 0, 0), "bigpts")) )
# 
#  dev.off()
# 
# 
# png(here("Plots", "PollDep species trends over temp.png"), width = 1600, height = 1000)
# 
# print(
#   #plot trends of collection day in relation to year
#   ggplot(data = dat.occ.polldep) +
#     facet_wrap( ~ species, drop = FALSE) +
#     geom_point(aes(x = y_mean_temp, y = mean.doy, col = PollDep),
#                size = 2) +
#     scale_color_grey(name = "Pollinator\ndependence", start = 0.8, end = 0.1) +
#     stat_smooth(data = filter(dat.occPollDep, pval.temp <= 0.05),
#                 aes(x = y_mean_temp, y = mean.doy, col = PollDep),
#                 geom ="line", method = lm,
#                 size = 2) +
#     geom_text(aes(x = 8.5, y = 360, label = paste0("R\u00B2 =  ", round(rs.temp, 3))),
#               size = 5, col = "gray31") +
#     labs(x = "Year", y = "DOY") +
#     theme(legend.position = "none",
#           axis.title = element_text(size = 40),
#           axis.text = element_text(size = 40),
#           axis.text.x.bottom = element_text(size = 40),
#           axis.ticks = element_line(colour = col.ax, size = 1.2),
#           axis.ticks.length.y.left = unit(8, "bigpts"),
#           axis.ticks.length.x.bottom = unit(8, "bigpts"),
#           axis.line = element_line(colour = col.ax, size = 1.2),
#           legend.text = element_text(size = 40),
#           legend.title = element_text(size = 40),
#           panel.background = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.spacing = unit(64, "bigpts"),
#           strip.background = element_blank(),
#           strip.text = element_text(size = 20),
#           plot.margin = unit(c(0, 64, 0, 0), "bigpts")) )
# 
# dev.off()


# Forest Plots PollDep -----------------------------------------------------


stat.spec.time.PollDep$PollDep <- fct_relevel(stat.spec.time.PollDep$PollDep, "Yes", "Intermediate", "No")

png(here("Plots", "Forest_PollDep_time.png"), width = 900, height = 1250)

print(
  #for time
  ggplot(
    data = stat.spec.time.PollDep,
    aes(
      x = reorder(species, -slope),
      y = slope,
      ymin = ci.min,
      ymax = ci.max,
      col = PollDep
    )
  ) +
    geom_pointrange(size = 1,
                    alpha = 0.7) +
    geom_hline(yintercept = 0, lty = 3, col = col.line) +
    coord_flip() +
    xlab("Species") +
    ylab("Slope [Days/year] (\u00B1 95% CI)") +
    scale_color_manual(
      name = "Group",
      labels = col.PollDep$group,
      values = col.PollDep$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 30),
      axis.text.y = element_blank(),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.y = element_blank(),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
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


stat.spec.temp.PollDep$PollDep <- fct_relevel(stat.spec.temp.PollDep$PollDep, "Yes", "Intermediate", "No")

png(here("Plots", "Forest_PollDep_temp.png"), width = 900, height = 1250)

print(
  #for temp
  ggplot(
    data = stat.spec.temp.PollDep,
    aes(
      x = reorder(species,-slope),
      y = slope,
      ymin = ci.min,
      ymax = ci.max,
      col = PollDep
    )
  ) +
    geom_pointrange(size = 1,
                    alpha = 0.15) +
    geom_hline(yintercept = 0, lty = 3, col = col.line) +
    coord_flip() +
    xlab("Species") + ylab("Slope [Days/\u00B0C] (\u00B1 95% CI)") +
    scale_color_manual(
      name = "Group",
      labels = col.PollDep$group,
      values = col.PollDep$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 30),
      axis.text.y = element_blank(),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.y = element_blank(),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
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


# Mean CI of DOY PollDep --------------------------------------------------

png(here("Plots", "Mean_CI_PollDep_mean_doy.png"), width = 1000, height = 1000)

print(
  # plot differences
  ggplot(polldep.spec.mean.doy, aes(PollDep, mean.doy, col = PollDep)) +
    geom_jitter(alpha = 0.2,
                col = "gray31") +
    stat_summary(
      size = 1.5,
      fun = mean,
      geom = "point"
    ) + 
    stat_summary(
      size = 1.5,
      fun.max = ci.max,
      fun.min = ci.min,
      geom = "errorbar"
    ) +
    # add group sizes
    geom_text(
      data = stat.polldep.mdoy.meta,
      aes(
        x = PollDep,
        y = ypos(polldep.spec.mean.doy$mean.doy),
        label = paste0("n = ", n.mean.doy)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(polldep.spec.mean.doy, mean.doy ~ PollDep),
      aes(
        x = level,
        y = ypos(polldep.spec.mean.doy$mean.doy, frac = 0.1),
        label = letter
      ),
      size = 15,
      col = "gray31"
    ) +
    labs(x = "Pollinator dependence", y = "Mean DOY (\u00B1 95% CI)") +
    scale_color_manual(
      name = "Group",
      labels = col.PollDep$group,
      values = col.PollDep$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.position = "none",
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 40)
    )
)

dev.off()

# Mean CI plot of PollDep ANOVA --------------------------------------------

stat.spec.time.PollDep.meta$PollDep <- fct_relevel(stat.spec.time.PollDep.meta$PollDep,
                                                   "Yes", "Intermediate", "No")

png(here("Plots", "Mean_CI_PollDep_time.png"), width = 1000, height = 1000)

print(
  #for time
  ggplot() +
    geom_hline(yintercept = 0, size = 1.5, lty = 3, col = col.line) +
    geom_jitter(
      data = stat.spec.time.PollDep,
      aes(x = PollDep, y = slope),
      size = 5,
      col = "gray40",
      alpha = 0.3,
      width = 0.3
    ) +
    geom_errorbar(
      data = stat.spec.time.PollDep.meta,
      aes(x = PollDep, ymin = ci.min, ymax = ci.max,
          col = PollDep),
      size = 2
    ) +
    geom_point(
      data = stat.spec.time.PollDep.meta,
      aes(x = PollDep, y = mean.slope,
          col = PollDep),
      size = 8
    ) +
    geom_text(
      data = stat.spec.temp.PollDep.meta,
      aes(
        x = PollDep,
        y = ypos(stat.spec.time.PollDep$slope),
        label = paste0("n = ", n.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(stat.spec.time.PollDep, slope ~ PollDep),
      aes(
        x = level,
        y = ypos(stat.spec.time.PollDep$slope, frac = 0.1),
        label = letter
      ),
      size = 15,
      col = "gray31"
    ) +
    labs(x = "Pollinator dependence", y = "Mean Slope [days/decade] (\u00B1 95% CI)") +
    scale_y_continuous(labels = mult_10_format()) +
    scale_color_manual(
      name = "Group",
      labels = col.PollDep$group,
      values = col.PollDep$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.position = "none",
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 40)
    )
  
)

dev.off()


stat.spec.temp.PollDep.meta$PollDep <- fct_relevel(stat.spec.temp.PollDep.meta$PollDep, "Yes", "Intermediate", "No")

png(here("Plots", "Mean_CI_PollDep_temp.png"), width = 1000, height = 1000)

print(
  
  #for temp
  ggplot() +
    geom_hline(yintercept = 0, size = 1.5, lty = 3, col = col.line) +
    geom_jitter(
      data = stat.spec.temp.PollDep,
      aes(x = PollDep, y = slope),
      size = 5,
      col = "gray10",
      alpha = 0.3,
      width = 0.15
    ) +
    geom_errorbar(
      data = stat.spec.temp.PollDep.meta,
      aes(
        x = PollDep,
        ymin = ci.min,
        ymax = ci.max,
        col = PollDep
      ),
      size = 2
    ) +
    geom_point(
      data = stat.spec.temp.PollDep.meta,
      aes(x = PollDep, y = mean.slope,
          col = PollDep),
      size = 8
    ) +
    geom_text(
      data = stat.spec.temp.PollDep.meta,
      aes(
        x = PollDep,
        y = ypos(stat.spec.temp.PollDep$slope),
        label = paste0("n = ", n.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(stat.spec.temp.PollDep, slope ~ PollDep),
      aes(
        x = level,
        y = ypos(stat.spec.temp.PollDep$slope, frac = 0.1),
        label = letter
      ),
      size = 15,
      col = "gray31"
    ) +
    labs(x = "Pollinator dependence", y = "Mean Slope [days/\u00B0C] (\u00B1 95% CI)") +
    scale_color_manual(
      name = "Group",
      labels = col.PollDep$group,
      values = col.PollDep$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.position = "none",
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 40)
    )
)

dev.off()


# without raw data +++++++++++++++++++++++++++++++++++++


png(here("Plots", "Mean_CI_PollDep_time_nopts.png"), width = 1000, height = 1000)

print(
  #for time
  ggplot() +
    # geom_hline(yintercept = 0, lty = 3, col = col.line) +
    geom_errorbar(
      data = stat.spec.time.PollDep.meta,
      aes(x = PollDep, ymin = ci.min, ymax = ci.max,
          col = PollDep),
      size = 2
    ) +
    geom_point(
      data = stat.spec.time.PollDep.meta,
      aes(x = PollDep, y = mean.slope,
          col = PollDep),
      size = 8
    ) +
    geom_text(
      data = stat.spec.temp.PollDep.meta,
      aes(
        x = PollDep,
        y = ypos(stat.spec.time.PollDep.meta$ci.max),
        label = paste0("n = ", n.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(stat.spec.time.PollDep, slope ~ PollDep),
      aes(
        x = level,
        y = ypos(stat.spec.time.PollDep.meta$ci.max, frac = 0.1),
        label = letter
      ),
      size = 15,
      col = "gray31"
    ) +
    labs(x = "Pollinator dependence", y = "Mean Slope [days/decade] (\u00B1 95% CI)") +
    scale_y_continuous(labels = mult_10_format()) +
    scale_color_manual(
      name = "Group",
      labels = col.PollDep$group,
      values = col.PollDep$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.position = "none",
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 40)
    )
)

dev.off()


png(here("Plots", "Mean_CI_PollDep_temp_nopts.png"), width = 1000, height = 1000)

print(
  #for temp
  ggplot() +
    # geom_hline(yintercept = 0, lty = 3, col = col.line) +
    geom_errorbar(
      data = stat.spec.temp.PollDep.meta,
      aes(x = PollDep, ymin = ci.min, ymax = ci.max,
          col = PollDep),
      size = 2
    ) +
    geom_point(
      data = stat.spec.temp.PollDep.meta,
      aes(x = PollDep, y = mean.slope,
          col = PollDep),
      size = 8,
    ) +
    geom_text(
      data = stat.spec.temp.PollDep.meta,
      aes(
        x = PollDep,
        y = ypos(stat.spec.temp.PollDep.meta$ci.max),
        label = paste0("n = ", n.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(stat.spec.temp.PollDep, slope ~ PollDep),
      aes(
        x = level,
        y = ypos(stat.spec.temp.PollDep.meta$ci.max, frac = 0.1),
        label = letter
      ),
      size = 15,
      col = "gray31"
    ) +
    labs(x = "Pollinator dependence", y = "Mean Slope [days/\u00B0C] (\u00B1 95% CI)") +
    scale_color_manual(
      name = "Group",
      labels = col.PollDep$group,
      values = col.PollDep$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 40),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.y.left = unit(8, "bigpts"),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.position = "none",
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 40)
    )
)

dev.off()


# Trait Plots -------------------------------------------------------------

if (any(c("full", "traits") %in% opts)) {
  
  for (abb_trait in abbreviate(str_to_lower(names(select(dat.occ, LifeForm:Habitat))), 7, strict = TRUE)) {
    
    traitplot(abb_trait)
    
  }
  
  
}

# always do mean flowering month plot
traitplot("mnflmnt")

#plot mean flowering month plot properly
png(here("Plots/traits",
         "Mean_CI_mnflmnt_time.png"), width = 1600, height = 1000)

plots_mnflmnt[[1]] +
  xlab("Peak month of flowering")

dev.off()

# Interactions: Indidividual slopes ---------------------------------------



png(here("Plots", "group_decadal_slopes.png"), width = 1600, height = 1000)

print(
  ggplot(
    dat.occ.dec %>%
      mutate(id.grp = recode_factor(id.grp, !!! recode.vec.int)) %>%
      group_by(species) %>%
      #plot only species with records in all decades
      filter(n() >= thr.dec)
  ) +
    geom_ribbon(aes(
      x = decade,
      ymin = ci.min.doy,
      ymax = ci.max.doy,
      group = species,
      fill = id.grp
    ),
    alpha = 0.3) +
    geom_line(aes(x = decade, y = mean.doy, group = species),
              size = 1.4, alpha = 0.5) +
    geom_point(aes(x = decade, y = mean.doy, col = id.grp),
               size = 4) +
    labs(x = "Decade", y = "Mean decadal DOY  (\u00B1 95% CI)") +
    scale_color_manual(
      name = "Group",
      aesthetics = c("color", "fill"),
      values = col.int3$colour,
    ) +
    facet_wrap( ~ id.grp, nrow = 2) +
    theme(
      axis.title = element_text(size = 40),
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
      panel.grid.major.x = element_blank()
    )
)

dev.off()


# Interactions: Slope differences -----------------------------------------


png(here("Plots", "mean_slope_differences_time.png"), width = 1000, height = 1000)

print(
  #plot results for time
  ggplot() +
    geom_jitter(
      data = dat.int.time,
      aes(x = group, y = diff.slope),
      size = 5,
      alpha = 0.3,
      width = 0.3,
      col = "gray40"
    ) +
    geom_hline(yintercept = 0, linetype = 3,
               col = "gray31", size = 2) +
    geom_errorbar(
      data = dat.int.meta.time,
      aes(x = group, ymin = ci.min, ymax = ci.max,
          col = group),
      size = 2
    ) +
    geom_point(
      data = dat.int.meta.time,
      aes(x = group, y = mean.diff.slope,
          col = group),
      size = 8
    ) +
    geom_text(
      data = dat.int.meta.time,
      aes(
        x = group,
        y = ypos(dat.int.time$diff.slope),
        label = paste0("n = ", N.diff.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(dat.int.time, diff.slope ~ group),
      aes(
        x = level,
        y = ypos(dat.int.time$diff.slope, frac = 0.1),
        label = letter
      ),
      size = 15,
      col = "gray31"
    ) +
    labs(x = "Group",
         y = "Mean Slope difference [days/decade] (\u00B1 95% CI)") +
    scale_color_manual(
      name = "Group",
      labels = col.int$group,
      values = col.int$colour
    ) +
    scale_y_continuous(labels = mult_10_format()) +
    theme(
      axis.title = element_text(size = 40),
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
      panel.grid.major.x = element_blank()
    )
)

dev.off()


png(here("Plots", "mean_slope_differences_temp.png"), width = 1000, height = 1000)

print(
  #plot results for temp
  ggplot() +
    geom_jitter(
      data = dat.int.temp,
      aes(x = group, y = diff.slope),
      size = 5,
      alpha = 0.3,
      width = 0.3,
      col = "gray40"
    ) +
    geom_hline(yintercept = 0, linetype = 3,
               col = "gray31", size = 2) +
    geom_errorbar(
      data = dat.int.meta.temp,
      aes(x = group, ymin = ci.min, ymax = ci.max,
          col = group),
      size = 2
    ) +
    geom_point(
      data = dat.int.meta.temp,
      aes(x = group, y = mean.diff.slope,
          col = group),
      size = 8
    ) +
    geom_text(
      data = dat.int.meta.temp,
      aes(
        x = group,
        y = ypos(dat.int.temp$diff.slope),
        label = paste0("n = ", N.diff.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(dat.int.temp, diff.slope ~ group),
      aes(
        x = level,
        y = ypos(dat.int.temp$diff.slope, frac = 0.1),
        label = letter
      ),
      size = 15,
      col = "gray31"
    ) +
    labs(x = "Group",
         y = "Mean Slope difference [days/\u00B0C] (\u00B1 95% CI)") +
    scale_color_manual(
      name = "Group",
      labels = col.int$group,
      values = col.int$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
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
      panel.grid.major.x = element_blank()
    )
)

dev.off()


# without raw data +++++++++++++++++++++++++++++++++++++



png(here("Plots", "mean_slope_differences_time_nopts.png"), width = 1000, height = 1000)

print(
  #plot results for time
  ggplot() +
    geom_hline(yintercept = 0, linetype = 3,
               col = "gray31", size = 2) +
    geom_errorbar(
      data = dat.int.meta.time,
      aes(x = group, ymin = ci.min, ymax = ci.max,
          col = group),
      size = 2
    ) +
    geom_point(
      data = dat.int.meta.time,
      aes(x = group, y = mean.diff.slope,
          col = group),
      size = 8
    ) +
    geom_text(
      data = dat.int.meta.time,
      aes(
        x = group,
        y = ypos(dat.int.meta.time$ci.max),
        label = paste0("n = ", N.diff.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(dat.int.temp, diff.slope ~ group),
      aes(
        x = level,
        y = ypos(dat.int.meta.time$ci.max, frac = 0.1),
        label = letter
      ),
      size = 15,
      col = "gray31"
    ) +
    labs(x = "Group",
         y = "Mean Slope difference [days/decade] (\u00B1 95% CI)") +
    scale_y_continuous(labels = mult_10_format()) +
    scale_color_manual(
      name = "Group",
      labels = col.int$group,
      values = col.int$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
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
      panel.grid.major.x = element_blank(),
      plot.margin = unit(c(24, 0, 0, 0), "bigpts")
    )
)

dev.off()


png(here("Plots", "mean_slope_differences_temp_nopts.png"), width = 1000, height = 1000)

print(
  #plot results for temp
  ggplot() +
    geom_hline(yintercept = 0, linetype = 3,
               col = "gray31", size = 2) +
    geom_errorbar(
      data = dat.int.meta.temp,
      aes(x = group, ymin = ci.min, ymax = ci.max,
          col = group),
      size = 2
    ) +
    geom_point(
      data = dat.int.meta.temp,
      aes(x = group, y = mean.diff.slope,
          col = group),
      size = 8
    ) +
    geom_text(
      data = dat.int.meta.temp,
      aes(
        x = group,
        y = ypos(dat.int.meta.temp$ci.max),
        label = paste0("n = ", N.diff.slope)
      ),
      size = 15,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(dat.int.temp, diff.slope ~ group),
      aes(
        x = level,
        y = ypos(dat.int.meta.temp$ci.max, frac = 0.1),
        label = letter
      ),
      size = 15,
      col = "gray31"
    ) +
    labs(x = "Group",
         y = "Mean Slope difference [days/\u00B0C] (\u00B1 95% CI)") +
    scale_color_manual(
      name = "Group",
      labels = col.int$group,
      values = col.int$colour
    ) +
    theme(
      axis.title = element_text(size = 40),
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
      panel.grid.major.x = element_blank()
    )
)

dev.off()


# Interactions: Synchrony differences -------------------------------------


# make sure the subdirectory exists
dir.check(here("plots/interactions/doy_diff"))

recode_vec_int_long <- c("Hoverfly" = "Hoverfly - Plant", "Bee" = "Bee - Plant",
                         "Butterfly" = "Butterfly - Plant")


# png(here("Plots", "mean_doy_differences_overspec.png"), width = 1600, height = 1000)
# 
# print(
#   #all interaction plots in one
#   ggplot(int.spec.dec, aes(
#     x = decade,
#     y = synchrony,
#     group = id,
#     col = group
#   )) +
#     geom_line(size = 1.5, alpha = 0.1) +
#     geom_point(size = 4, alpha = 0.2) +
#     geom_smooth(
#       aes(
#         x = decade,
#         y = synchrony,
#         group = group,
#         col = group,
#         size = 1.5
#       ),
#       method = "lm",
#       se = FALSE,
#       size = 2
#     ) +
#     geom_hline(yintercept = 0, lty = 3, size = 1.5, col = col.line) +
#     scale_color_manual(
#       name = "Group",
#       labels = col.int$group,
#       values = col.int$colour
#     ) +
#     labs(x = "Decade",
#          y = "Asynchrony") +
#     coord_cartesian(xlim = c(
#       min(dat.occ.dec$decade), max(dat.occ.dec$decade)
#     )) +
#     theme(
#       axis.title = element_text(size = 40),
#       axis.text = element_text(size = 40),
#       axis.text.x = element_text(angle = 30, hjust = 1),
#       axis.ticks = element_line(colour = col.ax, size = 1.2),
#       axis.ticks.length.y.left = unit(8, "bigpts"),
#       axis.ticks.length.x.bottom = unit(8, "bigpts"),
#       axis.line = element_line(colour = col.ax, size = 1.2),
#       plot.title = element_text(size = 40, hjust = 0.5),
#       legend.text = element_text(size = 40),
#       legend.title = element_text(size = 40),
#       legend.position = "bottom",
#       panel.background = element_rect(fill = "white"),
#       panel.grid.major = element_blank()
#     )
# )
# 
# dev.off()


#now faceted by groups

# first calculate linear model results for synchrony ~ decade
lm.res.int <- lm.sum(int.spec.dec, group, synchrony ~ decade) %>%
  mutate(breaks = cut(pval, breaks = c(0, 0.05, 1), labels = c("p < 0.05", "p > 0.05"),
                      right = FALSE),
         group = fct_relevel(group, c("Hoverfly", "Bee", "Butterfly"))) %>% 
  mutate(group = recode_factor(group, !!! recode_vec_int_long))

# generate data frame for plotting
int.spec.dec.plot <- int.spec.dec %>% 
  mutate(group = recode_factor(group, !!! recode_vec_int_long))


ggsave(
  ggplot(int.spec.dec.plot,
         aes(
           x = decade,
           y = synchrony,
           group = id,
           # col = group
         ),
         col = col.pt) +
    # geom_hline(yintercept = 0, size = 1.5, lty = 3, col = col.line) +
    geom_line(size = 1.5, alpha = 0.1,
              col = col.line,
              show.legend = FALSE) +
    geom_point(size = 4, alpha = 0.3,
               col = col.pt,
               show.legend = FALSE) +
    stat_summary(
      data = int.spec.dec.plot,
      aes(
        x = decade,
        y = synchrony,
        group = group,
        col = group
      ),
      size = 4,
      pch = 18,
      fun = mean,
      fun.max = ci.max,
      fun.min = ci.min,
      # col = "gray31",
      show.legend = FALSE
    ) +
    geom_abline(aes(intercept = intercept, slope = slope, lty = breaks, col = group),
                lm.res.int,
                # col = "gray31",
                size = line_size_large
    ) +
    scale_x_continuous(labels = decade_format()) +
    scale_color_manual(
      name = "Group",
      aesthetics = c("color", "fill"),
      values = col.int$colour,
      labels = col.int$group
    ) +
    scale_linetype_manual(values = c("p < 0.05" = 1, "p > 0.05" = 2)) +
    labs(x = NULL,
         y = "Mean plant-pollinator asynchrony [days]") +
    coord_cartesian(xlim = c(min(dat.occ.dec$decade), max(dat.occ.dec$decade))) +
    facet_wrap( ~ group, nrow = 3) +
    theme(axis.title = element_text(size = text_size_large),
          axis.text = element_text(size = text_size_large),
          # axis.text.x =  element_text(hjust = 1, angle = 30),
          axis.ticks = element_line(colour = col.ax, size = axis_ticks_size_large),
          axis.ticks.length = unit(axis_ticks_length_large, "bigpts"),
          axis.line = element_line(colour = col.ax, size = axis_size_large),
          legend.position = "none",
          legend.text = element_text(size = legend_text_size_large),
          legend.title = element_text(size = legend_text_size_large),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = text_size_large, color = col.note),
          plot.margin = unit(c(0,64,0,0), "bigpts")
          ),
  filename = here("Plots", "mean_doy_differences_overall_facet.png"), height = 20, width = 16
)

# plot fractions of interactions with insect earlier than plant

# first calculate linear model results for ins.early ~ decade
lm.res.ins.earl <- lm.sum(int.mean.time.grp, group, ins.early.frac ~ decade) %>%
  mutate(breaks = cut(pval, breaks = c(0, 0.05, 1), labels = c("p < 0.05", "p > 0.05"),
                      right = FALSE),
         # group = fct_relevel(group, col.int)
         ) %>% 
  mutate(group = recode_factor(group, !!! recode_vec_int_long))

int.mean.time.plot <- int.mean.time  %>% 
  mutate(group = recode_factor(group, !!! recode_vec_int_long))

# percentages of insect partner earlier (excluding overall data)
ggsave(
  ggplot(filter(int.mean.time.plot, group != "Overall"), 
         aes(decade, ins.early.frac, col = group)) + 
    geom_hline(yintercept = 0.5, linetype = 3,
               size = hline_size_large, col = "gray31") +
    geom_point(size = 8, show.legend = FALSE) +
    # geom_smooth(method = "lm", size = 2) +
    geom_abline(aes(intercept = intercept, slope = slope, lty = breaks, col = group),
                lm.res.ins.earl,
                col = "gray31",
                size = 2
    ) +
    facet_wrap(~group, nrow = 3) +
    scale_color_manual(
      name = "Group",
      aesthetics = c("color", "fill"),
      labels = col.int$group,
      values = col.int$colour
    ) +
    scale_linetype_manual(name = "Significance",
                          values = c("p < 0.05" = 1, "p > 0.05" = 2)) +
    scale_x_continuous(labels = decade_format()) +
    scale_y_continuous(labels = decimal_to_percent_format()) +
    labs(x = NULL,
         y = "\u0025 of interactions with earlier activity of insects") + 
    coord_cartesian(ylim = c(0, 1)) +
    theme(axis.title = element_text(size = text_size_large),
          axis.text = element_text(size = text_size_large),
          # axis.text.x =  element_text(hjust = 1, angle = 30),
          axis.ticks = element_line(colour = col.ax, size = axis_ticks_size_large),
          axis.ticks.length = unit(axis_ticks_length_large, "bigpts"),
          axis.line = element_line(colour = col.ax, size = axis_size_large),
          legend.position = "none",
          legend.text = element_text(size = legend_text_size_large),
          legend.title = element_text(size = legend_text_size_large),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = text_size_large, color = col.note),
          plot.margin = unit(c(0,64,0,0), "bigpts")
          ),
  filename = here("plots", "frac_ins_earlier_time.png"), height = 20, width = 16
)

# 
# png(here("Plots", "mean_doy_differences_overall_decade_means.png"), height = 1000, width = 1600)
# 
# print(
#   #group means of interactions
#   ggplot(int.mean.time,
#          aes(
#            x = decade,
#            y = mean.synchrony,
#            col = group
#          )) +
#     geom_line(size = 2, alpha = 0.6, lty = 3) +
#     geom_errorbar(aes(ymin = ci.min.synchrony, ymax = ci.max.synchrony),
#                   size = 1.5,
#                   width = 4,
#                   alpha = 0.7) +
#     geom_point(size = 5) +
#     geom_smooth(method = "lm", size = 2 , se = FALSE) +
#     scale_color_manual(
#       name = "Group",
#       aesthetics = c("color", "fill"),
#       values = col.grp.vec) +
#     labs(x = "Decade",
#          y = "DOY Pla - Poll\nDifference") +
#     coord_cartesian(xlim = c(min(dat.occ.dec$decade), max(dat.occ.dec$decade))) +
#     # facet_wrap( ~ group, nrow = 3) +
#     theme(
#       axis.title = element_text(size = 40),
#       axis.text = element_text(size = 40),
#       axis.ticks = element_line(colour = col.ax, size = 1.2),
#       axis.ticks.length.x.bottom = unit(8, "bigpts"),
#       axis.line = element_line(colour = col.ax, size = 1.2),
#       legend.position = "bottom",
#       legend.text = element_text(size = 40),
#       legend.title = element_text(size = 40),
#       panel.background = element_blank(),
#       panel.spacing = unit(32, "bigpts"),
#       strip.background = element_blank(),
#       strip.text = element_text(size = 40),
#       panel.grid.major.y = element_blank(),
#       panel.grid.major.x = element_blank()
#     )
# )
# 
# dev.off()
# 
# 
# png(here("Plots", "mean_doy_differences_overall_all_in_one.png"), width = 1600, height = 1000)
# 
# print(
#   #both together
#   ggplot() +
#     geom_point(data = int.spec.dec,
#                aes(decade, synchrony,
#                    col = group),
#                size = 2,
#                alpha = 0.1) +
#     geom_line(data = int.spec.dec,
#               aes(decade, synchrony, group = id,
#                   col = group),
#               size = 1.5,
#               alpha = 0.05,
#               lty = 3) +
#     geom_hline(yintercept = 0, linetype = 3,
#                col = "gray31", size = 2) +
#     geom_errorbar(data = int.mean.time,
#                   aes(decade, ymin = ci.min.synchrony,
#                       ymax = ci.max.synchrony,
#                       col = group),
#                   size = 1.2) +
#     geom_point(data = int.mean.time,
#                aes(decade, mean.synchrony, col = group),
#                size = 8) +
#     geom_line(data = int.mean.time,
#               aes(decade, mean.synchrony, col = group),
#               size = 1.2) +
#     # geom_vline(data = eqs.synchrony, aes(xintercept = xintercept, col = group), lty = 2) +
#     scale_color_manual(
#       name = "Group",
#       aesthetics = c("color", "fill"),
#       values = arrange(col.int2, group)$colour
#     ) +
#     scale_shape_manual(name = "Group",
#                        values = c(19, 19, 19, 18)
#     ) +
#     labs(x = "Decade",
#          y = "Mean Asynchrony\n (\u00B1 95% CI)") + 
#     theme(
#       axis.title = element_text(size = 40),
#       axis.text = element_text(size = 40),
#       axis.ticks = element_line(colour = col.ax, size = 1.2),
#       axis.ticks.length.x.bottom = unit(8, "bigpts"),
#       axis.line = element_line(colour = col.ax, size = 1.2),
#       legend.position = "bottom",
#       legend.text = element_text(size = 40),
#       legend.title = element_text(size = 40),
#       panel.background = element_blank(),
#       panel.spacing = unit(32, "bigpts"),
#       strip.background = element_blank(),
#       strip.text = element_text(size = 40),
#       panel.grid.major.y = element_blank(),
#       panel.grid.major.x = element_blank()
#     )
# )
# 
# dev.off()


#plots by insect species
for (s in sort(unique(int.spec.dec$poll))) {
  
  int.s.dec <- filter(int.spec.dec, poll == s)
  
  png(here("Plots/interactions/doy_diff", paste("mean_doy_differences", s, ".png")), width = 1600, height = 1000)
  
  print(
    ggplot(int.s.dec, aes(x = decade, y = synchrony, group = plant)) +
      geom_hline(yintercept = 0, lty = 3, size = 1.5, col = col.line) +
      geom_line(size = 1.5, alpha = 0.1) +
      geom_point(size = 4, alpha = 0.4) +
      geom_smooth(method = "lm", size = 1.5, se = FALSE, alpha = 0.5, col = "gray31") +
      stat_summary(
        data = int.s.dec,
        aes(
          x = decade,
          y = synchrony,
          col = group,
          group = poll
        ),
        size = 2,
        pch = 18,
        fun = mean,
        fun.max = ci.max,
        fun.min = ci.min
      ) +
      geom_smooth(aes(x = decade,
                      y = synchrony,
                      group = poll,
                      col = group),
                  method = "lm", size = 1.5, se = TRUE) +
      labs(title = s,
           subtitle = paste0("n = ", length(unique(int.s.dec$plant))),
           x = "Decade",
           y = "DOY Pla - Poll Difference") +
      scale_color_manual(
        name = "Group",
        values = filter(col.int.stc, group == int.s.dec$group[1])$colour
      ) +
      coord_cartesian(ylim = c(1.1 * max(int.spec.dec$synchrony),
                               1.1 * min(int.spec.dec$synchrony)))+
      theme(text = element_text(size = 40),
            axis.text.x = element_text(angle = 30, hjust = 1),
            axis.ticks = element_line(colour = col.ax, size = 1.2),
            axis.ticks.length.y.left = unit(8, "bigpts"),
            axis.ticks.length.x.bottom = unit(8, "bigpts"),
            axis.line = element_line(colour = col.ax, size = 1.2),
            plot.title = element_text(size = 40, hjust = 0.5),
            plot.subtitle = element_text(size = 30, hjust = 0.5),
            legend.position = "none",
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_blank())
  )
  
  dev.off()
  
}

# Interactions: Mean Synchrony differences --------------------------------

# for main publication
ggsave(
  ggplot() +
    geom_hline(
      size = 1.5,
      yintercept = 0,
      lty = 2,
      col = "gray31"
    ) +
    geom_point(
      data = stat.int.time,
      aes(x = group, y = synchrony.slope, 
          # shape = decades
      ),
      size = 3,
      alpha = 0.3,
      col = "gray40",
      # position = "jitter"
      position = position_jitter(width = 0.2)
    ) +
    geom_errorbar(
      data = stat.int.meta,
      aes(
        x = group,
        ymin = ci.min,
        ymax = ci.max,
        col = group
      ),
      size = 2,
      position = position_dodge(width = 1)
    ) +
    geom_point(
      data = stat.int.meta,
      aes(x = group, y = mean.synchrony.slope, col = group),
      size = 8,
      position = position_dodge(width = 1)
    ) +
    # geom_text(
    #   data = stat.int.meta,
    #   aes(
    #     x = group,
    #     y = ypos(stat.int.time$synchrony.slope, frac = 0.4),
    #     label = paste0("n = ", N.synchrony.slope)
    #   ),
    #   size = annot_text,
    #   col = "gray31"
    # ) +
    # #add labels for differing factor levels
    # geom_text(
    #   data = pairdiff(stat.int.time, synchrony.slope ~ group, level_order = levels(stat.int.time$group)),
    #   aes(x = level,
    #       y = ypos(c(stat.int.meta$ci.max, stat.int.time$synchrony.slope), frac = 0.15), # see above
    #       label = letter),
    #   size = annot_text,
    #   col = "gray31"
    # ) +
    scale_color_manual(
      name = "Group",
      labels = col.int$group,
      values = col.int$colour
    ) +
    # #giving all groups the same shape again
    # scale_shape_manual(name = "Period", values = c(19, 19, 19),
    #                    guide = "none") +
    labs(
      x = NULL,
      y = "Change in synchrony\n[days/decade") +
    scale_x_discrete(labels = recode_vec_int_long) +
    scale_y_continuous(labels = mult_10_format()) +
    theme(
      axis.title = element_text(size = text_size_large),
      axis.text = element_text(size = text_size_large),
      axis.ticks = element_line(colour = col.ax, size = axis_ticks_size_large),
      axis.ticks.length = unit(axis_ticks_length_large, "bigpts"),
      axis.line = element_line(colour = col.ax, size = axis_size_large),
      legend.position = "none",
      legend.text = element_text(size = text_size_large),
      legend.title = element_text(size = text_size_large),
      panel.background = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = text_size_large),
      panel.grid = element_blank(),
      plot.margin = unit(c(150, 32, 0, 32), "bigpts")
    ),
  filename = here("Plots", "group_mean_doy_differences.png"), width = 20, height = 12.5,
  bg = "transparent"
)

# for graphical abstract

ggsave(
  ggplot() +
    geom_hline(
      size = 1.5,
      yintercept = 0,
      lty = 2,
      col = "gray31"
    ) +
    geom_point(
      data = stat.int.time,
      aes(x = group, y = synchrony.slope, 
          # shape = decades
      ),
      size = 3,
      alpha = 0.3,
      col = "gray40",
      # position = "jitter"
      position = position_jitter(width = 0.2)
    ) +
    geom_errorbar(
      data = stat.int.meta,
      aes(
        x = group,
        ymin = ci.min,
        ymax = ci.max,
        col = group
      ),
      size = 2,
      position = position_dodge(width = 1)
    ) +
    geom_point(
      data = stat.int.meta,
      aes(x = group, y = mean.synchrony.slope, col = group),
      size = 8,
      position = position_dodge(width = 1)
    ) +
    # geom_text(
    #   data = stat.int.meta,
    #   aes(
    #     x = group,
    #     y = ypos(stat.int.time$synchrony.slope, frac = 0.4),
    #     label = paste0("n = ", N.synchrony.slope)
    #   ),
    #   size = annot_text,
    #   col = "gray31"
    # ) +
    # #add labels for differing factor levels
    # geom_text(
    #   data = pairdiff(stat.int.time, synchrony.slope ~ group, level_order = levels(stat.int.time$group)),
    #   aes(x = level,
    #       y = ypos(c(stat.int.meta$ci.max, stat.int.time$synchrony.slope), frac = 0.15), # see above
    #       label = letter),
    #   size = annot_text,
    #   col = "gray31"
    # ) +
    scale_color_manual(
      name = "Group",
      labels = col.int$group,
      values = col.int$colour
    ) +
    # #giving all groups the same shape again
    # scale_shape_manual(name = "Period", values = c(19, 19, 19),
    #                    guide = "none") +
    labs(
      title = paste("Climate change shifts synchrony",
                    "of plant-pollinator interactions",
                    sep = "\n"),
      x = NULL,
      y = paste("Changes in synchrony", "[days/decade], axis cut",
                sep = "\n")) +
    scale_x_discrete(labels = recode_vec_int_long) +
    scale_y_continuous(labels = mult_10_format(),
                       limits = c(-2, 2)
                       ) +
    theme(
      axis.title = element_text(size = text_size_large),
      axis.text = element_text(size = text_size_large),
      axis.ticks = element_line(colour = col.ax, size = axis_ticks_size_large),
      axis.ticks.length = unit(axis_ticks_length_large, "bigpts"),
      axis.line = element_line(colour = col.ax, size = axis_size_large),
      legend.position = "none",
      legend.text = element_text(size = text_size_large),
      legend.title = element_text(size = text_size_large),
      panel.background = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = text_size_large),
      panel.grid = element_blank(),
      plot.title = element_text(size = text_size_large,
                                hjust = 0.5, vjust = 18),
      plot.margin = unit(c(256, 32, 0, 32), "bigpts")
    ) +
    # special theme for graphical abstract
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent"), 
      legend.box.background = element_rect(fill = "transparent") 
    ),
  filename = here("Plots", "group_mean_doy_differences_abstract.png"), width = 20, height = 15,
  bg = "transparent"
)


#without raw data

ggsave(
  ggplot() +
    geom_hline(
      size = 1.5,
      yintercept = 0,
      lty = lty.sec, col = "gray31"
    ) +
    # geom_point(data = stat.int.time,
    #            aes(x = group, y = synchrony.slope, shape = decades),
    #            size = 2, alpha = 0.3, col = "gray40",
    #            position = position_jitterdodge(jitter.width = 0.3, dodge.width = 1)) +
    geom_errorbar(
      data = stat.int.meta,
      aes(
        x = group,
        ymin = ci.min,
        ymax = ci.max,
        col = group
      ),
      size = 2,
      position = position_dodge(width = 1)
    ) +
    geom_point(
      data = stat.int.meta,
      aes(x = group, y = mean.synchrony.slope, col = group),
      size = 10,
      position = position_dodge(width = 1)
    ) +
    geom_text(
      data = stat.int.meta,
      aes(
        x = group,
        # including the 0.x ensures we're at leat at x heightwise
        # (0.x because of the 10x scaling)
        y = ypos(stat.int.meta$ci.max, 0.25, frac = 0.6), 
        label = paste0("n = ", N.synchrony.slope)
      ),
      size = annot_text,
      col = "gray31"
    ) +
    #add labels for differing factor levels
    geom_text(
      data = pairdiff(stat.int.time, synchrony.slope ~ group, level_order = levels(stat.int.time$group)),
      aes(x = level,
          y = ypos(stat.int.meta$ci.max, 0.25, frac = 0.2), # see above
          label = letter),
      size = annot_text,
      col = "gray31"
    ) +
    # geom_text(data = pairdiff(stat.int.time, synchrony.slope ~ decades, Letters = letters),
    #           aes(x = group, y = stat.int.meta$ci.max + abs(stat.int.meta$ci.max) * 0.1,
    #               label = letter),
    #           size = 15, col = "gray31") +
    scale_color_manual(
      name = "Group",
      labels = col.int$group,
      values = col.int$colour
    ) +
    labs(
      title = paste("Shifts of asynchrony",
                    "of plants and insect pollinators",
                    sep = "\n"),
      x = NULL,
      y = paste("Asynchrony shift [days/decade]\n(\u00B1 95% CI)",
                "<- Greater synchrony",
                sep = "\n")) +
    scale_x_discrete(labels = recode_vec_int_long) +
    scale_y_continuous(labels = mult_10_format()) +
    theme(
      axis.title = element_text(size = 50),
      axis.text = element_text(size = 45),
      axis.ticks = element_line(colour = col.ax, size = 1.2),
      axis.ticks.length.x.bottom = unit(8, "bigpts"),
      axis.line = element_line(colour = col.ax, size = 1.2),
      legend.position = "none",
      legend.text = element_text(size = 40),
      legend.title = element_text(size = 40),
      panel.background = element_blank(),
      panel.spacing = unit(32, "bigpts"),
      strip.background = element_blank(),
      strip.text = element_text(size = 40),
      panel.grid = element_blank(),
      plot.margin = unit(c(256, 32, 0, 32), "bigpts"),
      plot.title = element_text(size = 60, hjust = 0.5, vjust = 18)
    ) +
    # special theme for graphical abstract
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent"), 
      legend.box.background = element_rect(fill = "transparent") 
    ),
  filename = here("Plots", "group_mean_doy_differences_nopts.png"), width = 20, height = 15,
  bg = "transparent"
)
