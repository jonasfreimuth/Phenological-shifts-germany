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
  "Lepidoptera" = "Butterflies/\nMoths"
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
col.grp <- data.frame(group = c("Beetles", "Flies", "Bees", "Butterflies/\nMoths",
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

# just to be save, run functions script
source(here('scripts', 'functions.R'))

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
  filename = here("plots", "Germany temperature over time.png"), width = 22, height = 14,
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
  filename = here("plots", "group_record_numbers_time.png"),
  width = 20, height = 20
)



# Plot 1: group trends over time ------------------------------------------
# 
# png(here("plots", "group_trends_time.png"), width = 1600, height = 1000)
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
# png(here("plots", "group_trends_temp.png"), width = 1600, height = 1000)
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
  filename = here("plots", "group_trends_time_orders.png"), width = 20, height = 12.5
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
  filename = here("plots", "group_trends_temp_orders.png"), width = 20, height = 12.5
  
)

# # same again but controling for orders
# 
# png(here("plots", "group_trends_time_orders_cont.png"), width = 1600, height = 1000)
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
# png(here("plots", "group_trends_temp_orders_cont.png"), width = 1600, height = 1000)
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


png(here("plots", "Plant order trends over time.png"), width = 2500, height = 1750)

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


png(here("plots", "Plant order trends over temp.png"), width = 2500, height = 1750)

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


png(here("plots", "Pollinator order trends over time.png"), width = 1600, height = 1000)

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


png(here("plots", "Pollinator order trends over temp.png"), width = 1600, height = 1000)

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
    
    png(here("plots/species", paste("doy_year_", s, ".png")), width = 1700, height = 1000)
    
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


# plot for year
dur_mikado_plot(here("plots", "species_mikado_year.png"), width = 1500, height = 1000,
                # rename id.grp to trivial name and exclude groups with to few species
                data = dat.occ %>% 
                  mutate(id.grp = recode_factor(id.grp, !!! recode.vec)) %>% 
                  filter(!(id.grp %in% excl.trivial.year)),
                xlab = "Year", ylab = "Day of the Year")

# plot for decade
dur_mikado_plot(here("plots", "species_mikado_decade.png"), width = 1500, height = 1000,
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


png(here("plots", "group_shifts_occurrence_duration.png"), width = 1600, height = 1000)

print(
  dur_slope_plot_save(xlab = "Group",
                      ylab = paste("Mean slope of dedacal activity duration",
                                   "[days/decade] (\u00B1 95% CI) [days/decade] (\u00B1 95% CI)",
                                   sep = "\n"))
)

dev.off()



png(here("plots", "group_shifts_first_occurrence.png"), width = 1600, height = 1000)

print(
  dur_slope_plot_save(y = slope.first,
                      ymeta = mean.slope.first,
                      ymin = ci.min.first,
                      ymax = ci.max.first,
                      xlab = "Group", ylab = "Mean slope [days/decade] (\u00B1 95% CI)")
)

dev.off()


png(here("plots", "group_shifts_last_occurrence.png"), width = 1600, height = 1000)

print(
  dur_slope_plot_save(y = slope.last,
                      ymeta = mean.slope.last,
                      ymin = ci.min.last,
                      ymax = ci.max.last,
                      xlab = "Group", ylab = "Mean slope [days/decade] (\u00B1 95% CI)")
)

dev.off()



ggsave(
  filename = here("plots", "group_shifts_diff_occurrence.png"),
  plot = dur_slope_plot_save(
    y = slope.diff,
    ymeta = mean.slope.diff,
    ymin = ci.min.diff,
    ymax = ci.max.diff,
    xlab = "Group",
    ylab = paste(
      "Asymmetry of shifts first vs last DOY",
      "[days/decade] (\u00B1 95% CI)",
      "<-- First stronger | Last stronger -->",
      sep = "\n"
    ),
    ylim = c(-5.5, 4.5)
  ),
  width = 20,
  height = 12
)



# Dirty Forest Plot Plants ------------------------------------------------

png(here("plots", "forest_plot_plants_time.png"), width = 1000, height = 1700)


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


png(here("plots", "forest_plot_plants_temp.png"), width = 1000, height = 1700)

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


png(here("plots", "forest_plot_pollinators_time.png"), width = 1000, height = 1700)

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


png(here("plots", "forest_plot_pollinators_temp.png"), width = 1000, height = 1700)

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


png(here("plots", "Boxplot Slope distribution time.png"), width = 1600, height = 1000)

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

png(here("plots", "Boxplot Slope distribution temp.png"), width = 1600, height = 1000)

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


png(here("plots", "Barplot group slope numbers.png"), width = 1600, height = 1000)

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


png(here("plots", "Mean_CI_time.png"), width = 1000, height = 1000)

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


png(here("plots", "Mean_CI_temp.png"), width = 1000, height = 1000)

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


png(here("plots", "Mean_CI_time_nopts.png"), width = 1000, height = 1000)

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


png(here("plots", "Mean_CI_temp_nopts.png"), width = 1000, height = 1000)

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


ggsave(
  slope_plot_save(xlab = "Group", ylab = "Mean shift [days/decade] (\u00B1 95% CI)",
                  title = "Phenological shifts over time",
                  data_points = TRUE, scale_y_10 = TRUE),
  filename = here("plots", "group_mean_slopes_time.png"), width = 20, height = 15,
  bg = "transparent"
)


ggsave(
  slope_plot_save(data = stat.all.temp, metadata = all.temp.meta,
                  xlab = "Group", ylab = "Mean shift [days/\u00B0C] (\u00B1 95% CI)",
                  data_points = TRUE, scale_y_10 = FALSE),
  filename = here("plots", "group_mean_slopes_temp.png"), width = 20, height = 15,
  bg = "transparent"
)

#without raw data +++++++++++++++++++++++++++++

ggsave(
  slope_plot_save(xlab = "Group", ylab = "Mean shift [days/decade] (\u00B1 95% CI)",
                  title = "Phenological shifts over time",
                  data_points = FALSE, scale_y_10 = TRUE),
  filename = here("plots", "group_mean_slopes_time_nopts.png"), width = 20, height = 15,
  bg = "transparent"
)

ggsave(
  slope_plot_save(data = stat.all.temp, metadata = all.temp.meta,
                  xlab = "Group", ylab = "Mean shift [days/\u00B0C] (\u00B1 95% CI)",
                  data_points = FALSE, scale_y_10 = FALSE),
  filename = here("plots", "group_mean_slopes_temp_nopts.png"), width = 20, height = 15,
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

slopes_correlation <- ggplot() +
  # zero axis (data arguments necessary, else it's acting up when trying to apply facets)
  geom_segment(
    aes(
      x = 0,
      xend = 0,
      y = -Inf,
      yend = ypos(
        stat.spec.both.plot$temp.slope,
        stat.spec.both.plot$temp.ci.max,
        frac = 0
      )
    ),
    data = stat.spec.both.plot,
    size = 2,
    col = "gray31",
    lty = 3
  ) +
  geom_hline(
    yintercept = 0,
    lty = 3,
    col = col.line,
    size = 2,
    data = stat.spec.both.plot
  ) +
  # raw data
  geom_errorbar(
    aes(x = time.slope,
        ymin = temp.ci.min,
        ymax = temp.ci.max),
    data = stat.spec.both.plot,
    size = 2,
    alpha = 0.5,
    col = col.pt
  ) +
  geom_errorbarh(
    aes(y = temp.slope,
        xmin = time.ci.min,
        xmax = time.ci.max),
    data = stat.spec.both.plot,
    size = 2,
    alpha = 0.5,
    col = col.pt
  ) +
  geom_point(
    aes(time.slope, temp.slope, group = id.grp),
    data = stat.spec.both.plot,
    size = 6,
    alpha = 0.5,
    col = col.pt
  ) +
  # lines for means down to axis for time
  geom_segment(
    aes(
      x = mean.slope.Year,
      xend = mean.slope.Year,
      y = -Inf,
      yend = mean.slope.Temperature,
      col = id.grp
    ),
    data = spec.both.meta.plot,
    size = 2,
    # alpha = 0.5
  ) +
  # lines for means down to axis for temp
  geom_segment(
    aes(
      x = -Inf,
      xend = mean.slope.Year,
      y = mean.slope.Temperature,
      yend = mean.slope.Temperature,
      col = id.grp
    ),
    data = spec.both.meta.plot,
    size = 2,
    # alpha = 0.5
  ) +
  # group means
  geom_errorbar(
    aes(
      x = mean.slope.Year,
      ymin = ci.min.Temperature,
      ymax = ci.max.Temperature,
      col = id.grp
    ),
    data = spec.both.meta.plot,
    size = 3
  ) +
  geom_errorbarh(
    aes(
      y = mean.slope.Temperature,
      xmin = ci.min.Year,
      xmax = ci.max.Year,
      col = id.grp
    ),
    data = spec.both.meta.plot,
    size = 3
  ) +
  geom_point(
    aes(mean.slope.Year, mean.slope.Temperature, col = id.grp),
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
  facet_wrap(~ id.grp) +
  labs(x = "Temporal trends [days / decade]",
       y = "Climate sensitivity [days / \u00B0C]") +
  scale_x_continuous(labels = mult_10_format()) +
  scale_color_manual(name = "Group",
                     values = col.grp.vec) +
  ylim(min(stat.spec.both.plot$temp.ci.min),
       max(stat.spec.both.plot$temp.ci.max) * 1.5) +
  theme(
    axis.title = element_text(size = 50),
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
  ) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent")
  )

slopes_correlation_pic <- ggdraw() +
  draw_plot(slopes_correlation) +
  draw_image(image = here('assets', 'beetle.png'),
             x = 0.22, y = 0.90,
             scale = 0.2, hjust = 0.5, vjust = 0.5) +
  draw_image(image = here('assets', 'butterfly.png'),
             x = 0.54, y = 0.90,
             scale = 0.2, hjust = 0.5, vjust = 0.5) +
  draw_image(image = here('assets', 'plant.png'),
             x = 0.86, y = 0.90,
             scale = 0.2, hjust = 0.5, vjust = 0.5)

save_plot(here('plots', 'slopes_correlation.png'),
          plot = slopes_correlation_pic,
          base_width = 20,
          base_height = 12.5,
          bg = "transparent")


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
  filename = here("plots", "PollDep overall trends over time.png"), width = 20, height = 12.5,
  bg = "transparent"
)


png(here("plots", "PollDep overall trends over temp.png"), width = 1600, height = 1000)

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
# png(here("plots", "PollDep species trends over time.png"), width = 1600, height = 1000)
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
# png(here("plots", "PollDep species trends over temp.png"), width = 1600, height = 1000)
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

png(here("plots", "Forest_PollDep_time.png"), width = 900, height = 1250)

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

png(here("plots", "Forest_PollDep_temp.png"), width = 900, height = 1250)

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

png(here("plots", "Mean_CI_PollDep_mean_doy.png"), width = 1000, height = 1000)

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

png(here("plots", "Mean_CI_PollDep_time.png"), width = 1000, height = 1000)

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

png(here("plots", "Mean_CI_PollDep_temp.png"), width = 1000, height = 1000)

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


png(here("plots", "Mean_CI_PollDep_time_nopts.png"), width = 1000, height = 1000)

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


png(here("plots", "Mean_CI_PollDep_temp_nopts.png"), width = 1000, height = 1000)

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
png(here("plots/traits",
         "Mean_CI_mnflmnt_time.png"), width = 1600, height = 1000)

plots_mnflmnt[[1]] +
  xlab("Peak month of flowering")

dev.off()

# Interactions: Indidividual slopes ---------------------------------------



png(here("plots", "group_decadal_slopes.png"), width = 1600, height = 1000)

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


png(here("plots", "mean_slope_differences_time.png"), width = 1000, height = 1000)

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


png(here("plots", "mean_slope_differences_temp.png"), width = 1000, height = 1000)

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



png(here("plots", "mean_slope_differences_time_nopts.png"), width = 1000, height = 1000)

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


png(here("plots", "mean_slope_differences_temp_nopts.png"), width = 1000, height = 1000)

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


# png(here("plots", "mean_doy_differences_overspec.png"), width = 1600, height = 1000)
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


mean_doy_differences_overall_facet <- ggplot(int.spec.dec.plot,
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
  ) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"), 
    legend.box.background = element_rect(fill = "transparent"))

ggsave(
  mean_doy_differences_overall_facet,
  filename = here("plots", "mean_doy_differences_overall_facet.png"), height = 20, width = 16
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

frac_ins_earlier_time <- ggplot(filter(int.mean.time.plot, group != "Overall"), 
       aes(decade, ins.early.frac, col = group)) + 
  geom_hline(yintercept = 0.5, linetype = 3,
             size = hline_size_large, col = "gray31") +
  geom_point(size = 8,
             pch = 18, 
             show.legend = FALSE) +
  # geom_smooth(method = "lm", size = 2) +
  geom_abline(aes(intercept = intercept, slope = slope, lty = breaks, col = group),
              lm.res.ins.earl,
              # col = "gray31",
              size = line_size_large
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
  ) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"), 
    legend.box.background = element_rect(fill = "transparent"))

# percentages of insect partner earlier (excluding overall data)
ggsave(
  frac_ins_earlier_time,
  filename = here("plots", "frac_ins_earlier_time.png"), height = 20, width = 16
)

# 
# png(here("plots", "mean_doy_differences_overall_decade_means.png"), height = 1000, width = 1600)
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
# png(here("plots", "mean_doy_differences_overall_all_in_one.png"), width = 1600, height = 1000)
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
  
  png(here("plots/interactions/doy_diff", paste("mean_doy_differences", s, ".png")), width = 1600, height = 1000)
  
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

group_mean_doy_differences <- ggplot() +
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
    size = 5,
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
    size = 3,
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
    plot.margin = unit(c(200, 32, 0, 32), "bigpts")
  ) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"), 
    legend.box.background = element_rect(fill = "transparent"))

ggsave(
  group_mean_doy_differences,
  filename = here("plots", "group_mean_doy_differences.png"), width = 20, height = 12.5,
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
  filename = here("plots", "group_mean_doy_differences_abstract.png"), width = 20, height = 15,
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
        # including the 0.x ensures we're at least at x heightwise
        # (0.x because of the 10x scaling)
        y = ypos(stat.int.meta$ci.max, 0.25, frac = 0.8), 
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
  filename = here("plots", "group_mean_doy_differences_nopts.png"), width = 20, height = 15,
  bg = "transparent"
)


# All interaction plots ---------------------------------------------------

# I'm now using yet another way to construct plots, namely cowplot to 
# build multi panel plots. for this method it would be better to have 
# plots with default point and text sizes, but that's what were stuck with
# now, hence the weird label_size and width and height of the plot
label_size <- 50

mean_frac <- plot_grid(mean_doy_differences_overall_facet, frac_ins_earlier_time,
                       nrow = 1, labels = c('A', 'B'),
                       label_size = label_size)
group <- plot_grid(group_mean_doy_differences,
                   labels = c('C'),
                   label_size = label_size)
mean_frac_group <- plot_grid(mean_frac, group,
                             ncol = 1)

mean_frac_group_pic <- ggdraw() +
  draw_plot(mean_frac_group) +
  draw_image(image = here('assets', 'Hoverfly_and_Plant.png'),
             x = 0.265, y = 0.45,
             scale = 0.2, hjust = 0.5, vjust = 0.5) +
  draw_image(image = here('assets', 'Bee_and_Plant.png'),
             x = 0.542, y = 0.45,
             scale = 0.2, hjust = 0.5, vjust = 0.5) +
  draw_image(image = here('assets', 'Butterfly_and_Plant.png'),
             x = 0.820, y = 0.45,
             scale = 0.2, hjust = 0.5, vjust = 0.5)

save_plot(here('plots', 'interactions_all.png'), mean_frac_group_pic, 
          base_height = 32.5, base_width = 32,
          bg = "transparent")

