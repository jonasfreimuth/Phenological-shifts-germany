##################################################################
#
#                     TO BE RUN AFTER ANALYSIS
#
##################################################################

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
#     !!! ALL ADDTIONS NEED TO BE !APPENDED! TO THE CSV !!!
#   OTHERWISE THE WORD LINKS BREAK (THEY ARE BASED ON POSITION)
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

options(stringsAsFactors = FALSE)

# set threshold below which p.values are abbreviated
thr.pval = 0.001

# initialise data.frame
res.figures <- data.frame(name = NA, value = NA, formattedValue = NA)

# define function to write name and value into the next row of the data frame
# also optionally transform the value and save it to extra column or alternatively provide
# formatted Value directly (one excludes the other)
append.df <- function(name, value, multiplier = NULL, formattedValue = NULL,  target = res.figures) {
  
  # throw error if both mulitplier and formattedValue are given
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

# Original numbers for methods --------------------------------------------

# first one is written into first row
res.figures[1, ] <- data.frame(name = "nBioflorPlants",
                               value = nrow(fread(here("static_data", "bioflor_traits.csv"))),
                               formattedValue = NA)


# count numbers of plants and insects in the pruned data
plapoll.count.pruned <- fread(here("data", "occurrence_full_meta.csv"))

res.figures <- append.df(name = "nPlantOccPruned",
                         value = plapoll.count.pruned$nPlants[1])
res.figures <- append.df(name = "nPollOccPruned",
                         value = plapoll.count.pruned$nInsects[1])
res.figures <- append.df(name = "nPlaPollOccPruned",
                         value = plapoll.count.pruned$nRow[1])

#  Thresholds for methods -------------------------------------------------

res.figures <- append.df(name = "thrDec",
                         value = thr.dec)
res.figures <- append.df(name = "thrDecRec",
                         value = thr.dec.rec)
res.figures <- append.df(name = "thrSpec",
                         value = thr.spec)
res.figures <- append.df(name = "thrPerc",
                         value = thr.perc,
                         multiplier = 100)
res.figures <- append.df(name = "cutoffDec",
                         value = cutoff.dec)
res.figures <- append.df(name = "cutoffYear",
                         value = cutoff.year)

# yearly and decadal numbers ----------------------------------------------

# count yearly numbers
plapoll.count.year <- count(dat.occ, kingdom)

res.figures <- append.df(name = "nPlantOccYearly",
                         value = plapoll.count.year$n[2])
res.figures <- append.df(name = "nPollOccYearly",
                         value = plapoll.count.year$n[1])
res.figures <- append.df(name = "nPlaPollOccYearly",
                         value = sum(plapoll.count.year$n))

# count PollDep records
res.figures <- append.df(name = "nPollDepYearly",
                         value = nrow(dat.occ.polldep))

# add mean and median
res.figures <- append.df(name = "meanRecYearly",
                         value = mean(dat.occ$n.rec))
res.figures <- append.df(name = "medianRecYearly",
                         value = median(dat.occ$n.rec))

# count decadal numbers
plapoll.count.dec <- count(dat.occ.dec, kingdom)

res.figures <- append.df(name = "nPlantOccDecadal",
                         value = plapoll.count.dec$n[2])
res.figures <- append.df(name = "nPollOccDecadal",
                         value = plapoll.count.dec$n[1])
res.figures <- append.df(name = "nPlaPollOccDecadal",
                         value = sum(plapoll.count.dec$n))

# add mean and median
res.figures <- append.df(name = "meanRecDecadal",
                         value = mean(dat.occ.dec$n.rec))
res.figures <- append.df(name = "medianRecDecadal",
                         value = median(dat.occ.dec$n.rec))


# Slope numbers -----------------------------------------------------------

# count yearly numbers, should all be the same for temp
plapoll.count.year.slopes <- count(stat.spec.time, kingdom,
                                   wt = n())

res.figures <- append.df(name = "nPlantSlopeYearly",
                         value = plapoll.count.year.slopes$n[2])
res.figures <- append.df(name = "nPollSlopeYearly",
                         value = plapoll.count.year.slopes$n[1])
res.figures <- append.df(name = "nPlaPollSlopeYearly",
                         value = sum(plapoll.count.year.slopes$n))



# Interaction numbers -----------------------------------------------------

res.figures <- append.df(name = "nInteractions",
                         value = nrow(dat.int.time))
res.figures <- append.df(name = "nPspecInt",
                         value = uniqueN(dat.int.time$plant))
res.figures <- append.df(name = "nPollspecInt",
                         value = uniqueN(dat.int.time$poll))



# T-Test Pla Poll diff ----------------------------------------------------

# for time
res.figures <- append.df(name = "TTestPlaPollTimeDF",
                         value = time.ttest[["parameter"]][["df"]])
res.figures <- append.df(name = "TTestPlaPollTimeStat",
                         value = time.ttest[["statistic"]][["t"]])
res.figures <- append.df(name = "TTestPlaPollTimePVal",
                         value = time.ttest[["p.value"]],
                         formattedValue = format.pval(time.ttest[["p.value"]],
                                                  eps = thr.pval))

# for temp
res.figures <- append.df(name = "TTestPlaPollTempDF",
                         value = temp.ttest[["parameter"]][["df"]])
res.figures <- append.df(name = "TTestPlaPollTempStat",
                         value = temp.ttest[["statistic"]][["t"]])
res.figures <- append.df(name = "TTestPlaPollTempPVal",
                         value = temp.ttest[["p.value"]],
                         formattedValue = format.pval(temp.ttest[["p.value"]],
                                                  eps = thr.pval))


# Group slopes ------------------------------------------------------------

# mean and SE for both groups for time
res.figures <- append.df(name = "slopePlaTime",
                         value = stat.spec.meta$mean.slope[2],
                         multiplier = 10)
res.figures <- append.df(name = "slopePlaTimeSE",
                         value = stat.spec.meta$se.slope[2],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopePlaTime",
                         value = stat.spec.meta$n.slope[2])
res.figures <- append.df(name = "slopePollTime",
                         value = stat.spec.meta$mean.slope[1],
                         multiplier = 10)
res.figures <- append.df(name = "slopePollTimeSE",
                         value = stat.spec.meta$se.slope[1],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopePollTime",
                         value = stat.spec.meta$n.slope[1])

# mean and SE for both groups for temp
res.figures <- append.df(name = "slopePlaTemp",
                         value = stat.spec.meta$mean.slope[4])
res.figures <- append.df(name = "slopePlaTempSE",
                         value = stat.spec.meta$se.slope[4])
res.figures <- append.df(name = "nSlopePlaTemp",
                         value = stat.spec.meta$n.slope[4])
res.figures <- append.df(name = "slopePollTemp",
                         value = stat.spec.meta$mean.slope[3])
res.figures <- append.df(name = "slopePollTempSE",
                         value = stat.spec.meta$se.slope[3])
res.figures <- append.df(name = "nSlopePollTemp",
                         value = stat.spec.meta$n.slope[3])

# mean and SE for pollorders for time
res.figures <- append.df(name = "slopeColTime",
                         value = stat.group.meta$mean.slope[1],
                         multiplier = 10)
res.figures <- append.df(name = "slopeColTimeSE",
                         value = stat.group.meta$se.slope[1],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopeColTime",
                         value = stat.group.meta$n.slope[1])

res.figures <- append.df(name = "slopeDipTime",
                         value = stat.group.meta$mean.slope[2],
                         multiplier = 10)
res.figures <- append.df(name = "slopeDipTimeSE",
                         value = stat.group.meta$se.slope[2],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopeDipTime",
                         value = stat.group.meta$n.slope[2])

res.figures <- append.df(name = "slopeHymTime",
                         value = stat.group.meta$mean.slope[3],
                         multiplier = 10)
res.figures <- append.df(name = "slopeHymTimeSE",
                         value = stat.group.meta$se.slope[3],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopeHymTime",
                         value = stat.group.meta$n.slope[3])

res.figures <- append.df(name = "slopeLepTime",
                         value = stat.group.meta$mean.slope[4],
                         multiplier = 10)
res.figures <- append.df(name = "slopeLepTimeSE",
                         value = stat.group.meta$se.slope[4],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopeLepTime",
                         value = stat.group.meta$n.slope[4])

# mean and SE for pollorders for temp
res.figures <- append.df(name = "slopeColTemp",
                         value = stat.group.meta$mean.slope[6])
res.figures <- append.df(name = "slopeColTempSE",
                         value = stat.group.meta$se.slope[6])
res.figures <- append.df(name = "nSlopeColTemp",
                         value = stat.group.meta$n.slope[6])

res.figures <- append.df(name = "slopeDipTemp",
                         value = stat.group.meta$mean.slope[7],
                         multiplier = 10)
res.figures <- append.df(name = "slopeDipTempSE",
                         value = stat.group.meta$se.slope[7],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopeDipTemp",
                         value = stat.group.meta$n.slope[7])

res.figures <- append.df(name = "slopeHymTemp",
                         value = stat.group.meta$mean.slope[8],
                         multiplier = 10)
res.figures <- append.df(name = "slopeHymTempSE",
                         value = stat.group.meta$se.slope[8],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopeHymTemp",
                         value = stat.group.meta$n.slope[8])

res.figures <- append.df(name = "slopeLepTemp",
                         value = stat.group.meta$mean.slope[9],
                         multiplier = 10)
res.figures <- append.df(name = "slopeLepTempSE",
                         value = stat.group.meta$se.slope[9],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopeLepTemp",
                         value = stat.group.meta$n.slope[9])


#  Interaction figures ----------------------------------------------------

res.figures <- append.df(name = "NIntHov",
                         value = stat.int.meta$N.synchrony.slope[1])
res.figures <- append.df(name = "slopeIntHov",
                         value = stat.int.meta$mean.synchrony.slope[1],
                         multiplier = 10)
res.figures <- append.df(name = "slopeIntHovSE",
                         value = stat.int.meta$se.synchrony.slope[1],
                         multiplier = 10)
res.figures <- append.df(name = "nPlaIntHov",
                         value = stat.int.meta$n.plant[1])
res.figures <- append.df(name = "nPollIntHov",
                         value = stat.int.meta$n.poll[1])
res.figures <- append.df(name = "revPointIntHov",
                         # only take the number without decimals,
                         # in the document it's only supposed to
                         # denominate the year
                         value = floor(stat.int.meta$xintercept[1]))

res.figures <- append.df(name = "NIntBee",
                         value = stat.int.meta$N.synchrony.slope[2])
res.figures <- append.df(name = "slopeIntBee",
                         value = stat.int.meta$mean.synchrony.slope[2],
                         multiplier = 10)
res.figures <- append.df(name = "slopeIntBeeSE",
                         value = stat.int.meta$se.synchrony.slope[2],
                         multiplier = 10)
res.figures <- append.df(name = "nPlaIntBee",
                         value = stat.int.meta$n.plant[2])
res.figures <- append.df(name = "nPollIntBee",
                         value = stat.int.meta$n.poll[2])
res.figures <- append.df(name = "revPointIntBee",
                         value = floor(stat.int.meta$xintercept[2]))

res.figures <- append.df(name = "NIntBut",
                         value = stat.int.meta$N.synchrony.slope[3])
res.figures <- append.df(name = "slopeIntBut",
                         value = stat.int.meta$mean.synchrony.slope[3],
                         multiplier = 10)
res.figures <- append.df(name = "slopeIntButSE",
                         value = stat.int.meta$se.synchrony.slope[3],
                         multiplier = 10)
res.figures <- append.df(name = "nPlaIntBut",
                         value = stat.int.meta$n.plant[3])
res.figures <- append.df(name = "nPollIntBut",
                         value = stat.int.meta$n.poll[3])
res.figures <- append.df(name = "revPointIntBut",
                         value = floor(stat.int.meta$xintercept[3]))


# PollDep slopes ----------------------------------------------------------

# n mean SE for time
res.figures <- append.df(name = "slopePollDepYesTime",
                         value = stat.spec.time.PollDep.meta$mean.slope[1],
                         multiplier = 10)
res.figures <- append.df(name = "slopePollDepYesTimeSE",
                         value = stat.spec.time.PollDep.meta$se.slope[1],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopePollDepYesTime",
                         value = stat.spec.time.PollDep.meta$n.slope[1])

res.figures <- append.df(name = "slopePollDepNoTime",
                         value = stat.spec.time.PollDep.meta$mean.slope[2],
                         multiplier = 10)
res.figures <- append.df(name = "slopePollDepNoTimeSE",
                         value = stat.spec.time.PollDep.meta$se.slope[2],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopePollDepNoTime",
                         value = stat.spec.time.PollDep.meta$n.slope[2])

res.figures <- append.df(name = "slopePollDepIMTime",
                         value = stat.spec.time.PollDep.meta$mean.slope[3],
                         multiplier = 10)
res.figures <- append.df(name = "slopePollDepIMTimeSE",
                         value = stat.spec.time.PollDep.meta$se.slope[3],
                         multiplier = 10)
res.figures <- append.df(name = "nSlopePollDepIMTime",
                         value = stat.spec.time.PollDep.meta$n.slope[3])

# PollDep mean DOYs

res.figures <- append.df(name = "nMeanDoyPollDepYes",
                         value = stat.polldep.mdoy.meta$n.mean.doy[1])
res.figures <- append.df(name = "meanDoyPollDepYes",
                         value = stat.polldep.mdoy.meta$mean.mean.doy[1])
res.figures <- append.df(name = "meanDoyPollDepYesSE",
                         value = stat.polldep.mdoy.meta$se.mean.doy[1])

res.figures <- append.df(name = "nMeanDoyPollDepNo",
                         value = stat.polldep.mdoy.meta$n.mean.doy[2])
res.figures <- append.df(name = "meanDoyPollDepNo",
                         value = stat.polldep.mdoy.meta$mean.mean.doy[2])
res.figures <- append.df(name = "meanDoyPollDepNoSE",
                         value = stat.polldep.mdoy.meta$se.mean.doy[2])

res.figures <- append.df(name = "nMeanDoyPollDepIM",
                         value = stat.polldep.mdoy.meta$n.mean.doy[3])
res.figures <- append.df(name = "meanDoyPollDepIM",
                         value = stat.polldep.mdoy.meta$mean.mean.doy[3])
res.figures <- append.df(name = "meanDoyPollDepIMSE",
                         value = stat.polldep.mdoy.meta$se.mean.doy[3])

# n mean SE for temp
res.figures <- append.df(name = "slopePollDepYesTemp",
                         value = stat.spec.temp.PollDep.meta$mean.slope[1])
res.figures <- append.df(name = "slopePollDepYesTempSE",
                         value = stat.spec.temp.PollDep.meta$se.slope[1])
res.figures <- append.df(name = "nSlopePollDepYesTemp",
                         value = stat.spec.temp.PollDep.meta$n.slope[1])

res.figures <- append.df(name = "slopePollDepNoTemp",
                         value = stat.spec.temp.PollDep.meta$mean.slope[2])
res.figures <- append.df(name = "slopePollDepNoTempSE",
                         value = stat.spec.temp.PollDep.meta$se.slope[2])
res.figures <- append.df(name = "nSlopePollDepNoTemp",
                         value = stat.spec.temp.PollDep.meta$n.slope[2])

res.figures <- append.df(name = "slopePollDepIMTemp",
                         value = stat.spec.temp.PollDep.meta$mean.slope[3])
res.figures <- append.df(name = "slopePollDepIMTempSE",
                         value = stat.spec.temp.PollDep.meta$se.slope[3])
res.figures <- append.df(name = "nSlopePollDepIMTemp",
                         value = stat.spec.temp.PollDep.meta$n.slope[3])



# Time and Temp slopes ANOVA and post-hoc ---------------------------------

# for time
res.figures <- append.df(name = "timeSlopeANOVADFs",
                         value = paste(summary(mod.time)[[1]][["Df"]],
                                       collapse = ", "))
res.figures <- append.df(name = "timeSlopesANOVAStat",
                         value = summary(mod.time)[[1]][["F value"]][1])
res.figures <- append.df(name = "timeSlopesANOVAPVal",
                         value = summary(mod.time)[[1]][["Pr(>F)"]][1],
                         formattedValue = format.pval(summary(mod.time)[[1]][["Pr(>F)"]][1],
                                                  eps = thr.pval))

# save tukey output as df
mod.time.tukey <- data.frame(TukeyHSD(mod.time)[[1]],
                                pairs = attr(as.data.frame(TukeyHSD(mod.time)[[1]]),
                                             "row.names"))

res.figures <- append.df(name = str_glue("timeSlopesPD{abbreviate(mod.time.tukey$pairs[1])}"),
                         value = mod.time.tukey$p.adj[1])
res.figures <- append.df(name = str_glue("timeSlopesPD{abbreviate(mod.time.tukey$pairs[2])}"),
                         value = mod.time.tukey$p.adj[2])
res.figures <- append.df(name = str_glue("timeSlopesPD{abbreviate(mod.time.tukey$pairs[3])}"),
                         value = mod.time.tukey$p.adj[3])
res.figures <- append.df(name = "*placeholder*",
                         value = NA)
res.figures <- append.df(name = "*placeholder*",
                         value = NA)
res.figures <- append.df(name = "*placeholder*",
                         value = NA)


#for temp
res.figures <- append.df(name = "tempSlopeANOVADFs",
                         value = paste(summary(mod.temp)[[1]][["Df"]],
                                       collapse = ", "))
res.figures <- append.df(name = "tempSlopesANOVAStat",
                         value = summary(mod.temp)[[1]][["F value"]][1])
res.figures <- append.df(name = "tempSlopesANOVAPVal",
                         value = summary(mod.temp)[[1]][["Pr(>F)"]][1],
                         formattedValue = format.pval(summary(mod.temp)[[1]][["Pr(>F)"]][1],
                                                   eps = thr.pval))

# save tukey output as df
mod.temp.tukey <- data.frame(TukeyHSD(mod.temp)[[1]],
                             pairs = attr(as.data.frame(TukeyHSD(mod.temp)[[1]]),
                                          "row.names"))

res.figures <- append.df(name = str_glue("tempSlopesPD{abbreviate(mod.temp.tukey$pairs[1])}"),
                         value = mod.temp.tukey$p.adj[1])
res.figures <- append.df(name = str_glue("tempSlopesPD{abbreviate(mod.temp.tukey$pairs[2])}"),
                         value = mod.temp.tukey$p.adj[2])
res.figures <- append.df(name = str_glue("tempSlopesPD{abbreviate(mod.temp.tukey$pairs[3])}"),
                         value = mod.temp.tukey$p.adj[3])
res.figures <- append.df(name = "*placeholder*",
                         value = NA)
res.figures <- append.df(name = "*placeholder*",
                         value = NA)
res.figures <- append.df(name = "*placeholder*",
                         value = NA)


# Additional figures ------------------------------------------------------

# load counts of institution occurrences and calculate percentages
ins.perc <- fread(here("data", "pruned_occ_institution_count.csv")) %>%
  mutate(percOfRecords = (n / sum(n))* 100)

res.figures <- append.df(name = "percRecNaturgucker",
                         value = ins.perc$percOfRecords[ins.perc$institutionCode == "naturgucker"])

res.figures <- append.df(name = "nSpecPollDep",
                         value = uniqueN(dat.occ.polldep$species))



# Interactions ANOVA and Tukey --------------------------------------------

# for time
res.figures <- append.df(name = "intANOVADFs",
                         value = paste(summary(mod.int.all)[[1]][["Df"]],
                                       collapse = ", "))
res.figures <- append.df(name = "intANOVAStat",
                         value = summary(mod.int.all)[[1]][["F value"]][1])
res.figures <- append.df(name = "intANOVAPVal",
                         value = summary(mod.int.all)[[1]][["Pr(>F)"]][1],
                         formattedValue = format.pval(summary(mod.int.all)[[1]][["Pr(>F)"]][1],
                                                   eps = thr.pval))

# save tukey output as df
mod.int.all.tukey <- data.frame(TukeyHSD(mod.int.all)[[1]],
                             pairs = attr(as.data.frame(TukeyHSD(mod.int.all)[[1]]),
                                          "row.names"))

res.figures <- append.df(name = "intPDBee-Hov",
                         value = mod.int.all.tukey$p.adj[1])
res.figures <- append.df(name = "intPDBut-Hov",
                         value = mod.int.all.tukey$p.adj[2])
res.figures <- append.df(name = "intPDBut-Bee",
                         value = mod.int.all.tukey$p.adj[3])



# Percentages of advancing species ----------------------------------------

#for time
res.figures <- append.df(name = "advfracPlantTime",
                         value = stat.spec.meta$adv.frac[2],
                         multiplier = 100)
res.figures <- append.df(name = "advfracPollTime",
                         value = stat.spec.meta$adv.frac[1],
                         multiplier = 100)

res.figures <- append.df(name = "advfracColTime",
                         value = stat.group.meta$adv.frac[1],
                         multiplier = 100)
res.figures <- append.df(name = "advfracDipTime",
                         value = stat.group.meta$adv.frac[2],
                         multiplier = 100)
res.figures <- append.df(name = "advfracHymTime",
                         value = stat.group.meta$adv.frac[3],
                         multiplier = 100)
res.figures <- append.df(name = "advfracLepTime",
                         value = stat.group.meta$adv.frac[4],
                         multiplier = 100)


#for temp
res.figures <- append.df(name = "advfracPlantTemp",
                         value = stat.spec.meta$adv.frac[4],
                         multiplier = 100)
res.figures <- append.df(name = "advfracPollTemp",
                         value = stat.spec.meta$adv.frac[3],
                         multiplier = 100)

res.figures <- append.df(name = "advfracColTemp",
                         value = stat.group.meta$adv.frac[6],
                         multiplier = 100)
res.figures <- append.df(name = "advfracDipTemp",
                         value = stat.group.meta$adv.frac[7],
                         multiplier = 100)
res.figures <- append.df(name = "advfracHymTemp",
                         value = stat.group.meta$adv.frac[8],
                         multiplier = 100)
res.figures <- append.df(name = "advfracLepTemp",
                         value = stat.group.meta$adv.frac[9],
                         multiplier = 100)

# Interactions doy diff
res.figures <- append.df(name = "negfracHov",
                         value = stat.int.meta$negfrac[1],
                         multiplier = 100)
res.figures <- append.df(name = "negfracBee",
                         value = stat.int.meta$negfrac[2],
                         multiplier = 100)
res.figures <- append.df(name = "negfracBut",
                         value = stat.int.meta$negfrac[3],
                         multiplier = 100)

# PollDep
res.figures <- append.df(name = "advfracPDYesTime",
                         value = stat.PollDep.meta$adv.frac[1],
                         multiplier = 100)
res.figures <- append.df(name = "advfracPDNoTime",
                         value = stat.PollDep.meta$adv.frac[2],
                         multiplier = 100)
res.figures <- append.df(name = "advfracPDIMTime",
                         value = stat.PollDep.meta$adv.frac[3],
                         multiplier = 100)

res.figures <- append.df(name = "advfracPDYesTemp",
                         value = stat.PollDep.meta$adv.frac[4],
                         multiplier = 100)
res.figures <- append.df(name = "advfracPDNoTemp",
                         value = stat.PollDep.meta$adv.frac[5],
                         multiplier = 100)
res.figures <- append.df(name = "advfracPDIMTemp",
                         value = stat.PollDep.meta$adv.frac[6],
                         multiplier = 100)


# Interaction fractions ---------------------------------------------------

# slopes of fraction of insect earlier
res.figures <- append.df(name = "earlfracSlopeHov",
                         value = eqs.ins.earl$slope[1],
                         multiplier = 10)
res.figures <- append.df(name = "earlfracSlopeBee",
                         value = eqs.ins.earl$slope[2],
                         multiplier = 10)
res.figures <- append.df(name = "earlfracSlopeBut",
                         value = eqs.ins.earl$slope[3],
                         multiplier = 10)
res.figures <- append.df(name = "earlfracSlopeAll",
                         value = eqs.ins.earl$slope[4],
                         multiplier = 10)


# First, last, duration shifts --------------------------------------------

# mean assymetry slopes [slopes should already be in days/decade]
res.figures <- append.df(name = "slopePlaDiff",
                         value = stat.spec.dur.meta$mean.slope.diff[5])
res.figures <- append.df(name = "slopePlaDiffSe",
                         value = stat.spec.dur.meta$se.slope.diff[5])
res.figures <- append.df(name = "nSlopePlaDiff",
                         value = stat.spec.dur.meta$n.slope.diff[5])

res.figures <- append.df(name = "slopeColDiff",
                         value = stat.spec.dur.meta$mean.slope.diff[1])
res.figures <- append.df(name = "slopeColDiffSe",
                         value = stat.spec.dur.meta$se.slope.diff[1])
res.figures <- append.df(name = "nSlopeColDiff",
                         value = stat.spec.dur.meta$n.slope.diff[1])

res.figures <- append.df(name = "slopeDipDiff",
                         value = stat.spec.dur.meta$mean.slope.diff[2])
res.figures <- append.df(name = "slopeDipDiffSe",
                         value = stat.spec.dur.meta$se.slope.diff[2])
res.figures <- append.df(name = "nSlopeDipDiff",
                         value = stat.spec.dur.meta$n.slope.diff[2])

res.figures <- append.df(name = "slopeHymDiff",
                         value = stat.spec.dur.meta$mean.slope.diff[3])
res.figures <- append.df(name = "slopeHymDiffSe",
                         value = stat.spec.dur.meta$se.slope.diff[3])
res.figures <- append.df(name = "nSlopeHymDiff",
                         value = stat.spec.dur.meta$n.slope.diff[3])

res.figures <- append.df(name = "slopeLepDiff",
                         value = stat.spec.dur.meta$mean.slope.diff[4])
res.figures <- append.df(name = "slopeLepDiffSe",
                         value = stat.spec.dur.meta$se.slope.diff[4])
res.figures <- append.df(name = "nSlopeLepDiff",
                         value = stat.spec.dur.meta$n.slope.diff[4])

# duration slopes
res.figures <- append.df(name = "slopePlaDur",
                         value = stat.spec.dur.meta$mean.slope[5])
res.figures <- append.df(name = "slopePlaDurSe",
                         value = stat.spec.dur.meta$se.slope[5])
res.figures <- append.df(name = "nSlopePlaDur",
                         value = stat.spec.dur.meta$n.slope[5])

res.figures <- append.df(name = "slopeColDur",
                         value = stat.spec.dur.meta$mean.slope[1])
res.figures <- append.df(name = "slopeColDurSe",
                         value = stat.spec.dur.meta$se.slope[1])
res.figures <- append.df(name = "nSlopeColDur",
                         value = stat.spec.dur.meta$n.slope[1])

res.figures <- append.df(name = "slopeDipDur",
                         value = stat.spec.dur.meta$mean.slope[2])
res.figures <- append.df(name = "slopeDipDurSe",
                         value = stat.spec.dur.meta$se.slope[2])
res.figures <- append.df(name = "nSlopeDipDur",
                         value = stat.spec.dur.meta$n.slope[2])

res.figures <- append.df(name = "slopeHymDur",
                         value = stat.spec.dur.meta$mean.slope[3])
res.figures <- append.df(name = "slopeHymDurSe",
                         value = stat.spec.dur.meta$se.slope[3])
res.figures <- append.df(name = "nSlopeHymDur",
                         value = stat.spec.dur.meta$n.slope[3])

res.figures <- append.df(name = "slopeLepDur",
                         value = stat.spec.dur.meta$mean.slope[4])
res.figures <- append.df(name = "slopeLepDurSe",
                         value = stat.spec.dur.meta$se.slope[4])
res.figures <- append.df(name = "nSlopeLepDur",
                         value = stat.spec.dur.meta$n.slope[4])

# first occurrence slopes
res.figures <- append.df(name = "slopePlaFirst",
                         value = stat.spec.dur.meta$mean.slope.first[5])
res.figures <- append.df(name = "slopePlaFirstSe",
                         value = stat.spec.dur.meta$se.slope.first[5])
res.figures <- append.df(name = "nSlopePlaFirst",
                         value = stat.spec.dur.meta$n.slope.first[5])

res.figures <- append.df(name = "slopeColFirst",
                         value = stat.spec.dur.meta$mean.slope.first[1])
res.figures <- append.df(name = "slopeColFirstSe",
                         value = stat.spec.dur.meta$se.slope.first[1])
res.figures <- append.df(name = "nSlopeColFirst",
                         value = stat.spec.dur.meta$n.slope.first[1])

res.figures <- append.df(name = "slopeDipFirst",
                         value = stat.spec.dur.meta$mean.slope.first[2])
res.figures <- append.df(name = "slopeDipFirstSe",
                         value = stat.spec.dur.meta$se.slope.first[2])
res.figures <- append.df(name = "nSlopeDipFirst",
                         value = stat.spec.dur.meta$n.slope.first[2])

res.figures <- append.df(name = "slopeHymFirst",
                         value = stat.spec.dur.meta$mean.slope.first[3])
res.figures <- append.df(name = "slopeHymFirstSe",
                         value = stat.spec.dur.meta$se.slope.first[3])
res.figures <- append.df(name = "nSlopeHymFirst",
                         value = stat.spec.dur.meta$n.slope.first[3])

res.figures <- append.df(name = "slopeLepFirst",
                         value = stat.spec.dur.meta$mean.slope.first[4])
res.figures <- append.df(name = "slopeLepFirstSe",
                         value = stat.spec.dur.meta$se.slope.first[4])
res.figures <- append.df(name = "nSlopeLepFirst",
                         value = stat.spec.dur.meta$n.slope.first[4])

# last occurrence slopes
res.figures <- append.df(name = "slopePlaLast",
                         value = stat.spec.dur.meta$mean.slope.last[5])
res.figures <- append.df(name = "slopePlaLastSe",
                         value = stat.spec.dur.meta$se.slope.last[5])
res.figures <- append.df(name = "nSlopePlaLast",
                         value = stat.spec.dur.meta$n.slope.last[5])

res.figures <- append.df(name = "slopeColLast",
                         value = stat.spec.dur.meta$mean.slope.last[1])
res.figures <- append.df(name = "slopeColLastSe",
                         value = stat.spec.dur.meta$se.slope.last[1])
res.figures <- append.df(name = "nSlopeColLast",
                         value = stat.spec.dur.meta$n.slope.last[1])

res.figures <- append.df(name = "slopeDipLast",
                         value = stat.spec.dur.meta$mean.slope.last[2])
res.figures <- append.df(name = "slopeDipLastSe",
                         value = stat.spec.dur.meta$se.slope.last[2])
res.figures <- append.df(name = "nSlopeDipLast",
                         value = stat.spec.dur.meta$n.slope.last[2])

res.figures <- append.df(name = "slopeHymLast",
                         value = stat.spec.dur.meta$mean.slope.last[3])
res.figures <- append.df(name = "slopeHymLastSe",
                         value = stat.spec.dur.meta$se.slope.last[3])
res.figures <- append.df(name = "nSlopeHymLast",
                         value = stat.spec.dur.meta$n.slope.last[3])

res.figures <- append.df(name = "slopeLepLast",
                         value = stat.spec.dur.meta$mean.slope.last[4])
res.figures <- append.df(name = "slopeLepLastSe",
                         value = stat.spec.dur.meta$se.slope.last[4])
res.figures <- append.df(name = "nSlopeLepLast",
                         value = stat.spec.dur.meta$n.slope.last[4])



# Save data to csv --------------------------------------------------------

fwrite(res.figures, here("data", "analysis_results.csv"))





