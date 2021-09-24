scale_preds <- FALSE
center_preds <- FALSE

dat.occ.lm <- dat.occ %>% 
  mutate(year = scale(year, center = center_preds, scale = scale_preds))

coef <- coef(lm(doy ~ year, dat.occ.lm))

ggplot() + 
  geom_point(data = dat.occ %>% 
               mutate(year = scale(year,
                                   center = FALSE,
                                   scale = FALSE)),
             aes(year,
                 doy)) + 
  geom_abline(aes(intercept = coef[1] - (coef[2] / sd(dat.occ$year)),
                  slope = coef[2] / sd(dat.occ$year)))
