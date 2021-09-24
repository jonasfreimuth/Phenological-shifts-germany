library("redres")

dir.check("plots_temp")

types <- c("raw_cond", "raw_mar", "std_cond",
           "std_mar")

mod_fitvl <- fitted(lm_mod)

# for (type in types) {
  
  
  mod_resid <- lm_mod@resp$y - (lm_mod@pp$X %*% matrix(lm_mod@beta, 
                                                     ncol = 1))
  
  # ggsave(paste0("plots_temp/",
  #               "lmm_resid_",
  #               type, "_",
  #               "_fit",
  #               ".png"),
  #        ,
  #        width = 20, height = 12)
  
# }