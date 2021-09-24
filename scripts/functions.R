
# define function for checking the existence of a directory
# and if it doesn't create it along with its parents 
dir.check <- function(path) {
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
}

# function to check whether a file is older than another
#   returns TRUE if file1 is older than file2 and FALSE otherwise
#   if any of the files dont exist, it returns FALSE
file.older.check <- function(file1, file2) {
  
  if (!(all(file.exists(file1, file2)))) {
    return(FALSE)
  }
  
  if (file.mtime(file1) < file.mtime(file2)) {
    return(TRUE)
  }
  
  return(FALSE)
}


# function for calculating a position above the range of a vector x
# muliple vectors can be supplied, again, the max result of all vectors
# is returned
ypos <- function(x, ..., frac = 0.3) {
  
  x <- c(x, ...)
  
  pos <- max(x, na.rm = TRUE) + abs(max(x, na.rm = TRUE)) * frac
  
  return(pos)
  
} 


# function for printing a log message starting with date and time
log_msg <- function(...) {
  msg <- paste0(toString(format(Sys.time())), " ", ...)
  cat(msg, "\n")
  
  # optionally log also to file, if that is given
  if (!is.null(getOption("log_file"))) {
    log_file <- getOption("log_file")
    cat(msg, fill = TRUE, file = log_file, append = TRUE, sep = "")
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


# Lmm functions -----------------------------------------------------------


# function to plot precomputed residuals and fitted values of a lm against
#   each other using ggplot. Formalized as a function mainly for consistency
lmResFitPlot <- function(mod_resid, mod_fit, col_vec = NULL, 
                         main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                         facet = FALSE) {
  
  if (is.null(col_vec)) {
    plot <- ggplot(data.frame(fit = mod_fit,
                              resid = mod_resid),
                   aes(fit, resid))
  } else {
    plot <- ggplot(data.frame(fit = mod_fit,
                              resid = mod_resid,
                              cols = col_vec),
                   aes(fit, resid, col = cols))
  }
  
  plot <- plot + 
    geom_point() +
    labs(title = main, subtitle = sub, xlab = xlab, ylab = ylab) +
    geom_hline(yintercept = 0)
  
  
  if (!is.null(col_vec)) {
    # if we have a separated line for groups, also add an overall line
    plot <- plot +
      geom_smooth() +
      geom_smooth(aes(fit, resid),
                  group = "overall", col = "red")
  }
  
  if (facet && !is.null(col_vec)) {
    plot <- plot +
      geom_density2d(col = "gray31") +
      geom_smooth(col = "red") +
      facet_wrap( ~ cols)
  }
  
  plot <- plot +
    theme_minimal() +
    theme(panel.grid = element_blank())
  
  return(plot)
}
